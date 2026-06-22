#' Ingest amplicon ASV count data into a gopheR den
#'
#' Reads a post-DADA2 count table (local ASV labels x samples), deduplicates
#' sequences into the global `asv` table via MD5 hashing, records the per-batch
#' label -> global ID mapping in `amplicon_asv`, and registers the count table
#' file in `workflow_file`.
#'
#' @param count_table Path to a tab-delimited file or a data.frame/tibble.
#'   Column 1 must be local ASV labels. Remaining columns are sample names
#'   with read counts. If `fasta_path` is `NULL`, column 2 must contain DNA
#'   sequences (they are used for `asv_id` computation and then dropped from
#'   the count data).
#' @param amplicon_id Object ID of the existing `amplicon` object for this
#'   round of ASV generation. The object's subtype is used as `primer_set_id`.
#' @param workflow_id Workflow ID of the DADA2 workflow that produced this data.
#'   Must already exist in the `workflow` table.
#' @param fasta_path Optional path to a FASTA file. If provided, sequences are
#'   read from the FASTA (matched by ASV label) rather than from column 2 of
#'   `count_table`.
#' @param sample_map Optional named character vector mapping local sample column
#'   names to readset object IDs in the database. Names are local column names;
#'   values are object IDs. If `NULL`, column names are assumed to already be
#'   readset object IDs.
#' @param validate_only Logical. If `TRUE`, validates inputs without writing to
#'   the database (no backup created).
#' @param db_path Path to the `.den` database. Defaults to the active database.
#'
#' @returns Invisibly returns a list: `n_new` (ASVs inserted), `n_existing`
#'   (ASVs already in `asv` table), `n_labels` (rows written to `amplicon_asv`).
#' @export
read_amplicon <- function(count_table,
                          amplicon_id,
                          workflow_id,
                          fasta_path = NULL,
                          sample_map = NULL,
                          validate_only = FALSE,
                          db_path = NULL) {

  db_path <- gopher_db_path(db_path)

  # --- 1. Load count table ---
  if (is.character(count_table) && length(count_table) == 1) {
    if (!file.exists(count_table)) {
      cli::cli_abort("count_table not found: {.path {count_table}}")
    }
    count_path  <- normalizePath(count_table, mustWork = FALSE)
    count_table <- utils::read.delim(count_table, check.names = FALSE,
                                     stringsAsFactors = FALSE)
  } else {
    count_path  <- NULL
    count_table <- as.data.frame(count_table, stringsAsFactors = FALSE)
  }

  if (ncol(count_table) < 2) {
    cli::cli_abort("count_table must have at least 2 columns (ASV label + one sample or sequence).")
  }

  asv_labels <- as.character(count_table[[1]])

  # --- 2. Resolve sequences ---
  if (!is.null(fasta_path)) {
    if (!file.exists(fasta_path)) {
      cli::cli_abort("fasta_path not found: {.path {fasta_path}}")
    }
    all_seqs <- .read_fasta(fasta_path)
    missing  <- setdiff(asv_labels, names(all_seqs))
    if (length(missing) > 0) {
      cli::cli_abort(c(
        "{length(missing)} ASV label(s) in count_table not found in FASTA:",
        "i" = paste(utils::head(missing, 5), collapse = ", ")
      ))
    }
    sequences  <- stats::setNames(all_seqs[asv_labels], asv_labels)
    sample_dat <- count_table[, -1, drop = FALSE]
  } else {
    col2 <- as.character(count_table[[2]])
    if (!all(grepl("^[ACGTUacgtuNnRYSWKMBDHV-]+$", col2))) {
      cli::cli_abort(c(
        "No {.arg fasta_path} provided and column 2 does not look like DNA sequences.",
        "i" = "Provide a FASTA file via {.arg fasta_path} or include sequences in column 2."
      ))
    }
    sequences  <- stats::setNames(col2, asv_labels)
    sample_dat <- count_table[, -c(1, 2), drop = FALSE]
    if (ncol(sample_dat) == 0) {
      cli::cli_abort("After removing ASV label and sequence columns, no sample columns remain.")
    }
  }

  # --- 3. Apply sample_map ---
  sample_cols <- names(sample_dat)
  if (!is.null(sample_map)) {
    mapped   <- sample_map[sample_cols]
    unmapped <- sample_cols[is.na(mapped)]
    if (length(unmapped) > 0) {
      cli::cli_abort(c(
        "{length(unmapped)} sample column(s) not found in sample_map:",
        "i" = paste(utils::head(unmapped, 5), collapse = ", ")
      ))
    }
    sample_cols <- unname(mapped)
  }

  # --- 4. Compute asv_ids ---
  clean_seq <- function(s) toupper(gsub("-", "", s))
  asv_ids   <- vapply(sequences, function(s) {
    digest::digest(clean_seq(s), algo = "md5", serialize = FALSE)
  }, character(1))

  n_raw    <- length(asv_ids)
  n_unique <- length(unique(asv_ids))
  if (n_unique < n_raw) {
    cli::cli_alert_warning("{n_raw - n_unique} duplicate sequence(s) detected -- duplicates share one asv_id.")
  }

  cli::cli_alert_info("count_table: {n_raw} ASV label(s), {length(sample_cols)} sample(s), primer_set resolved from amplicon")

  # --- 5. Validate and insert ---
  if (!validate_only) {
    backup_db(db_path = db_path)
  }

  result <- with_gopher_con(
    .f = function(con) {

      # Validate amplicon object
      batch_row <- DBI::dbGetQuery(con,
        "SELECT object_id, object_subtype FROM object WHERE object_id = ? AND object_type = 'amplicon'",
        params = list(amplicon_id))
      if (nrow(batch_row) == 0) {
        cli::cli_abort("amplicon_id {.val {amplicon_id}} not found or is not of type amplicon.")
      }
      primer_set_id <- batch_row$object_subtype[1]

      # Validate workflow
      wf_n <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS n FROM workflow WHERE workflow_id = ?",
        params = list(workflow_id))$n
      if (wf_n == 0L) {
        cli::cli_abort("workflow_id {.val {workflow_id}} not found in workflow table.")
      }

      # Warn about any sample columns not present as readsets
      if (length(sample_cols) > 0) {
        phs   <- paste(rep("?", length(sample_cols)), collapse = ", ")
        found <- DBI::dbGetQuery(con,
          sprintf("SELECT object_id FROM object WHERE object_id IN (%s) AND object_type = 'readset'", phs),
          params = as.list(sample_cols))$object_id
        missing_rs <- setdiff(sample_cols, found)
        if (length(missing_rs) > 0) {
          cli::cli_alert_warning(
            "{length(missing_rs)} sample column(s) not found as readset objects in the DB:"
          )
          cli::cli_bullets(stats::setNames(
            utils::head(missing_rs, 10),
            rep("*", min(10L, length(missing_rs)))
          ))
        }
      }

      if (validate_only) {
        cli::cli_alert_success(
          "Validation passed -- {n_raw} ASV(s), primer_set: {.val {primer_set_id}}"
        )
        return(invisible(list(n_new = NA, n_existing = NA, n_labels = NA)))
      }

      DBI::dbBegin(con)
      tryCatch({

        # Build unique ASV frame for insertion
        asv_frame <- data.frame(
          asv_id        = unname(asv_ids),
          sequence      = vapply(sequences, clean_seq, character(1)),
          primer_set_id = primer_set_id,
          stringsAsFactors = FALSE
        )
        asv_frame <- asv_frame[!duplicated(asv_frame$asv_id), ]

        # Determine which are already in the table (chunked to stay under SQLite limit)
        existing_ids <- character(0)
        chunk_size   <- 900L
        ids_to_check <- asv_frame$asv_id
        for (i in seq(1L, length(ids_to_check), by = chunk_size)) {
          chunk <- ids_to_check[i:min(i + chunk_size - 1L, length(ids_to_check))]
          phs   <- paste(rep("?", length(chunk)), collapse = ", ")
          found <- DBI::dbGetQuery(con,
            sprintf("SELECT asv_id FROM asv WHERE asv_id IN (%s)", phs),
            params = as.list(chunk))$asv_id
          existing_ids <- c(existing_ids, found)
        }

        new_asvs   <- asv_frame[!asv_frame$asv_id %in% existing_ids, ]
        n_new      <- nrow(new_asvs)
        n_existing <- nrow(asv_frame) - n_new

        if (n_new > 0L) {
          DBI::dbAppendTable(con, "asv", new_asvs)
        }

        # amplicon_asv mappings -- INSERT OR IGNORE handles re-runs
        for (i in seq_along(asv_labels)) {
          DBI::dbExecute(con,
            "INSERT OR IGNORE INTO amplicon_asv (amplicon_object_id, asv_label, asv_id) VALUES (?, ?, ?)",
            params = list(amplicon_id, asv_labels[i], asv_ids[i]))
        }

        # Register count table in workflow_file
        if (!is.null(count_path)) {
          already_wf <- DBI::dbGetQuery(con,
            "SELECT COUNT(*) AS n FROM workflow_file WHERE workflow_id = ? AND file_role = 'abundance_matrix' AND file_path = ?",
            params = list(workflow_id, count_path))$n
          if (already_wf == 0L) {
            ext <- tolower(tools::file_ext(count_path))
            fmt <- switch(ext, tsv = "tsv", txt = "tsv", csv = "csv", ext)
            DBI::dbExecute(con,
              "INSERT INTO workflow_file (workflow_id, file_role, file_path, file_format, checksum) VALUES (?, ?, ?, ?, ?)",
              params = list(workflow_id, "abundance_matrix", count_path, fmt, NA_character_))
            cli::cli_alert_info("Registered abundance_matrix: {.path {count_path}}")
          }
        }

        DBI::dbCommit(con)

        cli::cli_alert_success(
          "{n_new} new ASV(s) inserted, {n_existing} already existed"
        )
        cli::cli_alert_success(
          "{length(asv_labels)} label mapping(s) written to amplicon_asv"
        )

        invisible(list(n_new = n_new, n_existing = n_existing, n_labels = length(asv_labels)))

      }, error = function(e) {
        DBI::dbRollback(con)
        cli::cli_abort(c("read_amplicon() failed -- changes rolled back.", "x" = conditionMessage(e)))
      })
    },
    db_path = db_path
  )

  invisible(result)
}

# Minimal FASTA reader -- named character vector (label -> sequence, first word of header)
.read_fasta <- function(path) {
  lines      <- readLines(path, warn = FALSE)
  header_idx <- which(startsWith(lines, ">"))
  if (length(header_idx) == 0L) return(character(0))

  labels <- sub("^>\\s*", "", lines[header_idx])
  labels <- sub("\\s.*$", "", labels)

  seqs <- character(length(header_idx))
  for (i in seq_along(header_idx)) {
    start   <- header_idx[i] + 1L
    end     <- if (i < length(header_idx)) header_idx[i + 1L] - 1L else length(lines)
    seqs[i] <- if (start <= end) paste(lines[start:end], collapse = "") else ""
  }
  stats::setNames(seqs, labels)
}

# -----------------------------------------------------------------------------
# read_taxonomy
# -----------------------------------------------------------------------------

#' Ingest taxonomy assignments into a gopheR den
#'
#' Reads a two-column table of ASV identifiers and taxonomy strings and writes
#' them to `asv_taxonomy`. Each `(asv_id, workflow_id)` pair is the primary
#' key, so re-running with the same workflow replaces prior assignments.
#'
#' @param taxonomy_table Path to a tab-delimited file or a data.frame. Column 1
#'   is the ASV identifier (asv_id or local label); column 2 is the taxonomy
#'   string. Additional columns (e.g. confidence) are ignored.
#' @param workflow_id Workflow ID for this classification run. Must already
#'   exist in the `workflow` table.
#' @param amplicon_id If provided, column 1 contains local ASV labels scoped
#'   to this amplicon object; they are mapped to global asv_ids via `amplicon_asv`. If
#'   `NULL`, column 1 must already be asv_ids (MD5 hashes).
#' @param validate_only Logical. If `TRUE`, validate inputs without writing.
#' @param db_path Path to the `.den` database. Defaults to the active database.
#'
#' @returns Invisibly returns a list with `n_inserted`.
#' @export
read_taxonomy <- function(taxonomy_table,
                          workflow_id,
                          amplicon_id = NULL,
                          validate_only = FALSE,
                          db_path = NULL) {

  db_path <- gopher_db_path(db_path)

  if (is.character(taxonomy_table) && length(taxonomy_table) == 1) {
    if (!file.exists(taxonomy_table)) {
      cli::cli_abort("taxonomy_table not found: {.path {taxonomy_table}}")
    }
    taxonomy_table <- utils::read.delim(taxonomy_table, check.names = FALSE,
                                        stringsAsFactors = FALSE)
  } else {
    taxonomy_table <- as.data.frame(taxonomy_table, stringsAsFactors = FALSE)
  }

  if (ncol(taxonomy_table) < 2) {
    cli::cli_abort("taxonomy_table must have at least 2 columns (ID + taxonomy string).")
  }

  raw_ids  <- as.character(taxonomy_table[[1]])
  tax_strs <- as.character(taxonomy_table[[2]])

  cli::cli_alert_info("taxonomy_table: {length(raw_ids)} row(s)")

  if (!validate_only) backup_db(db_path = db_path)

  with_gopher_con(
    .f = function(con) {

      wf_n <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS n FROM workflow WHERE workflow_id = ?",
        params = list(workflow_id))$n
      if (wf_n == 0L) cli::cli_abort("workflow_id {.val {workflow_id}} not found.")

      # Resolve local labels -> asv_ids if needed
      asv_ids <- raw_ids
      if (!is.null(amplicon_id)) {
        mapping <- DBI::dbGetQuery(con,
          "SELECT asv_label, asv_id FROM amplicon_asv WHERE amplicon_object_id = ?",
          params = list(amplicon_id))
        if (nrow(mapping) == 0) {
          cli::cli_abort("No amplicon_asv entries for amplicon_id {.val {amplicon_id}}.")
        }
        lut     <- stats::setNames(mapping$asv_id, mapping$asv_label)
        asv_ids <- unname(lut[raw_ids])
        n_miss  <- sum(is.na(asv_ids))
        if (n_miss > 0) {
          cli::cli_alert_warning("{n_miss} label(s) not in amplicon_asv -- will be skipped.")
        }
      }

      # Filter to rows with valid asv_ids that actually exist in the asv table
      valid       <- !is.na(asv_ids)
      unique_ids  <- unique(asv_ids[valid])
      chunk_size  <- 900L
      found_ids   <- character(0)
      for (i in seq(1L, length(unique_ids), by = chunk_size)) {
        chunk <- unique_ids[i:min(i + chunk_size - 1L, length(unique_ids))]
        phs   <- paste(rep("?", length(chunk)), collapse = ", ")
        found_ids <- c(found_ids, DBI::dbGetQuery(con,
          sprintf("SELECT asv_id FROM asv WHERE asv_id IN (%s)", phs),
          params = as.list(chunk))$asv_id)
      }
      valid <- valid & asv_ids %in% found_ids
      n_skip <- sum(!valid)
      if (n_skip > 0) {
        cli::cli_alert_warning("{n_skip} row(s) skipped -- asv_id not found in asv table.")
      }

      if (validate_only) {
        cli::cli_alert_success("Validation passed -- {sum(valid)} taxonomy rows ready to insert.")
        return(invisible(NULL))
      }

      ins_df <- data.frame(
        asv_id      = asv_ids[valid],
        workflow_id = workflow_id,
        taxonomy    = tax_strs[valid],
        stringsAsFactors = FALSE
      )

      DBI::dbBegin(con)
      tryCatch({
        DBI::dbWriteTable(con, "tmp_taxonomy", ins_df, temporary = TRUE, overwrite = TRUE)
        n_inserted <- DBI::dbExecute(con,
          "INSERT OR REPLACE INTO asv_taxonomy SELECT * FROM tmp_taxonomy")
        DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_taxonomy")
        DBI::dbCommit(con)
        cli::cli_alert_success("{n_inserted} taxonomy assignment(s) written to asv_taxonomy.")
        invisible(list(n_inserted = n_inserted))
      }, error = function(e) {
        DBI::dbRollback(con)
        cli::cli_abort(c("read_taxonomy() failed -- changes rolled back.", "x" = conditionMessage(e)))
      })
    },
    db_path = db_path
  )
}

# -----------------------------------------------------------------------------
# read_clustering
# -----------------------------------------------------------------------------

#' Ingest ASV clustering results into a gopheR den
#'
#' Reads VSEARCH UC format or a simple two-column TSV and writes assignments to
#' `asv_cluster`. By default, immediately archives older clustering runs for the
#' same primer set and cluster type via `trim_clustering()`.
#'
#' @param clustering_output Path to the clustering output file.
#' @param workflow_id Workflow ID for this clustering run. Must already exist.
#' @param cluster_type Short label for the clustering threshold, e.g.
#'   `"cluster97"` or `"cluster99"`.
#' @param primer_set_id Primer set this clustering applies to (e.g. `"V4"`).
#'   Used to scope `trim_clustering()`.
#' @param format `"uc"` (VSEARCH UC output, default) or `"tsv"` (two-column:
#'   asv_id, cluster_id -- no representative information).
#' @param amplicon_id If provided, IDs in the file are local ASV labels from
#'   this amplicon object; they are mapped to asv_ids via `amplicon_asv`.
#' @param trim Logical. If `TRUE` (default), call `trim_clustering()` after
#'   inserting to archive older runs for this primer set + cluster type.
#' @param validate_only Logical.
#' @param db_path Path to the `.den` database.
#'
#' @returns Invisibly returns a list with `n_rows` and `n_reps`.
#' @export
read_clustering <- function(clustering_output,
                            workflow_id,
                            cluster_type,
                            primer_set_id,
                            format = c("uc", "tsv"),
                            amplicon_id = NULL,
                            trim = TRUE,
                            validate_only = FALSE,
                            db_path = NULL) {

  format  <- match.arg(format)
  db_path <- gopher_db_path(db_path)

  # Parse input
  if (format == "uc") {
    if (!file.exists(clustering_output)) {
      cli::cli_abort("clustering_output not found: {.path {clustering_output}}")
    }
    cluster_df <- .parse_uc(clustering_output)
  } else {
    if (is.character(clustering_output) && length(clustering_output) == 1) {
      if (!file.exists(clustering_output)) {
        cli::cli_abort("clustering_output not found: {.path {clustering_output}}")
      }
      cluster_df <- utils::read.delim(clustering_output, check.names = FALSE,
                                      stringsAsFactors = FALSE, header = TRUE)
    } else {
      cluster_df <- as.data.frame(clustering_output, stringsAsFactors = FALSE)
    }
    names(cluster_df)[1:2] <- c("asv_id", "cluster_id")
    cluster_df$is_representative <- as.integer(
      as.character(cluster_df$asv_id) == as.character(cluster_df$cluster_id)
    )
  }

  if (nrow(cluster_df) == 0) cli::cli_abort("No rows parsed from clustering_output.")

  n_rows <- nrow(cluster_df)
  n_reps <- sum(cluster_df$is_representative == 1L)
  cli::cli_alert_info("clustering_output: {n_rows} ASV(s), {n_reps} representative(s)")

  if (!validate_only) backup_db(db_path = db_path)

  with_gopher_con(
    .f = function(con) {

      wf_n <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS n FROM workflow WHERE workflow_id = ?",
        params = list(workflow_id))$n
      if (wf_n == 0L) cli::cli_abort("workflow_id {.val {workflow_id}} not found.")

      # Resolve local labels if needed
      if (!is.null(amplicon_id)) {
        mapping <- DBI::dbGetQuery(con,
          "SELECT asv_label, asv_id FROM amplicon_asv WHERE amplicon_object_id = ?",
          params = list(amplicon_id))
        lut <- stats::setNames(mapping$asv_id, mapping$asv_label)
        cluster_df$asv_id     <<- unname(lut[cluster_df$asv_id])
        cluster_df$cluster_id <<- unname(lut[cluster_df$cluster_id])
        n_na <- sum(is.na(cluster_df$asv_id) | is.na(cluster_df$cluster_id))
        if (n_na > 0) {
          cli::cli_alert_warning("{n_na} row(s) could not be mapped -- will be skipped.")
          cluster_df <<- cluster_df[!is.na(cluster_df$asv_id) & !is.na(cluster_df$cluster_id), ]
        }
      }

      # Check asv_ids exist with correct primer_set
      all_ids    <- unique(c(cluster_df$asv_id, cluster_df$cluster_id))
      chunk_size <- 900L
      found_ids  <- character(0)
      for (i in seq(1L, length(all_ids), by = chunk_size)) {
        chunk <- all_ids[i:min(i + chunk_size - 1L, length(all_ids))]
        phs   <- paste(rep("?", length(chunk)), collapse = ", ")
        found_ids <- c(found_ids, DBI::dbGetQuery(con,
          sprintf("SELECT asv_id FROM asv WHERE asv_id IN (%s) AND primer_set_id = ?", phs),
          params = c(as.list(chunk), list(primer_set_id)))$asv_id)
      }
      missing_ids <- setdiff(all_ids, found_ids)
      if (length(missing_ids) > 0) {
        cli::cli_alert_warning(
          "{length(missing_ids)} ID(s) not found in asv table for primer_set {.val {primer_set_id}}."
        )
      }

      if (validate_only) {
        cli::cli_alert_success("Validation passed -- {nrow(cluster_df)} clustering rows ready.")
        return(invisible(NULL))
      }

      ins_df <- data.frame(
        asv_id           = cluster_df$asv_id,
        cluster_type     = cluster_type,
        cluster_id       = cluster_df$cluster_id,
        workflow_id      = workflow_id,
        is_representative = as.integer(cluster_df$is_representative),
        stringsAsFactors = FALSE
      )
      ins_df <- ins_df[ins_df$asv_id %in% found_ids & ins_df$cluster_id %in% found_ids, ]

      DBI::dbBegin(con)
      tryCatch({
        DBI::dbWriteTable(con, "tmp_cluster", ins_df, temporary = TRUE, overwrite = TRUE)
        n_ins <- DBI::dbExecute(con,
          "INSERT OR REPLACE INTO asv_cluster SELECT * FROM tmp_cluster")
        DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_cluster")
        DBI::dbCommit(con)
        cli::cli_alert_success(
          "{n_ins} clustering row(s) written ({n_reps} representative(s))."
        )
      }, error = function(e) {
        DBI::dbRollback(con)
        cli::cli_abort(c("read_clustering() failed -- changes rolled back.", "x" = conditionMessage(e)))
      })

      if (trim) {
        trim_clustering(primer_set_id    = primer_set_id,
                        cluster_type     = cluster_type,
                        keep_workflow_id = workflow_id,
                        db_path          = db_path)
      }

      invisible(list(n_rows = nrow(ins_df), n_reps = n_reps))
    },
    db_path = db_path
  )
}

# -----------------------------------------------------------------------------
# trim_clustering
# -----------------------------------------------------------------------------

#' Archive old clustering runs for a primer set
#'
#' For a given `primer_set_id` and `cluster_type`, writes all `asv_cluster`
#' rows from workflows other than `keep_workflow_id` to JSON files in
#' `archive/clustering/`, then removes them from the database. Called
#' automatically by `read_clustering()` when `trim = TRUE`.
#'
#' @param primer_set_id Primer set to scope the trim (e.g. `"V4"`).
#' @param cluster_type Cluster type to scope the trim (e.g. `"cluster97"`).
#' @param keep_workflow_id The workflow whose rows remain in the database.
#' @param db_path Path to the `.den` database.
#'
#' @returns Invisibly returns the number of workflows archived.
#' @export
trim_clustering <- function(primer_set_id,
                            cluster_type,
                            keep_workflow_id,
                            db_path = NULL) {

  db_path  <- gopher_db_path(db_path)
  den_root <- find_den_root(dirname(db_path))

  if (is.null(den_root)) {
    cli::cli_alert_warning("No den.yaml found -- cannot locate archive path. Skipping trim.")
    return(invisible(0L))
  }

  archive_dir <- file.path(den_root, "archive", "clustering")
  if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)

  with_gopher_con(
    .f = function(con) {

      old_wfs <- DBI::dbGetQuery(con, "
        SELECT DISTINCT ac.workflow_id
        FROM asv_cluster ac
        JOIN asv a ON a.asv_id = ac.asv_id
        WHERE a.primer_set_id = ?
          AND ac.cluster_type = ?
          AND ac.workflow_id  != ?
      ", params = list(primer_set_id, cluster_type, keep_workflow_id))$workflow_id

      if (length(old_wfs) == 0L) {
        cli::cli_alert_info(
          "No old {.val {cluster_type}} runs to archive for primer set {.val {primer_set_id}}."
        )
        return(invisible(0L))
      }

      DBI::dbBegin(con)
      tryCatch({
        for (wf in old_wfs) {
          rows <- DBI::dbGetQuery(con, "
            SELECT ac.*
            FROM asv_cluster ac
            JOIN asv a ON a.asv_id = ac.asv_id
            WHERE a.primer_set_id = ? AND ac.workflow_id = ?
          ", params = list(primer_set_id, wf))

          payload <- list(
            primer_set_id = primer_set_id,
            cluster_type  = cluster_type,
            workflow_id   = wf,
            archived_at   = format(Sys.Date(), "%Y-%m-%d"),
            rows          = rows
          )
          archive_file <- file.path(archive_dir,
            sprintf("clustering_%s_%s.json", primer_set_id, wf))
          writeLines(
            jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE),
            archive_file
          )

          DBI::dbExecute(con, "
            DELETE FROM asv_cluster
            WHERE workflow_id = ?
              AND asv_id IN (SELECT asv_id FROM asv WHERE primer_set_id = ?)
          ", params = list(wf, primer_set_id))

          cli::cli_alert_success(
            "Archived {nrow(rows)} row(s) from {.val {wf}} to {.path {archive_file}}"
          )
        }
        DBI::dbCommit(con)
        invisible(length(old_wfs))
      }, error = function(e) {
        DBI::dbRollback(con)
        cli::cli_abort(c("trim_clustering() failed -- changes rolled back.", "x" = conditionMessage(e)))
      })
    },
    db_path = db_path
  )
}

# -----------------------------------------------------------------------------
# restore_clustering
# -----------------------------------------------------------------------------

#' Restore an archived clustering run into the database
#'
#' Reads a JSON archive file produced by `trim_clustering()` and re-inserts
#' its rows into `asv_cluster`. Rows already present (same primary key) are
#' silently skipped.
#'
#' @param workflow_id The workflow ID of the archived run to restore.
#' @param db_path Path to the `.den` database.
#'
#' @returns Invisibly returns a list with `n_restored`.
#' @export
restore_clustering <- function(workflow_id, db_path = NULL) {

  db_path  <- gopher_db_path(db_path)
  den_root <- find_den_root(dirname(db_path))

  if (is.null(den_root)) {
    cli::cli_abort("No den.yaml found -- cannot locate archive directory.")
  }

  archive_dir <- file.path(den_root, "archive", "clustering")
  pattern     <- sprintf("clustering_.*_%s\\.json$", workflow_id)
  files       <- list.files(archive_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0L) {
    cli::cli_abort(
      "No archive file found for {.val {workflow_id}} in {.path {archive_dir}}"
    )
  }
  if (length(files) > 1L) {
    cli::cli_abort(
      "Multiple archive files matched {.val {workflow_id}} -- expected one: {.path {files}}"
    )
  }

  payload <- jsonlite::fromJSON(files[1], simplifyVector = TRUE)
  rows    <- as.data.frame(payload$rows, stringsAsFactors = FALSE)

  if (nrow(rows) == 0L) {
    cli::cli_alert_info("Archive file is empty -- nothing to restore.")
    return(invisible(list(n_restored = 0L)))
  }

  with_gopher_con(
    .f = function(con) {
      DBI::dbBegin(con)
      tryCatch({
        DBI::dbWriteTable(con, "tmp_restore", rows, temporary = TRUE, overwrite = TRUE)
        n_restored <- DBI::dbExecute(con,
          "INSERT OR IGNORE INTO asv_cluster SELECT * FROM tmp_restore")
        DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_restore")
        DBI::dbCommit(con)
        cli::cli_alert_success(
          "Restored {n_restored} clustering row(s) for workflow {.val {workflow_id}}."
        )
        invisible(list(n_restored = n_restored))
      }, error = function(e) {
        DBI::dbRollback(con)
        cli::cli_abort(c("restore_clustering() failed -- rolled back.", "x" = conditionMessage(e)))
      })
    },
    db_path = db_path
  )
}

# VSEARCH UC format parser.
# Returns data.frame: asv_id, cluster_id, is_representative
# S rows = centroid (representative); H rows = member hitting a centroid; others ignored.
.parse_uc <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) == 0L) return(data.frame())

  parts <- strsplit(lines, "\t", fixed = TRUE)
  types <- vapply(parts, `[[`, character(1), 1)
  keep  <- types %in% c("H", "S")
  if (!any(keep)) return(data.frame())

  parts <- parts[keep]
  types <- types[keep]

  # UC columns (1-indexed): 1=type, 9=query label, 10=target label (* for S)
  queries <- vapply(parts, function(p) if (length(p) >= 9) p[[9]] else NA_character_, character(1))
  targets <- vapply(parts, function(p) if (length(p) >= 10) p[[10]] else NA_character_, character(1))

  data.frame(
    asv_id           = queries,
    cluster_id       = ifelse(types == "S", queries, targets),
    is_representative = as.integer(types == "S"),
    stringsAsFactors = FALSE
  )
}
