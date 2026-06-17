# Migration script: add amplicon support to an existing .den database
#
# Run this on any den created before gopheR 0.6.1.
# Safe to run more than once -- each step checks before acting.
#
# Usage:
#   gopheR::use_db("/path/to/your.den")
#   source(system.file("extdata", "migrate_amplicon.R", package = "gopheR"))

library(gopheR)
library(DBI)

local({

  con <- gopheR::gopher_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  existing_tables  <- DBI::dbListTables(con)
  existing_columns <- function(table) DBI::dbListFields(con, table)

  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")

  # ---------------------------------------------------------------------------
  # 1. Rebuild primer_set with TEXT primary key
  # ---------------------------------------------------------------------------
  if ("primer_set" %in% existing_tables) {
    pk_type <- DBI::dbGetQuery(con,
      "SELECT typeof(primer_set_id) FROM pragma_table_info('primer_set') WHERE name='primer_set_id'")[[1]]
    if (is.integer(pk_type)) {
      cli::cli_alert_info("primer_set: already has TEXT PK -- skipping rebuild")
    } else {
      cli::cli_alert_info("Rebuilding primer_set with TEXT primary key...")
      DBI::dbExecute(con, "
        CREATE TABLE primer_set_new (
          primer_set_id  TEXT PRIMARY KEY,
          marker         TEXT NOT NULL,
          region         TEXT,
          forward_primer TEXT NOT NULL,
          reverse_primer TEXT NOT NULL,
          fwd_name       TEXT,
          rev_name       TEXT,
          reference_type TEXT,
          reference_id   TEXT,
          target_notes   TEXT
        )")
      DBI::dbExecute(con, "DROP TABLE primer_set")
      DBI::dbExecute(con, "ALTER TABLE primer_set_new RENAME TO primer_set")
      DBI::dbExecute(con,
        "CREATE INDEX IF NOT EXISTS idx_primer_set_marker_region ON primer_set(marker, region)")
      cli::cli_alert_success("primer_set rebuilt")
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Add primer_set_id to asv
  # ---------------------------------------------------------------------------
  if (!("primer_set_id" %in% existing_columns("asv"))) {
    cli::cli_alert_info("Adding primer_set_id to asv...")
    DBI::dbExecute(con,
      "ALTER TABLE asv ADD COLUMN primer_set_id TEXT REFERENCES primer_set(primer_set_id)")
    cli::cli_alert_success("asv.primer_set_id added")
  } else {
    cli::cli_alert_info("asv.primer_set_id already exists -- skipping")
  }

  # ---------------------------------------------------------------------------
  # 3. Rebuild asv_cluster (remove CHECK, add workflow_id to PK, add is_representative)
  # ---------------------------------------------------------------------------
  asv_cluster_sql <- DBI::dbGetQuery(con,
    "SELECT sql FROM sqlite_master WHERE type='table' AND name='asv_cluster'")$sql
  if (length(asv_cluster_sql) > 0 &&
      (grepl("CHECK", asv_cluster_sql) || !grepl("is_representative", asv_cluster_sql))) {
    cli::cli_alert_info("Rebuilding asv_cluster...")
    old_rows <- DBI::dbGetQuery(con, "SELECT * FROM asv_cluster")
    DBI::dbExecute(con, "DROP INDEX IF EXISTS idx_asv_cluster_cluster")
    DBI::dbExecute(con, "DROP TABLE asv_cluster")
    DBI::dbExecute(con, "
      CREATE TABLE asv_cluster (
        asv_id            TEXT    NOT NULL,
        cluster_type      TEXT    NOT NULL,
        cluster_id        TEXT    NOT NULL,
        workflow_id       TEXT    NOT NULL,
        is_representative INTEGER NOT NULL DEFAULT 0,
        PRIMARY KEY (asv_id, cluster_type, workflow_id),
        FOREIGN KEY (asv_id)      REFERENCES asv(asv_id)           ON DELETE CASCADE,
        FOREIGN KEY (workflow_id) REFERENCES workflow(workflow_id)
      )")
    DBI::dbExecute(con,
      "CREATE INDEX idx_asv_cluster_cluster ON asv_cluster(cluster_type, cluster_id)")
    if (nrow(old_rows) > 0 && "workflow_id" %in% names(old_rows)) {
      old_rows$is_representative <- 0L
      DBI::dbWriteTable(con, "asv_cluster", old_rows, append = TRUE, row.names = FALSE)
      cli::cli_alert_info("Re-inserted {nrow(old_rows)} existing asv_cluster rows")
    }
    cli::cli_alert_success("asv_cluster rebuilt")
  } else {
    cli::cli_alert_info("asv_cluster already up to date -- skipping")
  }

  # ---------------------------------------------------------------------------
  # 4. Add asv_taxonomy table
  # ---------------------------------------------------------------------------
  if (!("asv_taxonomy" %in% existing_tables)) {
    cli::cli_alert_info("Creating asv_taxonomy...")
    DBI::dbExecute(con, "
      CREATE TABLE asv_taxonomy (
        asv_id      TEXT NOT NULL,
        workflow_id TEXT NOT NULL,
        taxonomy    TEXT NOT NULL,
        PRIMARY KEY (asv_id, workflow_id),
        FOREIGN KEY (asv_id)      REFERENCES asv(asv_id)           ON DELETE CASCADE,
        FOREIGN KEY (workflow_id) REFERENCES workflow(workflow_id)
      )")
    cli::cli_alert_success("asv_taxonomy created")
  } else {
    cli::cli_alert_info("asv_taxonomy already exists -- skipping")
  }

  # ---------------------------------------------------------------------------
  # 5. Add asv_batch object type and spec entries
  # ---------------------------------------------------------------------------
  insert_if_missing <- function(con, table, where_col, where_val, insert_sql) {
    n <- DBI::dbGetQuery(con,
      sprintf("SELECT COUNT(*) FROM %s WHERE %s = ?", table, where_col),
      params = list(where_val))[[1]]
    if (n == 0) DBI::dbExecute(con, insert_sql)
  }

  insert_if_missing(con, "object_type", "object_type", "asv_batch",
    "INSERT INTO object_type VALUES ('asv_batch',
      'A batch of ASV generation from a set of samples (e.g. one DADA2 run)')")

  old_readset_subtypes <- c("paired_end", "single_end", "nanopore", "pacbio")
  for (s in old_readset_subtypes) {
    n <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) FROM object_subtype WHERE object_type='readset' AND object_subtype=?",
      params = list(s))[[1]]
    if (n > 0) {
      DBI::dbExecute(con,
        "DELETE FROM object_subtype WHERE object_type='readset' AND object_subtype=?",
        params = list(s))
      cli::cli_alert_info("Removed deprecated readset subtype: {s}")
    }
  }

  subtypes <- list(
    c("asv_batch", "V4",     "16S V4 amplicon"),
    c("asv_batch", "V3-V4",  "16S V3-V4 amplicon"),
    c("asv_batch", "16S_FL", "16S full-length amplicon (PacBio/Nanopore)"),
    c("asv_batch", "ITS2",   "ITS2 amplicon (fungi)"),
    c("asv_batch", "18S",    "18S amplicon (legacy)"),
    c("asv_batch", "WANDA",  "SSU rRNA amplicon for arbuscular mycorrhizal fungi"),
    c("readset",   "shotgun","Shotgun metagenomics (technology in object_result)"),
    c("readset",   "V4",     "16S V4 amplicon"),
    c("readset",   "V3-V4",  "16S V3-V4 amplicon"),
    c("readset",   "16S_FL", "16S full-length amplicon (PacBio/Nanopore)"),
    c("readset",   "ITS2",   "ITS2 amplicon (fungi)"),
    c("readset",   "18S",    "18S amplicon (legacy)"),
    c("readset",   "WANDA",  "SSU rRNA amplicon for arbuscular mycorrhizal fungi")
  )
  for (s in subtypes) {
    n <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) FROM object_subtype WHERE object_type = ? AND object_subtype = ?",
      params = list(s[1], s[2]))[[1]]
    if (n == 0)
      DBI::dbExecute(con,
        "INSERT INTO object_subtype (object_type, object_subtype, description) VALUES (?, ?, ?)",
        params = as.list(s))
  }

  edge_rows <- list(
    c("readset", "asv_batch", "derived_from", "asv_batch was generated from this readset")
  )
  for (e in edge_rows) {
    n <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) FROM edge_spec WHERE parent_type=? AND child_type=? AND edge_type=?",
      params = list(e[1], e[2], e[3]))[[1]]
    if (n == 0)
      DBI::dbExecute(con, "INSERT INTO edge_spec VALUES (?, ?, ?, ?)", params = as.list(e))
  }

  obj_file_rows <- list(c("asv_batch", "asv_fasta", "FASTA file of filtered ASV sequences"))
  for (r in obj_file_rows) {
    n <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) FROM object_file_type_spec WHERE object_type=? AND file_role=?",
      params = list(r[1], r[2]))[[1]]
    if (n == 0)
      DBI::dbExecute(con,
        "INSERT INTO object_file_type_spec VALUES (?, ?, ?)", params = as.list(r))
  }

  wf_file_rows <- list(
    c("abundance_matrix",     "Filtered sample x ASV count table (TSV)"),
    c("abundance_matrix_raw", "Unfiltered DADA2 count table (TSV)"),
    c("phylogenetic_tree",    "Newick tree of filtered ASV sequences")
  )
  for (r in wf_file_rows) {
    n <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) FROM workflow_file_type_spec WHERE file_role=?",
      params = list(r[1]))[[1]]
    if (n == 0)
      DBI::dbExecute(con,
        "INSERT INTO workflow_file_type_spec VALUES (?, ?)", params = as.list(r))
  }

  result_rows <- list(
    c("asv_batch", "total_asvs",       "integer", "Total ASVs before prevalence filtering"),
    c("asv_batch", "filtered_asvs",    "integer", "ASVs retained after prevalence filtering"),
    c("asv_batch", "filter_threshold", "text",    "Prevalence filter applied, e.g. 2x2 or 5x5"),
    c("asv_batch", "median_depth",     "numeric", "Median per-sample sequencing depth"),
    c("readset",   "technology",       "text",    "Sequencing technology: illumina, pacbio, nanopore"),
    c("readset",   "layout",           "text",    "Library layout: paired_end, single_end, long_read")
  )
  for (r in result_rows) {
    n <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) FROM object_result_spec WHERE object_type=? AND key=?",
      params = list(r[1], r[2]))[[1]]
    if (n == 0)
      DBI::dbExecute(con,
        "INSERT INTO object_result_spec (object_type, key, value_type, description)
         VALUES (?, ?, ?, ?)",
        params = as.list(r))
  }

  cli::cli_alert_success("Spec entries updated")

  # ---------------------------------------------------------------------------
  # 6. Update existing readset objects: paired_end -> shotgun
  # ---------------------------------------------------------------------------
  n_paired <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) FROM object WHERE object_type='readset' AND object_subtype='paired_end'")[[1]]

  if (n_paired > 0) {
    cli::cli_alert_info(
      "Found {n_paired} readset:paired_end object(s) -- updating to readset:shotgun")
    cli::cli_alert_info(
      "These are assumed to be shotgun metagenomics; update manually if any are amplicon.")
    DBI::dbExecute(con,
      "UPDATE object SET object_subtype = 'shotgun'
       WHERE object_type = 'readset' AND object_subtype = 'paired_end'")
    cli::cli_alert_success("{n_paired} readset(s) updated to subtype 'shotgun'")
  } else {
    cli::cli_alert_info("No readset:paired_end objects found -- skipping subtype migration")
  }

  # ---------------------------------------------------------------------------
  # 7. Create archive/clustering directory next to the den file
  # ---------------------------------------------------------------------------
  db_path     <- gopheR::gopher_db_path()
  den_root    <- dirname(db_path)
  cluster_dir <- file.path(den_root, "archive", "clustering")
  if (!dir.exists(cluster_dir)) {
    dir.create(cluster_dir, recursive = TRUE)
    cli::cli_alert_success("Created {.path {cluster_dir}}")
  } else {
    cli::cli_alert_info("archive/clustering already exists -- skipping")
  }

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  cli::cli_alert_success("Migration complete -- den is ready for gopheR 0.6.1 amplicon features")

})
