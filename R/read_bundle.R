#' Read and ingest a gopheR Excel bundle
#'
#' Reads an Excel bundle, validates the data against database specifications,
#' and optionally inserts the data into the database. All ingestion happens
#' within a transaction with automatic rollback on error.
#'
#' @param bundle_path Character scalar. Path to the Excel bundle file to read.
#' @param db_path Optional character scalar. Directory containing the gopheR
#'   SQLite database. If \code{NULL}, the path is resolved via
#'   \code{gopher_db_path()}.
#' @param backup Logical. If \code{TRUE} (default), creates a timestamped backup
#'   of the database before making any changes. The backup is used for automatic
#'   restoration if any error occurs during ingestion.
#' @param validate_only Logical. If \code{TRUE}, validates the bundle without
#'   inserting any data. Useful for checking data quality before committing.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns a list with components:
#'   \describe{
#'     \item{bundle_path}{Normalized path to the bundle file}
#'     \item{db_path}{Normalized path to the database file}
#'     \item{backup}{List with backup information (if created), or NULL}
#'     \item{results}{List with ingestion results for each sheet (e.g., objects, edges)}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Creates a database backup (unless \code{backup = FALSE} or \code{validate_only = TRUE})
#'   \item Loads the Excel workbook
#'   \item Begins a database transaction
#'   \item Validates and ingests each sheet (currently: objects)
#'   \item If any error occurs: rolls back transaction and restores from backup
#'   \item If all succeeds: commits transaction
#' }
#'
#' Results include counts by object type and validation status. A summary
#' report is printed to the console showing what was processed.
#'
#' @examples
#' \dontrun{
#' # Set database location
#' options(gopheR.db_path = "/path/to/database/folder")
#'
#' # Validate without inserting
#' read_bundle("my_data.xlsx", validate_only = TRUE)
#'
#' # Insert validated data
#' results <- read_bundle("my_data.xlsx")
#' results$results$objects  # Check what was inserted
#' }
#'
#' @export

read_bundle <- function(bundle_path,
                        db_path = NULL,
                        backup = TRUE,
                        validate_only = FALSE,
                        ...) {

  db_path <- gopher_db_path(db_path)

  if (!file.exists(bundle_path)) {
    cli::cli_abort("Bundle file does not exist: {.path {bundle_path}}")
  }

  # Create backup before any modifications
  backup_info <- NULL
  if (isTRUE(backup) && !isTRUE(validate_only)) {
    backup_info <- backup_db(db_path = db_path)
  }

  # Load workbook
  wb <- openxlsx::loadWorkbook(bundle_path)
  sheet_names <- names(wb)

  cli::cli_alert_info("Processing bundle: {.path {basename(bundle_path)}}")
  cli::cli_alert_info("Available sheets: {.val {sheet_names}}")

  # Wrap ingestion in tryCatch for rollback on error
  ingestion_result <- tryCatch(
    {
      # Process sheets with transaction support
      with_gopher_con(
        .f = function(con) {

          # Begin transaction
          if (!isTRUE(validate_only)) {
            DBI::dbBegin(con)
          }

          # Initialize results list
          results <- list()

          # Process object sheet
          if ("object" %in% sheet_names) {
            results$objects <- ingest_objects_with_con(wb, con, validate_only = validate_only)
          } else {
            cli::cli_alert_warning("No 'object' sheet found in bundle.")
            results$objects <- NULL
          }

          # Future: process other sheets (edges, workflows, etc.)
          # if ("edge" %in% sheet_names) {
          #   results$edges <- ingest_edges_with_con(wb, con, validate_only = validate_only)
          # }

          # Commit transaction if not validate_only
          if (!isTRUE(validate_only)) {
            DBI::dbCommit(con)
            cli::cli_alert_success("All changes committed to database.")
          }

          results
        },
        db_path = db_path,
        read_only = validate_only
      )
    },
    error = function(e) {
      # On error, restore from backup if we created one
      if (!is.null(backup_info) && !isTRUE(validate_only)) {
        cli::cli_alert_warning("Error during ingestion. Restoring from backup...")

        restore_result <- tryCatch(
          {
            file.copy(
              from = backup_info$backup_path,
              to = backup_info$db_path,
              overwrite = TRUE,
              copy.mode = TRUE,
              copy.date = FALSE
            )
            cli::cli_alert_success("Database restored from backup.")
            TRUE
          },
          error = function(restore_error) {
            cli::cli_alert_danger(
              "Failed to restore backup: {conditionMessage(restore_error)}"
            )
            FALSE
          }
        )
      }

      # Re-throw the original error
      cli::cli_abort(c(
        "Bundle ingestion failed:",
        "x" = conditionMessage(e)
      ))
    }
  )

  # Print summary report
  print_ingestion_summary(ingestion_result, validate_only = validate_only)

  invisible(list(
    bundle_path = normalizePath(bundle_path, mustWork = TRUE),
    db_path = normalizePath(db_path, mustWork = TRUE),
    backup = backup_info,
    results = ingestion_result
  ))
}



#' Ingest objects from bundle worksheet (with existing connection)
#'
#' Reads the object sheet from an Excel bundle, validates the data, and
#' optionally inserts it into the database using an existing connection.
#'
#' @param wb An openxlsx workbook object.
#' @param con A DBI connection to the database.
#' @param validate_only Logical. If TRUE, only validates without inserting.
#'
#' @return Invisibly returns validation results.
#' @keywords internal
#' @importFrom rlang .data

ingest_objects_with_con <- function(wb, con, validate_only = FALSE) {

  # Read the object sheet
  object_data <- openxlsx::read.xlsx(wb, sheet = "object")

  if (is.null(object_data) || nrow(object_data) == 0) {
    cli::cli_alert_warning("Object sheet is empty. Skipping.")
    return(list(
      n_processed = 0,
      n_inserted = 0,
      by_type = list(),
      validation_passed = TRUE
    ))
  }

  cli::cli_alert_info("Found {nrow(object_data)} object(s) in bundle.")

  # Validate required columns
  required_cols <- c("object_id", "object_type")
  missing_cols <- setdiff(required_cols, names(object_data))

  if (length(missing_cols) > 0) {
    cli::cli_abort("Object sheet is missing required columns: {.val {missing_cols}}")
  }

  # Split object_type into object_type and object_subtype
  object_data <- split_object_type(object_data)

  # Validate against database specs
  validation_results <- validate_objects_with_con(object_data, con)

  if (!validation_results$valid) {
    cli::cli_abort(c(
      "Object validation failed:",
      "x" = validation_results$message
    ))
  }

  cli::cli_alert_success("Object validation passed.")

  # Count by object type
  type_counts <- object_data |>
    dplyr::count(.data$object_type, name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n))

  by_type <- as.list(stats::setNames(type_counts$n, type_counts$object_type))

  # Insert if not validate_only
  n_inserted <- 0
  if (!isTRUE(validate_only)) {
    n_inserted <- insert_objects_with_con(object_data, con)
    cli::cli_alert_success("Inserted {nrow(object_data)} object(s) into database.")
  } else {
    cli::cli_alert_info("Validation only mode - no data inserted.")
  }

  list(
    n_processed = nrow(object_data),
    n_inserted = n_inserted,
    by_type = by_type,
    validation_passed = TRUE
  )
}


#' Split combined object_type into type and subtype
#'
#' Takes a data frame with object_type column that may contain combined
#' values like "genome:MAG" and splits them into separate object_type and
#' object_subtype columns.
#'
#' @param df Data frame with object_type column.
#'
#' @return Data frame with object_type and object_subtype columns.
#' @keywords internal
#' @importFrom rlang .data

split_object_type <- function(df) {

  if (!"object_type" %in% names(df)) {
    return(df)
  }

  df |>
    tidyr::separate(
      col = .data$object_type,
      into = c("object_type", "object_subtype"),
      sep = ":",
      fill = "right",
      remove = TRUE
    )
}


#' Validate objects against database specifications (with connection)
#'
#' Checks that object types and subtypes are valid according to the database
#' specs, and that object IDs don't already exist.
#'
#' @param object_data Data frame of objects to validate.
#' @param con A DBI connection to the database.
#'
#' @return List with valid (logical) and message (character) elements.
#' @keywords internal
#' @importFrom rlang .data

validate_objects_with_con <- function(object_data, con) {

  # Get valid object types
  valid_types <- DBI::dbReadTable(con, "object_type") |>
    dplyr::pull(.data$object_type)

  # Get valid subtypes
  valid_subtypes <- DBI::dbReadTable(con, "object_subtype")

  # Check object_type validity
  invalid_types <- setdiff(object_data$object_type, valid_types)

  if (length(invalid_types) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Invalid object types:", paste(invalid_types, collapse = ", "))
    ))
  }

  # Check object_subtype validity (if present) using anti_join
  objects_with_subtype <- object_data |>
    dplyr::filter(!is.na(.data$object_subtype), nzchar(.data$object_subtype))

  if (nrow(objects_with_subtype) > 0) {
    invalid_subtypes <- objects_with_subtype |>
      dplyr::anti_join(
        valid_subtypes,
        by = c("object_type", "object_subtype")
      )

    if (nrow(invalid_subtypes) > 0) {
      first_invalid <- invalid_subtypes |>
        dplyr::slice(1)

      return(list(
        valid = FALSE,
        message = sprintf(
          "Invalid subtype '%s' for object type '%s'",
          first_invalid$object_subtype,
          first_invalid$object_type
        )
      ))
    }
  }

  # Check for duplicate object_ids in the bundle
  dup_ids <- object_data |>
    dplyr::group_by(.data$object_id) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::pull(.data$object_id) |>
    unique()

  if (length(dup_ids) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Duplicate object_ids in bundle:", paste(dup_ids, collapse = ", "))
    ))
  }

  # Check if object_ids already exist in database
  existing_objects <- DBI::dbReadTable(con, "object")

  if (nrow(existing_objects) > 0) {
    existing_ids <- intersect(object_data$object_id, existing_objects$object_id)

    if (length(existing_ids) > 0) {
      return(list(
        valid = FALSE,
        message = paste("Object IDs already exist in database:",
                      paste(existing_ids, collapse = ", "))
      ))
    }
  }

  list(valid = TRUE, message = "All validations passed")
}


#' Insert objects into the database (with connection)
#'
#' Inserts validated object data into the object table using an existing connection.
#'
#' @param object_data Data frame of validated objects.
#' @param con A DBI connection to the database.
#'
#' @return Invisibly returns the number of rows inserted.
#' @keywords internal
#' @importFrom rlang .data

insert_objects_with_con <- function(object_data, con) {

  # Prepare data for insertion - ensure columns match database schema
  insert_data <- object_data |>
    dplyr::select(
      .data$object_id,
      .data$object_type,
      .data$object_subtype,
      .data$label,
      .data$description,
      .data$created_by
    ) |>
    dplyr::mutate(
      object_subtype = dplyr::if_else(is.na(.data$object_subtype), "", .data$object_subtype)
    )

  # Insert into database
  DBI::dbWriteTable(
    con,
    "object",
    insert_data,
    append = TRUE,
    row.names = FALSE
  )

  invisible(nrow(insert_data))
}


#' Print ingestion summary report
#'
#' Prints a formatted summary of what was ingested from a bundle.
#'
#' @param results List of results from sheet ingestion functions.
#' @param validate_only Logical. Whether this was a validation-only run.
#'
#' @return Invisibly returns NULL.
#' @keywords internal

print_ingestion_summary <- function(results, validate_only = FALSE) {

  if (is.null(results) || length(results) == 0) {
    return(invisible(NULL))
  }

  cli::cli_h2("Ingestion Summary")

  # Objects summary
  if (!is.null(results$objects)) {
    obj_res <- results$objects

    if (obj_res$n_processed > 0) {
      if (isTRUE(validate_only)) {
        cli::cli_alert_info("Objects validated: {obj_res$n_processed}")
      } else {
        cli::cli_alert_success("Objects inserted: {obj_res$n_inserted}")
      }

      # Show breakdown by type
      if (length(obj_res$by_type) > 0) {
        type_summary <- purrr::map2_chr(
          names(obj_res$by_type),
          obj_res$by_type,
          ~ paste0(.y, " ", .x)
        )
        cli::cli_bullets(c("*" = paste(type_summary, collapse = ", ")))
      }
    }
  }

  # Future: edges, workflows, etc.

  invisible(NULL)
}


#' Create a backup of the gopheR database
#'
#' Creates a timestamped backup copy of the database with MD5 verification.
#' Useful for creating manual backups before risky operations.
#'
#' @param db_path Optional character scalar. Full path to the database file.
#'   If \code{NULL}, the path is resolved via \code{gopher_db_path()}.
#' @param backup_dir Optional character scalar. Directory to store the backup.
#'   If \code{NULL}, creates a \code{backups} folder next to the database.
#' @param timestamp Character scalar. Timestamp string to use in the backup
#'   filename. Defaults to current time in ISO format.
#' @param overwrite Logical. Whether to overwrite an existing backup with the
#'   same timestamp.
#'
#' @return A list with components:
#'   \describe{
#'     \item{db_path}{Normalized path to the source database}
#'     \item{backup_path}{Normalized path to the backup file}
#'     \item{md5}{MD5 checksum of the backup (verified to match source)}
#'     \item{timestamp}{Timestamp used in the backup filename}
#'   }
#'
#' @details
#' The backup filename format is: \code{basename.pre_ingest.TIMESTAMP.ext}
#'
#' After copying, the function verifies that the backup's MD5 checksum matches
#' the source file, ensuring the backup is not corrupted.
#'
#' @examples
#' \dontrun{
#' # Manual backup before risky operation
#' backup_info <- backup_db()
#'
#' # Backup to specific location
#' backup_db(backup_dir = "/path/to/backups")
#' }
#'
#' @export

backup_db <- function(db_path = NULL,
                      backup_dir = NULL,
                      timestamp = format(Sys.time(), "%Y%m%dT%H%M%S"),
                      overwrite = FALSE) {

  if (is.null(db_path)) {
    db_path <- gopher_db_path()
  }

  if (!file.exists(db_path)) {
    cli::cli_abort("Database file does not exist: {.path {db_path}}")
  }

  if (is.null(backup_dir)) {
    backup_dir <- file.path(dirname(db_path), "backups")
  }

  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

  db_name <- basename(db_path)
  db_stem <- sub("\\.[^.]+$", "", db_name)
  db_ext  <- sub("^.*(\\.[^.]+)$", "\\1", db_name)
  if (identical(db_ext, db_name)) db_ext <- ""

  backup_path <- file.path(
    backup_dir,
    paste0(db_stem, ".pre_ingest.", timestamp, db_ext)
  )

  if (file.exists(backup_path) && !isTRUE(overwrite)) {
    cli::cli_abort("Backup file already exists: {.path {backup_path}}")
  }

  ok <- file.copy(
    from = db_path,
    to = backup_path,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!ok || !file.exists(backup_path)) {
    cli::cli_abort("Failed to create backup: {.path {backup_path}}")
  }

  src_md5 <- unname(tools::md5sum(db_path))
  bak_md5 <- unname(tools::md5sum(backup_path))

  if (!identical(src_md5, bak_md5)) {
    cli::cli_abort(c(
      "Backup checksum mismatch.",
      "x" = "Source: {src_md5}",
      "x" = "Backup: {bak_md5}"
    ))
  }

  cli::cli_inform(c(
    "v" = "Database backup verified.",
    "i" = "MD5: {src_md5}",
    " " = "{.path {backup_path}}"
  ))

  list(
    db_path = normalizePath(db_path, mustWork = TRUE),
    backup_path = normalizePath(backup_path, mustWork = TRUE),
    md5 = src_md5,
    timestamp = timestamp
  )
}
