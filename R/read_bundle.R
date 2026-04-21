#' Pre-flight validation of bundle before database backup
#'
#' Performs fast validation checks that don't require database queries.
#' Checks for empty created_by fields, duplicate IDs within bundle, and
#' basic structural issues.
#'
#' @param wb openxlsx workbook object
#' @param default_user Optional character scalar for filling empty created_by fields
#'
#' @return List with valid (logical), message (character), and wb (potentially modified workbook)
#' @keywords internal
preflight_validate_bundle <- function(wb, default_user = NULL) {

  sheet_names <- names(wb)
  errors <- character()

  # Read sheets if they exist
  people_data <- NULL
  workflow_data <- NULL
  object_data <- NULL
  edge_data <- NULL

  if ("people" %in% sheet_names) {
    people_data <- openxlsx::readWorkbook(wb, sheet = "people")
    # Filter empty rows
    if (nrow(people_data) > 0) {
      people_data <- people_data |>
        dplyr::filter(!is.na(.data$person_id) & nzchar(.data$person_id))
    }
  }

  if ("workflow" %in% sheet_names) {
    workflow_data <- openxlsx::readWorkbook(wb, sheet = "workflow")
    # Filter empty rows (prefilled templates)
    if (nrow(workflow_data) > 0) {
      workflow_data <- workflow_data |>
        dplyr::filter(!is.na(.data$workflow_id) & nzchar(.data$workflow_id)) |>
        dplyr::filter(!is.na(.data$description) & nzchar(.data$description))
    }
  }

  if ("object" %in% sheet_names) {
    object_data <- openxlsx::readWorkbook(wb, sheet = "object")
    # Filter empty rows
    if (nrow(object_data) > 0) {
      object_data <- object_data |>
        dplyr::filter(!is.na(.data$object_id) & nzchar(.data$object_id))
    }
  }

  if ("edge" %in% sheet_names) {
    edge_data <- openxlsx::readWorkbook(wb, sheet = "edge")
    # Filter empty rows
    if (nrow(edge_data) > 0) {
      edge_data <- edge_data |>
        dplyr::filter(!is.na(.data$parent_id) & nzchar(.data$parent_id)) |>
        dplyr::filter(!is.na(.data$child_id) & nzchar(.data$child_id))
    }
  }

  # Check 1: created_by required in workflows
  if (!is.null(workflow_data) && nrow(workflow_data) > 0) {
    empty_created_by <- is.na(workflow_data$created_by) | !nzchar(workflow_data$created_by)
    if (any(empty_created_by)) {
      if (!is.null(default_user)) {
        workflow_data$created_by[empty_created_by] <- default_user
        cli::cli_alert_info("Filled {sum(empty_created_by)} empty workflow created_by field(s) with '{default_user}'")
      } else {
        errors <- c(errors, sprintf("Workflow sheet has %d row(s) with empty created_by field. Provide default_user parameter or fill in Excel.", sum(empty_created_by)))
      }
    }
  }

  # Check 2: created_by required in objects
  if (!is.null(object_data) && nrow(object_data) > 0) {
    empty_created_by <- is.na(object_data$created_by) | !nzchar(object_data$created_by)
    if (any(empty_created_by)) {
      if (!is.null(default_user)) {
        object_data$created_by[empty_created_by] <- default_user
        cli::cli_alert_info("Filled {sum(empty_created_by)} empty object created_by field(s) with '{default_user}'")
      } else {
        errors <- c(errors, sprintf("Object sheet has %d row(s) with empty created_by field. Provide default_user parameter or fill in Excel.", sum(empty_created_by)))
      }
    }
  }

  # Check 3: Duplicate workflow_ids within bundle
  if (!is.null(workflow_data) && nrow(workflow_data) > 0) {
    dup_workflows <- workflow_data$workflow_id[duplicated(workflow_data$workflow_id)]
    if (length(dup_workflows) > 0) {
      errors <- c(errors, sprintf("Duplicate workflow_ids in bundle: %s", paste(dup_workflows, collapse = ", ")))
    }
  }

  # Check 4: Duplicate object_ids within bundle
  if (!is.null(object_data) && nrow(object_data) > 0) {
    dup_objects <- object_data$object_id[duplicated(object_data$object_id)]
    if (length(dup_objects) > 0) {
      errors <- c(errors, sprintf("Duplicate object_ids in bundle: %s", paste(dup_objects, collapse = ", ")))
    }
  }

  # Check 5: Edges reference objects in bundle (or will need DB check)
  # This is just a structural check - we'll verify DB existence later
  if (!is.null(edge_data) && nrow(edge_data) > 0) {
    if (is.null(object_data) || nrow(object_data) == 0) {
      # No objects in bundle - all edge references must exist in DB
      # We'll check this during DB validation phase
    }
  }

  if (length(errors) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Pre-flight validation failed:\n", paste("  -", errors, collapse = "\n")),
      people_data = NULL,
      workflow_data = NULL,
      object_data = NULL
    ))
  }

  list(
    valid = TRUE,
    message = "Pre-flight validation passed",
    people_data = people_data,
    workflow_data = workflow_data,
    object_data = object_data
  )
}


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
#' @param default_user Optional character scalar. If provided, will be used to
#'   fill any empty \code{created_by} fields in workflows and objects. If not
#'   provided and empty \code{created_by} fields are found, an error is raised.
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
#'   \item Loads the Excel workbook
#'   \item Performs pre-flight validation (duplicates, required fields, created_by)
#'   \item Creates a database backup (unless \code{backup = FALSE} or \code{validate_only = TRUE})
#'   \item Begins a database transaction
#'   \item Validates and ingests each sheet (workflows, objects, edges)
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
#' # Insert validated data with default user
#' results <- read_bundle("my_data.xlsx", default_user = "jkimbrel")
#' results$results$objects  # Check what was inserted
#' }
#'
#' @export

read_bundle <- function(bundle_path,
                        db_path = NULL,
                        backup = TRUE,
                        validate_only = FALSE,
                        default_user = NULL,
                        ...) {

  db_path <- gopher_db_path(db_path)

  if (!file.exists(bundle_path)) {
    cli::cli_abort("Bundle file does not exist: {.path {bundle_path}}")
  }

  # Load workbook
  wb <- openxlsx::loadWorkbook(bundle_path)
  sheet_names <- names(wb)

  cli::cli_alert_info("Processing bundle: {.path {basename(bundle_path)}}")
  cli::cli_alert_info("Available sheets: {.val {sheet_names}}")

  # Pre-flight validation (before backup)
  preflight_result <- preflight_validate_bundle(wb, default_user = default_user)
  if (!preflight_result$valid) {
    cli::cli_abort(preflight_result$message)
  }

  # Get potentially modified data from preflight (with filled default_user)
  preflight_people_data <- preflight_result$people_data
  preflight_workflow_data <- preflight_result$workflow_data
  preflight_object_data <- preflight_result$object_data

  # Create backup before any modifications
  backup_info <- NULL
  if (isTRUE(backup) && !isTRUE(validate_only)) {
    backup_info <- backup_db(db_path = db_path)
  }

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

          # Track auto-created people for warning at end
          auto_created_people <- character()

          # Process people sheet first (must come before workflows/objects that reference them)
          if ("people" %in% sheet_names) {
            people_result <- ingest_people_with_con(wb, con,
                                                     validate_only = validate_only,
                                                     preflight_data = preflight_people_data)
            results$people <- people_result
          } else {
            results$people <- NULL
          }

          # Process workflow sheet (must come before edges/objects that reference it)
          if ("workflow" %in% sheet_names) {
            workflow_result <- ingest_workflows_with_con(wb, con,
                                                          validate_only = validate_only,
                                                          preflight_data = preflight_workflow_data)
            results$workflows <- workflow_result$results
            auto_created_people <- c(auto_created_people, workflow_result$auto_created_people)
          } else {
            results$workflows <- NULL
          }

          # Process object sheet
          if ("object" %in% sheet_names) {
            obj_result <- ingest_objects_with_con(wb, con,
                                                   validate_only = validate_only,
                                                   preflight_data = preflight_object_data)
            results$objects <- obj_result
            auto_created_people <- c(auto_created_people, obj_result$auto_created_people)
          } else {
            cli::cli_alert_warning("No 'object' sheet found in bundle.")
            results$objects <- NULL
          }

          # Process edge sheet (after objects so they can reference new objects)
          if ("edge" %in% sheet_names) {
            results$edges <- ingest_edges_with_con(wb, con, validate_only = validate_only)
          } else {
            results$edges <- NULL
          }

          # Commit transaction if not validate_only
          if (!isTRUE(validate_only)) {
            DBI::dbCommit(con)
            cli::cli_alert_success("All changes committed to database.")
          }

          # Add auto-created people list to results
          results$auto_created_people <- auto_created_people

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

  # Warn about auto-created people
  if (!is.null(ingestion_result$auto_created_people) &&
      length(ingestion_result$auto_created_people) > 0) {
    cli::cli_alert_warning(
      "Auto-created {length(ingestion_result$auto_created_people)} person/people with minimal info: {.val {ingestion_result$auto_created_people}}"
    )
    cli::cli_alert_info("Please update their full_name and email in the people table.")
  }

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

ingest_objects_with_con <- function(wb, con, validate_only = FALSE, preflight_data = NULL) {

  # Use preflight data if provided, otherwise read from workbook
  if (!is.null(preflight_data)) {
    object_data <- preflight_data
  } else {
    object_data <- openxlsx::read.xlsx(wb, sheet = "object")
  }

  if (is.null(object_data) || nrow(object_data) == 0) {
    cli::cli_alert_warning("Object sheet is empty. Skipping.")
    return(list(
      n_processed = 0,
      n_inserted = 0,
      by_type = list(),
      validation_passed = TRUE,
      auto_created_people = character()
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

  # Auto-create missing people if created_by is provided
  auto_created_people <- character()
  if ("created_by" %in% names(object_data)) {
    auto_created_people <- auto_create_people(object_data$created_by, con, validate_only)
  }

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
    validation_passed = TRUE,
    auto_created_people = auto_created_people
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

  # Workflows summary
  if (!is.null(results$workflows)) {
    wf_res <- results$workflows

    if (wf_res$n_processed > 0) {
      if (isTRUE(validate_only)) {
        cli::cli_alert_info("Workflows validated: {wf_res$n_processed}")
      } else {
        cli::cli_alert_success("Workflows inserted: {wf_res$n_inserted}")
      }
    }
  }

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

  # Edges summary
  if (!is.null(results$edges)) {
    edge_res <- results$edges

    if (edge_res$n_processed > 0) {
      if (isTRUE(validate_only)) {
        cli::cli_alert_info("Edges validated: {edge_res$n_processed}")
      } else {
        cli::cli_alert_success("Edges inserted: {edge_res$n_inserted}")
      }

      # Show breakdown by type
      if (length(edge_res$by_type) > 0) {
        type_summary <- purrr::map2_chr(
          names(edge_res$by_type),
          edge_res$by_type,
          ~ paste0(.y, " ", .x)
        )
        cli::cli_bullets(c("*" = paste(type_summary, collapse = ", ")))
      }
    }
  }

  invisible(NULL)
}


#' Ingest people from bundle worksheet (with existing connection)
#'
#' Reads people records from the 'people' sheet, validates them, and inserts
#' into the database. Only allows insertions, not updates. If person_id already
#' exists in the database, an error is raised.
#'
#' @param wb openxlsx workbook object
#' @param con A DBI connection to the database
#' @param validate_only Logical; if TRUE, validation only (no insertion)
#' @param preflight_data Optional data.frame from preflight validation
#'
#' @return List with results
#' @keywords internal
ingest_people_with_con <- function(wb, con, validate_only = FALSE, preflight_data = NULL) {

  # Use preflight data if provided, otherwise read from workbook
  if (!is.null(preflight_data)) {
    people_data <- preflight_data
  } else {
    people_data <- openxlsx::read.xlsx(wb, sheet = "people")
  }

  if (is.null(people_data) || nrow(people_data) == 0) {
    cli::cli_alert_warning("People sheet is empty. Skipping.")
    return(list(
      n_processed = 0,
      n_inserted = 0,
      validation_passed = TRUE
    ))
  }

  # Filter out empty rows
  people_data <- people_data |>
    dplyr::filter(!is.na(.data$person_id) & nzchar(.data$person_id))

  if (nrow(people_data) == 0) {
    cli::cli_alert_warning("People sheet has no valid person_id entries. Skipping.")
    return(list(
      n_processed = 0,
      n_inserted = 0,
      validation_passed = TRUE
    ))
  }

  cli::cli_alert_info("Found {nrow(people_data)} person/people in bundle.")

  # Validate
  validation_result <- validate_people_with_con(people_data, con)
  if (!validation_result$valid) {
    cli::cli_abort(validation_result$message)
  }

  # Insert people
  if (!isTRUE(validate_only)) {
    result <- insert_people_with_con(people_data, con)
    cli::cli_alert_success("Inserted {result$n_inserted} person/people into database.")
  } else {
    result <- list(n_inserted = 0)
  }

  list(
    n_processed = nrow(people_data),
    n_inserted = result$n_inserted,
    validation_passed = TRUE
  )
}


#' Validate people against database (with connection)
#'
#' Checks that person_ids are unique within bundle and don't already exist
#' in database.
#'
#' @param people_data Data frame of people to validate
#' @param con A DBI connection to the database
#'
#' @return List with valid (logical) and message (character) elements
#' @keywords internal
validate_people_with_con <- function(people_data, con) {

  errors <- character()

  # Check for duplicates within bundle
  dup_ids <- people_data$person_id[duplicated(people_data$person_id)]
  if (length(dup_ids) > 0) {
    errors <- c(errors, sprintf("Duplicate person_ids in bundle: %s",
                                 paste(dup_ids, collapse = ", ")))
  }

  # Check for existing person_ids in database
  existing_people <- DBI::dbReadTable(con, "people")
  if (nrow(existing_people) > 0) {
    existing_ids <- intersect(people_data$person_id, existing_people$person_id)
    if (length(existing_ids) > 0) {
      errors <- c(errors, sprintf("Person IDs already exist in database: %s",
                                   paste(existing_ids, collapse = ", ")))
    }
  }

  if (length(errors) > 0) {
    return(list(valid = FALSE, message = paste(errors, collapse = "\n")))
  }

  list(valid = TRUE, message = "People validation passed")
}


#' Insert people into database (with connection)
#'
#' Inserts validated people records into the people table.
#'
#' @param people_data Data frame of people to insert
#' @param con A DBI connection to the database
#'
#' @return List with n_inserted count
#' @keywords internal
insert_people_with_con <- function(people_data, con) {

  # Select only columns that exist in the people table
  db_cols <- DBI::dbListFields(con, "people")

  # Exclude created_at (auto-generated by DB) and is_active (we set it explicitly)
  db_cols <- db_cols[!db_cols %in% c("created_at", "is_active")]

  insert_cols <- intersect(names(people_data), db_cols)
  insert_data <- people_data[, insert_cols, drop = FALSE]

  # Always set is_active = 1 for new people
  insert_data$is_active <- 1L

  DBI::dbWriteTable(con, "people", insert_data, append = TRUE, row.names = FALSE)

  list(n_inserted = nrow(insert_data))
}


#' Ingest workflows from bundle worksheet (with existing connection)
#'
#' Reads the workflow sheet from an Excel bundle, validates the data, and
#' optionally inserts it into the database using an existing connection.
#'
#' @param wb An openxlsx workbook object.
#' @param con A DBI connection to the database.
#' @param validate_only Logical. If TRUE, only validates without inserting.
#'
#' @return List with results and auto_created_people.
#' @keywords internal
#' @importFrom rlang .data

ingest_workflows_with_con <- function(wb, con, validate_only = FALSE, preflight_data = NULL) {

  # Use preflight data if provided, otherwise read from workbook
  if (!is.null(preflight_data)) {
    workflow_data <- preflight_data
  } else {
    workflow_data <- openxlsx::read.xlsx(wb, sheet = "workflow")
  }

  if (is.null(workflow_data) || nrow(workflow_data) == 0) {
    cli::cli_alert_warning("Workflow sheet is empty. Skipping.")
    return(list(
      results = list(
        n_processed = 0,
        n_inserted = 0,
        validation_passed = TRUE
      ),
      auto_created_people = character()
    ))
  }

  # Filter out empty rows (where description is NA/empty)
  workflow_data <- workflow_data |>
    dplyr::filter(
      !is.na(.data$workflow_id),
      nzchar(.data$workflow_id),
      !is.na(.data$description),
      nzchar(.data$description)
    )

  if (nrow(workflow_data) == 0) {
    cli::cli_alert_warning("Workflow sheet has no rows with both workflow_id and description. Skipping.")
    return(list(
      results = list(
        n_processed = 0,
        n_inserted = 0,
        validation_passed = TRUE
      ),
      auto_created_people = character()
    ))
  }

  cli::cli_alert_info("Found {nrow(workflow_data)} workflow(s) in bundle.")

  # Validate required columns
  required_cols <- c("workflow_id", "description")
  missing_cols <- setdiff(required_cols, names(workflow_data))

  if (length(missing_cols) > 0) {
    cli::cli_abort("Workflow sheet is missing required columns: {.val {missing_cols}}")
  }

  # Auto-create missing people if created_by is provided
  auto_created_people <- character()
  if ("created_by" %in% names(workflow_data)) {
    auto_created_people <- auto_create_people(workflow_data$created_by, con, validate_only)
  }

  # Validate against database specs
  validation_results <- validate_workflows_with_con(workflow_data, con)

  if (!validation_results$valid) {
    cli::cli_abort(c(
      "Workflow validation failed:",
      "x" = validation_results$message
    ))
  }

  cli::cli_alert_success("Workflow validation passed.")

  # Insert if not validate_only
  n_inserted <- 0
  if (!isTRUE(validate_only)) {
    n_inserted <- insert_workflows_with_con(workflow_data, con)
    cli::cli_alert_success("Inserted {nrow(workflow_data)} workflow(s) into database.")
  } else {
    cli::cli_alert_info("Validation only mode - no data inserted.")
  }

  list(
    results = list(
      n_processed = nrow(workflow_data),
      n_inserted = n_inserted,
      validation_passed = TRUE
    ),
    auto_created_people = auto_created_people
  )
}


#' Auto-create missing people
#'
#' Checks if person_ids exist in the people table and creates minimal records
#' for any that are missing.
#'
#' @param person_ids Character vector of person IDs to check/create.
#' @param con A DBI connection to the database.
#' @param validate_only Logical. If TRUE, doesn't actually insert.
#'
#' @return Character vector of auto-created person IDs.
#' @keywords internal

auto_create_people <- function(person_ids, con, validate_only = FALSE) {

  # Remove NAs
  person_ids <- person_ids[!is.na(person_ids) & nzchar(person_ids)]

  if (length(person_ids) == 0) {
    return(character())
  }

  # Get existing people
  existing_people <- DBI::dbReadTable(con, "people")

  # Find missing people
  if (nrow(existing_people) > 0) {
    missing_people <- setdiff(person_ids, existing_people$person_id)
  } else {
    missing_people <- unique(person_ids)
  }

  if (length(missing_people) == 0) {
    return(character())
  }

  # Create records for missing people
  if (!isTRUE(validate_only)) {

    # Prompt for email if interactive
    if (interactive()) {
      new_people_list <- list()

      for (person in missing_people) {
        message(sprintf("\nNew user '%s' detected in bundle.", person))
        email <- readline(prompt = sprintf("Enter email address for '%s': ", person))

        # Check if email already exists
        email_check <- DBI::dbGetQuery(
          con,
          "SELECT person_id FROM people WHERE email = ?",
          params = list(email)
        )

        if (nrow(email_check) > 0) {
          existing_id <- email_check$person_id[1]
          warning(sprintf(
            "Email '%s' already exists for user '%s'. Consider using '%s' instead of '%s' in your bundle to avoid duplicate people.",
            email, existing_id, existing_id, person
          ))
          proceed <- readline(prompt = "Continue anyway? (y/n): ")
          if (!tolower(proceed) %in% c("y", "yes")) {
            stop("Ingestion cancelled. Please update person_id in your bundle and try again.", call. = FALSE)
          }
        }

        new_people_list[[person]] <- data.frame(
          person_id = person,
          full_name = NA_character_,
          email = email,
          is_active = 1L,
          successor_person_id = NA_character_,
          stringsAsFactors = FALSE
        )
      }

      new_people <- dplyr::bind_rows(new_people_list)

    } else {
      # Non-interactive: create with NA email
      new_people <- data.frame(
        person_id = missing_people,
        full_name = NA_character_,
        email = NA_character_,
        is_active = 1L,
        successor_person_id = NA_character_,
        stringsAsFactors = FALSE
      )
    }

    DBI::dbWriteTable(con, "people", new_people, append = TRUE, row.names = FALSE)
  }

  missing_people
}


#' Validate workflows against database specifications (with connection)
#'
#' Checks that workflow_ids are unique and created_by references valid people.
#'
#' @param workflow_data Data frame of workflows to validate.
#' @param con A DBI connection to the database.
#'
#' @return List with valid (logical) and message (character) elements.
#' @keywords internal
#' @importFrom rlang .data

validate_workflows_with_con <- function(workflow_data, con) {

  # Check for duplicate workflow_ids in bundle
  dup_ids <- workflow_data |>
    dplyr::group_by(.data$workflow_id) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::pull(.data$workflow_id) |>
    unique()

  if (length(dup_ids) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Duplicate workflow_ids in bundle:", paste(dup_ids, collapse = ", "))
    ))
  }

  # Check for duplicates against existing database
  existing_workflows <- DBI::dbReadTable(con, "workflow")

  if (nrow(existing_workflows) > 0) {
    existing_ids <- intersect(workflow_data$workflow_id, existing_workflows$workflow_id)

    if (length(existing_ids) > 0) {
      return(list(
        valid = FALSE,
        message = paste("Workflow IDs already exist in database:",
                      paste(existing_ids, collapse = ", "))
      ))
    }
  }

  # Check created_by references (should all exist now due to auto-create)
  if ("created_by" %in% names(workflow_data)) {
    creator_ids <- workflow_data$created_by |>
      stats::na.omit() |>
      as.character() |>
      unique()

    if (length(creator_ids) > 0) {
      people <- DBI::dbReadTable(con, "people")

      if (nrow(people) > 0) {
        missing_creators <- setdiff(creator_ids, people$person_id)

        if (length(missing_creators) > 0) {
          return(list(
            valid = FALSE,
            message = paste("Person IDs not found:", paste(missing_creators, collapse = ", "))
          ))
        }
      } else {
        return(list(
          valid = FALSE,
          message = "created_by references people but no people in database"
        ))
      }
    }
  }

  list(valid = TRUE, message = "All validations passed")
}


#' Insert workflows into the database (with connection)
#'
#' Inserts validated workflow data into the workflow table using an existing connection.
#'
#' @param workflow_data Data frame of validated workflows.
#' @param con A DBI connection to the database.
#'
#' @return Invisibly returns the number of rows inserted.
#' @keywords internal
#' @importFrom rlang .data

insert_workflows_with_con <- function(workflow_data, con) {

  # Prepare data for insertion
  insert_data <- workflow_data |>
    dplyr::select(
      .data$workflow_id,
      dplyr::any_of(c("description", "created_by", "workflow_date"))
    )

  # Ensure optional columns exist
  if (!"created_by" %in% names(insert_data)) {
    insert_data$created_by <- NA_character_
  }
  if (!"workflow_date" %in% names(insert_data)) {
    insert_data$workflow_date <- NA_character_
  }

  # Insert into database
  DBI::dbWriteTable(
    con,
    "workflow",
    insert_data,
    append = TRUE,
    row.names = FALSE
  )

  invisible(nrow(insert_data))
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


#' Ingest edges from bundle worksheet (with existing connection)
#'
#' Reads the edge sheet from an Excel bundle, validates the data, and
#' optionally inserts it into the database using an existing connection.
#'
#' @param wb An openxlsx workbook object.
#' @param con A DBI connection to the database.
#' @param validate_only Logical. If TRUE, only validates without inserting.
#'
#' @return List with ingestion results.
#' @keywords internal
#' @importFrom rlang .data

ingest_edges_with_con <- function(wb, con, validate_only = FALSE) {

  # Read the edge sheet
  edge_data <- openxlsx::read.xlsx(wb, sheet = "edge")

  if (is.null(edge_data) || nrow(edge_data) == 0) {
    cli::cli_alert_warning("Edge sheet is empty. Skipping.")
    return(list(
      n_processed = 0,
      n_inserted = 0,
      by_type = list(),
      validation_passed = TRUE
    ))
  }

  cli::cli_alert_info("Found {nrow(edge_data)} edge(s) in bundle.")

  # Validate required columns
  required_cols <- c("parent_id", "child_id", "edge_type")
  missing_cols <- setdiff(required_cols, names(edge_data))

  if (length(missing_cols) > 0) {
    cli::cli_abort("Edge sheet is missing required columns: {.val {missing_cols}}")
  }

  # Validate against database specs
  validation_results <- validate_edges_with_con(edge_data, con)

  if (!validation_results$valid) {
    cli::cli_abort(c(
      "Edge validation failed:",
      "x" = validation_results$message
    ))
  }

  cli::cli_alert_success("Edge validation passed.")

  # Count by edge type
  type_counts <- edge_data |>
    dplyr::count(.data$edge_type, name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n))

  by_type <- as.list(stats::setNames(type_counts$n, type_counts$edge_type))

  # Insert if not validate_only
  n_inserted <- 0
  if (!isTRUE(validate_only)) {
    n_inserted <- insert_edges_with_con(edge_data, con)
    cli::cli_alert_success("Inserted {nrow(edge_data)} edge(s) into database.")
  } else {
    cli::cli_alert_info("Validation only mode - no data inserted.")
  }

  list(
    n_processed = nrow(edge_data),
    n_inserted = n_inserted,
    by_type = by_type,
    validation_passed = TRUE
  )
}


#' Validate edges against database specifications (with connection)
#'
#' Checks that parent/child IDs exist, edge types are valid, and type
#' combinations match edge_spec.
#'
#' @param edge_data Data frame of edges to validate.
#' @param con A DBI connection to the database.
#'
#' @return List with valid (logical) and message (character) elements.
#' @keywords internal
#' @importFrom rlang .data

validate_edges_with_con <- function(edge_data, con) {

  # Get valid edge types
  valid_edge_types <- DBI::dbReadTable(con, "edge_spec") |>
    dplyr::pull(.data$edge_type) |>
    unique()

  # Check edge_type validity
  invalid_types <- setdiff(edge_data$edge_type, valid_edge_types)

  if (length(invalid_types) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Invalid edge types:", paste(invalid_types, collapse = ", "))
    ))
  }

  # Get all objects (existing + being inserted in this transaction)
  all_objects <- DBI::dbReadTable(con, "object")

  if (nrow(all_objects) == 0) {
    return(list(
      valid = FALSE,
      message = "No objects found in database. Cannot create edges without objects."
    ))
  }

  # Check parent_id validity
  missing_parents <- setdiff(edge_data$parent_id, all_objects$object_id)

  if (length(missing_parents) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Parent IDs not found:", paste(missing_parents, collapse = ", "))
    ))
  }

  # Check child_id validity
  missing_children <- setdiff(edge_data$child_id, all_objects$object_id)

  if (length(missing_children) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Child IDs not found:", paste(missing_children, collapse = ", "))
    ))
  }

  # Get edge_spec with object type constraints
  edge_spec <- DBI::dbReadTable(con, "edge_spec")

  # For each edge, verify (parent_type, child_type, edge_type) is in edge_spec
  edges_with_types <- edge_data |>
    dplyr::left_join(
      all_objects |> dplyr::select(.data$object_id, parent_type = .data$object_type),
      by = c("parent_id" = "object_id")
    ) |>
    dplyr::left_join(
      all_objects |> dplyr::select(.data$object_id, child_type = .data$object_type),
      by = c("child_id" = "object_id")
    )

  # Check for invalid combinations
  invalid_combos <- edges_with_types |>
    dplyr::anti_join(
      edge_spec |> dplyr::select(.data$parent_type, .data$child_type, .data$edge_type),
      by = c("parent_type", "child_type", "edge_type")
    )

  if (nrow(invalid_combos) > 0) {
    first_invalid <- invalid_combos |> dplyr::slice(1)
    return(list(
      valid = FALSE,
      message = sprintf(
        "Invalid edge combination: '%s' (%s) -[%s]-> '%s' (%s) not allowed by edge_spec",
        first_invalid$parent_id,
        first_invalid$parent_type,
        first_invalid$edge_type,
        first_invalid$child_id,
        first_invalid$child_type
      )
    ))
  }

  # Check for duplicate edges in bundle
  dup_edges <- edge_data |>
    dplyr::group_by(.data$parent_id, .data$child_id, .data$edge_type) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup() |>
    dplyr::distinct(.data$parent_id, .data$child_id, .data$edge_type)

  if (nrow(dup_edges) > 0) {
    first_dup <- dup_edges |> dplyr::slice(1)
    return(list(
      valid = FALSE,
      message = sprintf(
        "Duplicate edges in bundle: %s -[%s]-> %s",
        first_dup$parent_id,
        first_dup$edge_type,
        first_dup$child_id
      )
    ))
  }

  # Check for duplicates against existing database
  existing_edges <- DBI::dbReadTable(con, "edge")

  if (nrow(existing_edges) > 0) {
    duplicate_edges <- edge_data |>
      dplyr::inner_join(
        existing_edges |> dplyr::select(.data$parent_id, .data$child_id, .data$edge_type),
        by = c("parent_id", "child_id", "edge_type")
      )

    if (nrow(duplicate_edges) > 0) {
      first_dup <- duplicate_edges |> dplyr::slice(1)
      return(list(
        valid = FALSE,
        message = sprintf(
          "Edge already exists in database: %s -[%s]-> %s",
          first_dup$parent_id,
          first_dup$edge_type,
          first_dup$child_id
        )
      ))
    }
  }

  # Check workflow_id if provided
  if ("workflow_id" %in% names(edge_data)) {
    workflow_ids <- edge_data$workflow_id |>
      stats::na.omit() |>
      as.character() |>
      unique()

    if (length(workflow_ids) > 0) {
      workflows <- DBI::dbReadTable(con, "workflow")

      if (nrow(workflows) > 0) {
        missing_workflows <- setdiff(workflow_ids, workflows$workflow_id)

        if (length(missing_workflows) > 0) {
          return(list(
            valid = FALSE,
            message = paste("Workflow IDs not found:", paste(missing_workflows, collapse = ", "))
          ))
        }
      } else if (length(workflow_ids) > 0) {
        return(list(
          valid = FALSE,
          message = "Workflow IDs referenced but no workflows in database"
        ))
      }
    }
  }

  list(valid = TRUE, message = "All validations passed")
}


#' Insert edges into the database (with connection)
#'
#' Inserts validated edge data into the edge table using an existing connection.
#'
#' @param edge_data Data frame of validated edges.
#' @param con A DBI connection to the database.
#'
#' @return Invisibly returns the number of rows inserted.
#' @keywords internal
#' @importFrom rlang .data

insert_edges_with_con <- function(edge_data, con) {

  # Prepare data for insertion
  insert_data <- edge_data |>
    dplyr::select(
      .data$parent_id,
      .data$child_id,
      .data$edge_type,
      dplyr::any_of("workflow_id")
    )

  # Ensure workflow_id column exists (can be NA)
  if (!"workflow_id" %in% names(insert_data)) {
    insert_data$workflow_id <- NA_character_
  }

  # Insert into database
  DBI::dbWriteTable(
    con,
    "edge",
    insert_data,
    append = TRUE,
    row.names = FALSE
  )

  invisible(nrow(insert_data))
}
