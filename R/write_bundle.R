#' Write a gopheR Excel bundle template
#'
#' Creates an Excel workbook for data ingestion into a gopheR database.
#' The workbook contains one or more data-entry sheets (e.g., \code{object})
#' and a hidden \code{spec} sheet used to populate dropdowns.
#'
#' @param out_xlsx Character scalar. Path to the output Excel file.
#' @param db_path Optional character scalar. Full path to a gopheR SQLite database.
#'   If \code{NULL}, the path is resolved via \code{gopher_db_path()}.
#' @param overwrite Logical. Whether to overwrite an existing file.
#' @param hide_spec Logical. If \code{TRUE}, hides the internal \code{spec} sheet.
#' @param open_bundle Logical. If \code{TRUE}, opens the workbook after creation.
#' @param people_sheet Logical. If \code{TRUE}, includes a \code{people} sheet
#'   for adding new people records. The sheet will have headers only; users can
#'   add as many rows as needed.
#'
#' @return Invisibly returns the path to the created Excel file.
#'
#' @details
#' The \code{spec} worksheet stores allowed values used for dropdown validation
#' in the data-entry sheets. Users typically do not need to view or edit this sheet.
#'
#' When \code{people_sheet = TRUE}, a people sheet is included with column headers
#' only. Users can add new people records. Note that \code{read_bundle()} will only
#' \strong{insert} new people, not update existing ones. If a person_id already
#' exists in the database, an error will be raised.
#'
#' @export
#' @importFrom utils browseURL

write_bundle <- function(out_xlsx,
                         db_path = NULL,
                         overwrite = TRUE,
                         hide_spec = TRUE,
                         open_bundle = FALSE,
                         people_sheet = FALSE) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required.")
  }

  # workbook
  wb <- openxlsx::createWorkbook()
  header_style <- bundle_header_style()

  # spec - logic for hiding spec sheet is at the bottom
  openxlsx::addWorksheet(wb, "spec")


  # people logic (optional, should be first sheet if requested)
  if (isTRUE(people_sheet)) {
    people_cols <- get_table_columns(
      "people",
      exclude_cols = c("created_at", "is_active")
    )

    add_bundle_sheet(
      wb = wb,
      sheet = "people",
      cols = people_cols,
      header_style = header_style
    )

    # Add comment explaining that new people default to active
    person_id_col <- match("person_id", people_cols)
    if (!is.na(person_id_col)) {
      add_cell_comment(
        wb = wb,
        sheet = "people",
        row = 1,
        col = person_id_col,
        comment = "Add new people here. They will be set as active (is_active=1) by default when inserted."
      )
    }
  }


  # workflow logic
  workflow_cols <- get_table_columns(
    "workflow",
    exclude_cols = c("created_at")
  )

  add_bundle_sheet(
    wb = wb,
    sheet = "workflow",
    cols = workflow_cols,
    header_style = header_style
  )

  suggested_workflow_ids <- get_next_workflow_ids(
    n = 10,
    db_path = db_path
  )

  prefill_bundle_column(
    wb = wb,
    sheet = "workflow",
    cols = workflow_cols,
    col_name = "workflow_id",
    values = suggested_workflow_ids
  )

  ## add workflow comment
  workflow_id_col <- match("workflow_id", workflow_cols)

  add_cell_comment(
    wb = wb,
    sheet = "workflow",
    row = 1,
    col = workflow_id_col,
    comment = "Suggested workflow IDs are prefilled below. You may use, increment or replace them. IDs must be unique."
  )


  # object logic
  object_cols <- get_table_columns(
    "object",
    exclude_cols = c("created_at", "object_subtype")
  )

  allowed_object_types <- get_object_types()

  add_bundle_sheet(
    wb = wb,
    sheet = "object",
    cols = object_cols,
    header_style = header_style
  )

  ## object spec
  add_spec_dropdown(
    wb = wb,
    target_sheet = "object",
    target_cols = object_cols,
    target_col_name = "object_type",
    spec_values = allowed_object_types,
    spec_name = "object_type"
  )


  # edge logic
  edge_cols <- get_table_columns(
    "edge",
    exclude_cols = c("created_at")
  )

  allowed_edge_types <- get_edge_types()

  add_bundle_sheet(
    wb = wb,
    sheet = "edge",
    cols = edge_cols,
    header_style = header_style
  )

  add_spec_dropdown(
    wb = wb,
    target_sheet = "edge",
    target_cols = edge_cols,
    target_col_name = "edge_type",
    spec_values = allowed_edge_types,
    spec_name = "edge_type"
  )

  add_sheet_dropdown(
    wb = wb,
    target_sheet = "edge",
    target_cols = edge_cols,
    target_col_name = "workflow_id",
    source_sheet = "workflow",
    source_col_name = "workflow_id",
    source_cols = workflow_cols
  )



  # result logic
  result_cols <- get_table_columns(
    "result",
    exclude_cols = c("result_id", "created_at")
  )

  allowed_result_types <- get_result_types()

  add_bundle_sheet(
    wb = wb,
    sheet = "result",
    cols = result_cols,
    header_style = header_style
  )

  add_spec_dropdown(
    wb = wb,
    target_sheet = "result",
    target_cols = result_cols,
    target_col_name = "key",
    spec_values = allowed_result_types,
    spec_name = "key"
  )

  add_sheet_dropdown(
    wb = wb,
    target_sheet = "result",
    target_cols = result_cols,
    target_col_name = "workflow_id",
    source_sheet = "workflow",
    source_col_name = "workflow_id",
    source_cols = workflow_cols
  )

  # file logic
  object_file_cols <- get_table_columns(
    "object_file",
    exclude_cols = c("object_file_id", "created_at")
  )

  allowed_object_file_types <- get_object_file_types()

  add_bundle_sheet(
    wb = wb,
    sheet = "object_file",
    cols = object_file_cols,
    header_style = header_style
  )

  add_spec_dropdown(
    wb = wb,
    target_sheet = "object_file",
    target_cols = object_file_cols,
    target_col_name = "file_role",
    spec_values = allowed_object_file_types,
    spec_name = "file_role"
  )

  add_sheet_dropdown(
    wb = wb,
    target_sheet = "object_file",
    target_cols = object_file_cols,
    target_col_name = "workflow_id",
    source_sheet = "workflow",
    source_col_name = "workflow_id",
    source_cols = workflow_cols
  )


  # hide spec sheet
  # Set active sheet to first visible sheet (people if it exists, otherwise workflow)
  if (isTRUE(people_sheet)) {
    openxlsx::activeSheet(wb) <- "people"
  } else {
    openxlsx::activeSheet(wb) <- "workflow"
  }
  if (isTRUE(hide_spec)) {
    hide_sheet(wb, "spec")
  }



  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = overwrite)

  if (isTRUE(open_bundle)) {
    browseURL(out_xlsx)
  }

  invisible(out_xlsx)
}



#' Get column names from a database table
#'
#' Internal helper that returns the column names for a table in a gopheR
#' database, optionally excluding selected columns.
#'
#' @param table Character scalar. Name of the database table.
#' @param db_path Optional character scalar. Full path to a gopheR SQLite
#'   database. If \code{NULL}, the path is resolved via
#'   \code{gopher_db_path()}.
#' @param read_only Logical. Whether to open the database in read-only mode.
#' @param exclude_cols Character vector of column names to exclude from the
#'   returned result.
#'
#' @return Character vector of column names.
#' @keywords internal

get_table_columns <- function(table,
                              db_path = NULL,
                              read_only = TRUE,
                              exclude_cols = character()) {
  with_gopher_con(
    .f = function(con) {
      tables <- DBI::dbListTables(con)

      if (!table %in% tables) {
        cli::cli_abort("Table `{table}` not found in database.")
      }

      cols <- DBI::dbListFields(con, table)
      cols[!cols %in% exclude_cols]
    },
    db_path = db_path,
    read_only = read_only
  )
}





#' Get object type labels for bundle dropdowns
#'
#' Internal helper that returns a sorted vector of object labels for use in
#' Excel bundle dropdowns. Labels include both base object types and combined
#' \code{object_type:object_subtype} values.
#'
#' @param db_path Optional character scalar. Full path to a gopheR SQLite
#'   database. If \code{NULL}, the path is resolved via
#'   \code{gopher_db_path()}.
#' @param read_only Logical. Whether to open the database in read-only mode.
#'
#' @return Character vector of object labels, including object types and
#'   \code{object_type:object_subtype} combinations.
#'
#' @details
#' This helper is used to populate a single dropdown in the \code{object}
#' worksheet. During ingestion, combined labels can be split back into
#' \code{object_type} and \code{object_subtype}.
#'
#' @keywords internal
#' @importFrom rlang .data

get_object_types <- function(db_path = NULL,
                             read_only = TRUE) {
  with_gopher_con(
    .f = function(con) {

      tables <- DBI::dbListTables(con)

      if (!"object_type" %in% tables) {
        cli::cli_abort("Table {.val object_type} not found in database.")
      }

      if (!"object_subtype" %in% tables) {
        cli::cli_abort("Table {.val object_subtype} not found in database.")
      }

      object_type_ref <- DBI::dbReadTable(con, "object_type")
      object_subtype_ref <- DBI::dbReadTable(con, "object_subtype")

      if (!"object_type" %in% names(object_type_ref)) {
        cli::cli_abort("Table {.val object_type} must contain column {.val object_type}.")
      }

      required_subtype_cols <- c("object_type", "object_subtype")
      missing_cols <- setdiff(required_subtype_cols, names(object_subtype_ref))

      if (length(missing_cols) > 0) {
        cli::cli_abort(
          "Table {.val object_subtype} is missing required columns: {.val {missing_cols}}."
        )
      }

      objects <- object_type_ref |>
        dplyr::pull(.data$object_type)

      subtypes <- object_subtype_ref |>
        dplyr::filter(!is.na(.data$object_subtype), nzchar(.data$object_subtype)) |>
        dplyr::mutate(
          object_label = paste(.data$object_type, .data$object_subtype, sep = ":")
        ) |>
        dplyr::pull(.data$object_label)

      sort(unique(c(objects, subtypes)))
    },
    db_path = db_path,
    read_only = read_only
  )
}





#' Get allowed edge types from the database
#'
#' Returns unique edge types defined in `edge_spec`.
#'
#' @param db_path Optional path to the database.
#' @param read_only Logical; open connection in read-only mode.
#'
#' @return Character vector of edge types.
#' @keywords internal

get_edge_types <- function(db_path = NULL,
                           read_only = TRUE) {
  get_spec_values(
    table = "edge_spec",
    column = "edge_type",
    db_path = db_path,
    read_only = read_only
  )
}


#' Get allowed result keys from the database
#'
#' Returns unique result keys defined in `key_spec`.
#'
#' @param db_path Optional path to the database.
#' @param read_only Logical; open connection in read-only mode.
#'
#' @return Character vector of result keys.
#' @keywords internal

get_result_types <- function(db_path = NULL,
                             read_only = TRUE) {
  get_spec_values(
    table = "key_spec",
    column = "key",
    db_path = db_path,
    read_only = read_only
  )
}




#' Get allowed object file types from the database
#'
#' Returns unique file roles defined in `object_file_type_spec`.
#'
#' @param db_path Optional path to the database.
#' @param read_only Logical; open connection in read-only mode.
#'
#' @return Character vector of file roles.
#' @keywords internal

get_object_file_types <- function(db_path = NULL,
                                  read_only = TRUE) {
  get_spec_values(
    table = "object_file_type_spec",
    column = "file_role",
    db_path = db_path,
    read_only = read_only
  )
}



#' Generate next workflow IDs
#'
#' Generates a sequence of workflow IDs based on existing IDs in the database,
#' using a prefix and zero-padded numeric suffix.
#'
#' @param n Number of workflow IDs to generate.
#' @param db_path Optional path to the database.
#' @param read_only Logical; open connection in read-only mode.
#' @param prefix Prefix for workflow IDs. Defaults to `"workflow_"`.
#' @param pad_width Width for zero-padding numeric suffix.
#'
#' @return Character vector of workflow IDs.
#' @keywords internal

get_next_workflow_ids <- function(n = 10,
                                  db_path = NULL,
                                  read_only = TRUE,
                                  prefix = "workflow_",
                                  pad_width = 4) {
  with_gopher_con(
    .f = function(con) {

      workflow_tbl <- DBI::dbReadTable(con, "workflow")

      if (!"workflow_id" %in% names(workflow_tbl) || nrow(workflow_tbl) == 0) {
        start_num <- 1L
      } else {
        ids <- workflow_tbl$workflow_id

        pattern <- paste0("^", prefix, "([0-9]+)$")
        matches <- grepl(pattern, ids)

        if (!any(matches)) {
          start_num <- 1L
        } else {
          nums <- sub(pattern, "\\1", ids[matches])
          nums <- suppressWarnings(as.integer(nums))
          nums <- nums[!is.na(nums)]

          start_num <- if (length(nums) == 0) 1L else max(nums) + 1L
        }
      }

      paste0(prefix, stringr::str_pad(start_num:(start_num + n - 1),
                                      width = pad_width,
                                      side = "left",
                                      pad = "0"))
    },
    db_path = db_path,
    read_only = read_only
  )
}




#' Get unique values from a specification table column
#'
#' Reads a table from the database and returns sorted unique values from a
#' specified column.
#'
#' @param table Name of the table to read.
#' @param column Name of the column to extract values from.
#' @param db_path Optional path to the database.
#' @param read_only Logical; open connection in read-only mode.
#'
#' @return Character vector of unique values.
#' @keywords internal

get_spec_values <- function(table,
                            column,
                            db_path = NULL,
                            read_only = TRUE) {
  with_gopher_con(
    .f = function(con) {
      DBI::dbReadTable(con, table) |>
        dplyr::pull(.data[[column]]) |>
        unique() |>
        sort()
    },
    db_path = db_path,
    read_only = read_only
  )
}
