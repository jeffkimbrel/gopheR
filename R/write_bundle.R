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
#'
#' @return Invisibly returns the path to the created Excel file.
#'
#' @details
#' The \code{spec} worksheet stores allowed values used for dropdown validation
#' in the data-entry sheets. Users typically do not need to view or edit this sheet.
#'
#' Additional sheets and validation rules will be added in future versions.
#'
#' @export
#' @importFrom utils browseURL

write_bundle <- function(out_xlsx,
                         db_path = NULL,
                         overwrite = TRUE,
                         hide_spec = TRUE,
                         open_bundle = FALSE) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required.")
  }

  # workbook
  wb <- openxlsx::createWorkbook()
  header_style <- bundle_header_style()

  # spec - logic for hiding spec sheet is at the bottom
  openxlsx::addWorksheet(wb, "spec")


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


  # hide spec sheet
  openxlsx::activeSheet(wb) <- "object" # spec is likely "active", so need to switch it
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
        dplyr::pull(.data$object_type) |>
        unique()

      subtypes <- object_subtype_ref |>
        dplyr::filter(!is.na(.data$object_subtype), nzchar(.data$object_subtype)) |>
        dplyr::mutate(
          object_label = paste(.data$object_type, .data$object_subtype, sep = ":")
        ) |>
        dplyr::pull(.data$object_label) |>
        unique()

      sort(unique(c(objects, subtypes)))
    },
    db_path = db_path,
    read_only = read_only
  )
}
