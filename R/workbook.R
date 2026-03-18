#' Create a standard header style for bundle worksheets
#'
#' Internal helper used when building Excel workbooks for data-entry bundles.
#'
#' @return An \code{openxlsx} style object.
#' @keywords internal

bundle_header_style <- function() {
  openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "bottom"
  )
}



#' Add a bundle data-entry sheet
#'
#' Internal helper that adds a worksheet, writes column headers, applies
#' header styling, and sets basic sheet formatting.
#'
#' @param wb An \code{openxlsx} workbook object.
#' @param sheet Character scalar. Name of the worksheet to add.
#' @param cols Character vector of column names to write as headers.
#' @param header_style Optional \code{openxlsx} style object for the header row.
#'
#' @return Invisibly returns the workbook object.
#' @keywords internal

add_bundle_sheet <- function(wb, sheet, cols, header_style = NULL) {
  openxlsx::addWorksheet(wb, sheet)

  header_df <- as.data.frame(matrix(cols, nrow = 1), stringsAsFactors = FALSE)

  openxlsx::writeData(
    wb,
    sheet = sheet,
    x = header_df,
    startRow = 1,
    startCol = 1,
    colNames = FALSE,
    rowNames = FALSE
  )

  if (!is.null(header_style)) {
    openxlsx::addStyle(
      wb,
      sheet = sheet,
      style = header_style,
      rows = 1,
      cols = seq_along(cols),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  openxlsx::addFilter(
    wb,
    sheet = sheet,
    rows = 1,
    cols = seq_along(cols)
  )

  openxlsx::freezePane(wb, sheet = sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet, cols = seq_along(cols), widths = "auto")

  invisible(wb)
}




#' Add a specification column to the spec worksheet
#'
#' Internal helper that writes a header and a vector of allowed values into a
#' column of the hidden \code{spec} worksheet.
#'
#' @param wb An \code{openxlsx} workbook object.
#' @param values Character vector of allowed values.
#' @param col Integer scalar. Target column index in the \code{spec} sheet.
#' @param name Character scalar. Header to write for this spec column.
#'
#' @return Invisibly returns the workbook object.
#' @keywords internal

add_spec_column <- function(wb, values, col = 1, name = "spec") {
  values <- unique(values)
  values <- values[!is.na(values)]
  values <- values[nzchar(values)]

  header_df <- data.frame(x = name, stringsAsFactors = FALSE)
  names(header_df) <- name

  openxlsx::writeData(
    wb,
    sheet = "spec",
    x = header_df,
    startRow = 1,
    startCol = col,
    colNames = TRUE,
    rowNames = FALSE
  )

  if (length(values) > 0) {
    value_df <- data.frame(x = values, stringsAsFactors = FALSE)
    names(value_df) <- name

    openxlsx::writeData(
      wb,
      sheet = "spec",
      x = value_df,
      startRow = 2,
      startCol = col,
      colNames = FALSE,
      rowNames = FALSE
    )
  }

  invisible(wb)
}



#' Add dropdown validation from the spec worksheet
#'
#' Internal helper that applies Excel data validation to a target column using
#' a range in the hidden \code{spec} worksheet.
#'
#' @param wb An \code{openxlsx} workbook object.
#' @param sheet Character scalar. Name of the worksheet receiving validation.
#' @param target_col Integer scalar. Target column index in \code{sheet}.
#' @param spec_col Integer scalar. Source column index in the \code{spec} sheet.
#' @param n_values Integer scalar. Number of allowed values in the spec range.
#' @param rows Integer vector of worksheet rows to which validation is applied.
#'
#' @return Invisibly returns the workbook object.
#' @keywords internal

add_dropdown_from_spec <- function(wb,
                                   sheet,
                                   target_col,
                                   spec_col,
                                   n_values,
                                   rows = 2:5000) {
  if (n_values < 1) {
    return(invisible(wb))
  }

  col_letter <- openxlsx::int2col(spec_col)
  last_row <- n_values + 1

  openxlsx::dataValidation(
    wb,
    sheet = sheet,
    cols = target_col,
    rows = rows,
    type = "list",
    value = sprintf("'spec'!$%s$2:$%s$%d", col_letter, col_letter, last_row)
  )

  invisible(wb)
}



#' Hide a worksheet in a workbook
#'
#' Internal helper used to hide support worksheets such as the \code{spec} sheet.
#'
#' @param wb An \code{openxlsx} workbook object.
#' @param sheet Character scalar. Name of the worksheet to hide.
#'
#' @return Invisibly returns the workbook object.
#' @keywords internal

hide_sheet <- function(wb, sheet) {
  sheet_names <- wb$sheet_names

  if (!sheet %in% sheet_names) {
    cli::cli_abort("Sheet {.val {sheet}} not found in workbook.")
  }

  vis <- openxlsx::sheetVisibility(wb)
  vis[sheet_names == sheet] <- "hidden"
  openxlsx::sheetVisibility(wb) <- vis

  invisible(wb)
}



#' Add a spec-backed dropdown to a bundle worksheet
#'
#' Internal helper that writes allowed values to the \code{spec} worksheet and
#' applies dropdown validation to a target column in a user-facing worksheet.
#'
#' @param wb An \code{openxlsx} workbook object.
#' @param target_sheet Character scalar. Name of the worksheet receiving validation.
#' @param target_cols Character vector of column names in the target worksheet.
#' @param target_col_name Character scalar. Name of the target column to validate.
#' @param spec_values Character vector of allowed values.
#' @param spec_name Character scalar. Header name to use in the \code{spec} sheet.
#' @param rows Integer vector of worksheet rows to which validation is applied.
#'
#' @return Invisibly returns the workbook object.
#' @keywords internal

add_spec_dropdown <- function(wb,
                              target_sheet,
                              target_cols,
                              target_col_name,
                              spec_values,
                              spec_name,
                              rows = 2:5000) {

  spec_col <- get_next_spec_col(wb)

  add_spec_column(
    wb = wb,
    values = spec_values,
    col = spec_col,
    name = spec_name
  )

  target_col <- match(target_col_name, target_cols)

  if (is.na(target_col)) {
    cli::cli_abort(
      "Internal error: column {.val {target_col_name}} not found in sheet {.val {target_sheet}}."
    )
  }

  add_dropdown_from_spec(
    wb = wb,
    sheet = target_sheet,
    target_col = target_col,
    spec_col = spec_col,
    n_values = length(spec_values),
    rows = rows
  )

  invisible(wb)
}


#' Get the next available column in the spec worksheet
#'
#' Internal helper that determines the next available column index in the
#' \code{spec} worksheet. This is used to place new specification values
#' (e.g., object types, edge types) without overwriting existing columns.
#'
#' @param wb An \code{openxlsx} workbook object.
#'
#' @return Integer scalar giving the next available column index in the
#'   \code{spec} worksheet.
#'
#' @details
#' The function inspects the internal worksheet structure to determine which
#' columns already contain data. If the \code{spec} sheet is empty, it returns 1.
#'
#' @keywords internal

get_next_spec_col <- function(wb) {
  spec_sheet_index <- match("spec", wb$sheet_names)

  if (is.na(spec_sheet_index)) {
    cli::cli_abort("Sheet {.val spec} not found in workbook.")
  }

  spec_sheet <- wb$worksheets[[spec_sheet_index]]

  if (length(spec_sheet$sheet_data$cols) == 0) {
    return(1)
  }

  max(spec_sheet$sheet_data$cols) + 1
}





prefill_bundle_column <- function(wb,
                                  sheet,
                                  cols,
                                  col_name,
                                  values,
                                  start_row = 2) {
  col_index <- match(col_name, cols)

  if (is.na(col_index)) {
    stop(sprintf("Column '%s' not found in sheet '%s'.", col_name, sheet))
  }

  openxlsx::writeData(
    wb = wb,
    sheet = sheet,
    x = data.frame(value = values),
    startCol = col_index,
    startRow = start_row,
    colNames = FALSE
  )

  invisible(wb)
}







add_sheet_dropdown <- function(wb,
                               target_sheet,
                               target_cols,
                               target_col_name,
                               source_sheet,
                               source_col_name,
                               source_cols,
                               target_rows = 2:1000,
                               source_rows = 2:1000) {
  target_col_index <- match(target_col_name, target_cols)
  source_col_index <- match(source_col_name, source_cols)

  if (is.na(target_col_index)) {
    stop(sprintf("Target column '%s' not found in sheet '%s'.",
                 target_col_name, target_sheet))
  }

  if (is.na(source_col_index)) {
    stop(sprintf("Source column '%s' not found in sheet '%s'.",
                 source_col_name, source_sheet))
  }

  source_col_letter <- openxlsx::int2col(source_col_index)

  source_range <- paste0(
    source_sheet, "!$",
    source_col_letter, "$", min(source_rows),
    ":$",
    source_col_letter, "$", max(source_rows)
  )

  openxlsx::dataValidation(
    wb = wb,
    sheet = target_sheet,
    cols = target_col_index,
    rows = target_rows,
    type = "list",
    value = source_range
  )

  invisible(wb)
}
