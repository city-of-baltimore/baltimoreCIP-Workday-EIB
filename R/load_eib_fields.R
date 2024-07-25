#' Load fields from an EIB workbook
#'
#' In Workday Enterprise Interface Builder (EIB) spreadsheets, the field values
#' start in the 2nd column and the 5th row. These parameters can be modified but
#' typically should not be changed.
#'
#' @inheritParams openxlsx2::wb_read
#' @returns Character vector with field names
load_eib_fields <- function(file, ..., sheet = 1, start_row = 5, start_col = 2) {
  eib_wb <- openxlsx2::wb_read(
    file,
    sheet = sheet,
    start_row = start_row,
    start_col = start_col
  )

  names(eib_wb)
}
