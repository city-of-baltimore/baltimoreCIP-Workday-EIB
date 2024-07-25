#' Write data to one or more sheets in an EIB workbook
#'
#' [wb_save_eib_sheets()] writes a data frame or list to an Excel spreadsheet
#' based on an input template. The default parameters are set to work with the
#' a Workday Enterprise Interface Builder (EIB) spreadsheet template.
#'
#' @param data Data frame or list with data to write to an EIB Excel
#'   Spreadsheet. If data is a named list, the names of the list are used in
#'   place of any value supplied to `sheets`.
#' @inheritParams openxlsx2::wb_save
#' @inheritParams add_data_eib_sheet
#' @returns Invisibly returns the input data.
#' @importFrom openxlsx2 wb_load wb_save
#' @importFrom rlang is_named
#' @importFrom vctrs vec_check_size
wb_save_eib_sheets <- function(
    data,
    file,
    template,
    overwrite = FALSE,
    sheets = 1,
    start_row = 6,
    col_offset = 1,
    dt_version = TRUE,
    dt_format = "%Y-%m-%d_%I-%M-%S_%p") {
  wb_template <- openxlsx2::wb_load(template)

  if (!is.data.frame(data)) {
    if (rlang::is_named(data)) {
      # sheets is ignored if data is named
      sheets <- names(data)
    }

    vctrs::vec_check_size(
      data,
      size = length(sheets)
    )

    wb_update <- reduce_seq_len(
      # Use column index as vector
      .x = data,
      .f = \(wb, idx) {
        add_data_eib_sheet(
          data[[idx]],
          template = wb,
          sheet = sheets[[idx]],
          start_row = start_row,
          col_offset = col_offset
        )
      },
      .init = wb_template
    )
  } else {
    wb_update <- add_data_eib_sheet(
      data,
      template = wb_template,
      sheet = sheets,
      start_row = start_row,
      col_offset = col_offset
    )
  }

  if (dt_version) {
    file <- paste0(
      fs::path_ext_remove(file), "_",
      format(Sys.time(), dt_format), ".",
      fs::path_ext(file)
    )
  }

  openxlsx2::wb_save(
    wb_update,
    file = file,
    overwrite = overwrite
  )

  invisible(data)
}
