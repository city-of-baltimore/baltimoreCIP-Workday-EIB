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

#' Summarise CIP data by fiscal year
#'
#' [summarise_cip_data_fy()] wraps [dplyr::summarise()] to combine fiscal year
#' amount columns grouped by some other variables.
#'
#' @returns A data frame with FY columns summed by the column names passed to
#'   .by
summarise_cip_data_fy <- function(data,
                                  .by = NULL) {
  data |>
    summarise(
      across(
        starts_with("FY"),
        \(x) {
          sum(x, na.rm = TRUE)
        }
      ),
      .by = all_of(.by)
    )
}

#' Write a data frame to an Excel workbook
#'
#' [write_data_eib_sheet()] uses [openxlsx2::write_data()] to write a data frame
#' to a single sheet of an Excel workbook.
#'
#' @inheritParams openxlsx2::write_data
#' @inheritDotParams openxlsx2::wb_load -file
write_data_eib_sheet <- function(data,
                                 template,
                                 sheet = 1,
                                 ...,
                                 start_row = 6,
                                 col_offset = 1) {
  stopifnot(is.data.frame(data))

  if (is.character(template)) {
    template <- openxlsx2::wb_load(template, ...)
  }

  wb_fields <- load_eib_fields(
    template,
    sheet = sheet,
    start_row = start_row - 1
  )

  wb_fields <- wb_fields[!is.na(wb_fields)]

  data <- data |>
    select(any_of(wb_fields))

  nm <- names(data)

  reduce_seq_len(
    # Use column index as vector
    data,
    \(wb, y) {
      start_col <- match(nm[[y]], wb_fields) + col_offset

      openxlsx2::write_data(
        wb,
        # Subset each column
        x = data[[y]],
        col_names = FALSE,
        # Write to matching column in sheet
        sheet = sheet,
        start_col = start_col,
        start_row = start_row
      )
    },
    .init = template
  )
}

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
#' @inheritParams write_data_eib_sheet
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
        write_data_eib_sheet(
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
    wb_update <- write_data_eib_sheet(
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
