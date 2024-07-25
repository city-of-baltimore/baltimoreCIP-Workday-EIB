#' Write a data frame to an Excel workbook
#'
#' [add_data_eib_sheet()] uses the `add_data` method for the
#' [openxlsx2::wbWorkbook] class to write a data frame to a single sheet of an
#' Excel workbook.
#'
#' @inheritParams openxlsx2::wb_add_data
#' @inheritDotParams openxlsx2::wb_load -file
add_data_eib_sheet <- function(data,
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

      wb$add_data(
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
