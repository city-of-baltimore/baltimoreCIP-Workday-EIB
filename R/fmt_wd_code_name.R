#' Format a pair of code and name columns from a Workday or Adaptive Planning report
fmt_wd_code_name <- function(data,
                             code_col,
                             name_col,
                             code_pattern = NULL,
                             new_code_col = code_col,
                             new_name_col = name_col) {
  data |>
    dplyr::mutate(
      "{new_code_col}" := str_extract(.data[[code_col]], code_pattern),
      "{new_name_col}" := str_trim(str_remove(.data[[name_col]], paste0("^", .data[[new_code_col]])))
    )
}
