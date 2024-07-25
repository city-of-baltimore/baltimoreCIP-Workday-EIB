#' Format a header key based on the project code
fmt_header_key <- function(data,
                           project_code_col = "Project Code",
                           header_key_col = "Header Key") {
  data |>
    dplyr::mutate(
      "{header_key_col}" := cur_group_id(),
      .by = all_of(project_code_col),
      .before = all_of(project_code_col)
    )
}
