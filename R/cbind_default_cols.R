#' Use `purrr::list_cbind` to bind a set of "default" column values
cbind_default_cols <- function(data, default_cols) {
  if (!is.data.frame(default_cols)) {
    default_cols <- tibble::as_tibble(default_cols)
  }

  purrr::list_cbind(
    list(
      data,
      default_cols
    )
  )
}
