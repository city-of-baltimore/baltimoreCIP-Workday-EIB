#' Filter projects with a missing end date
filter_project_end_date <- function(data) {
  project_missing_end_date <- data |>
    filter(
      is.na(.data[["Project End Date"]])
    ) |>
    distinct(`Project Code`)

  n_missing_end_date <- nrow(project_missing_end_date)

  if (n_missing_end_date > 0) {
    cli::cli_bullets(
      c(
        "Excluding {n_missing_end_date} project{?s} with with a missing end date.",
        "i" = "Projects with missing end dates can't be loaded with a Put Budget EIB."
      )
    )

    data <- data |>
      dplyr::filter(
        !is.na(.data[["Project End Date"]])
      )
  }

  data
}
