#' Filter projects with a start date after the start of the current fiscal year
filter_project_start_date <- function(data,
                                      curr_fy_start_date = "2024-07-01 UTC") {
  project_late_start_date <- data |>
    filter(
      .data[["Project Start Date"]] > lubridate::date(curr_fy_start_date)
    ) |>
    distinct(`Project Code`)

  n_late_start_date <- nrow(project_late_start_date)

  # FIXME: Add date checks
  if (n_late_start_date > 0) {
    cli::cli_bullets(
      c(
        "Excluding {n_late_start_date} project{?s} with with a late start date.",
        "i" = "Projects with start dates after the current fiscal year should
        be corrected before creating a budget plan."
      )
    )

    data <- data |>
      dplyr::filter(
        .data[["Project Start Date"]] <= lubridate::date(curr_fy_start_date)
      )
  }

  data
}
