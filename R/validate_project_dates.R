#' Validate project start and end dates
validate_project_dates <- function(data,
                                   curr_fy_start_date) {
  data |>
    dplyr::mutate(
      "Project Start Date Flag" := case_when(
        `Project Start Date` > curr_fy_start_date ~ "Start date after current fiscal year",
        `Project Start Date` < lubridate::date("2022-07-01") ~ "Start date before FY23 fiscal year",
        is.na(`Project Start Date`) ~ "Missing start date"
      ),
      "Project End Date Flag" := case_when(
        is.na(`Project End Date`) ~ "Missing end date",
        `Project End Date` < curr_fy_start_date ~ "End date before current fiscal year",
        `Project End Date` == curr_fy_start_date ~ "End date set to first day of current fiscal year"
      )
    )
}
