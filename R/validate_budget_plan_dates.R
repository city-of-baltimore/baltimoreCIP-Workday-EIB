#' Validate budget plan "Date From" values
validate_budget_plan_dates <- function(data,
                                       curr_fy_start_date = NULL) {
  curr_fy_start_date <- lubridate::as_date(curr_fy_start_date)

  data |>
    dplyr::mutate(
      "Date From Flag" := case_when(
        is.na(`Date From`) & is.na(`Plan Status`) ~ "Missing Date From",
        `Date From` < curr_fy_start_date ~ "Date From before current fiscal year",
        `Date From` > `Project Start Date` ~ "Date From after Project Start Date",
        `Date From` != `Project Start Date` ~ "Date From mismatch with Project Start Date"
      )
    )
}
