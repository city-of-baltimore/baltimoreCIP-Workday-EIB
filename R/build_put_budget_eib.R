#' Build a put budget EIB
build_put_budget_eib <- function(data,
                                 project_code_col = "Project Code",
                                 cols = c(
                                   "Plan Name" = "Project Name",
                                   "Project" = project_code_col
                                 ),
                                 amt_col = getOption("curr_fy_col", "FY2025"),
                                 plan_defaults = list(
                                   "Add Only" = "Y",
                                   "Company+" = "COB",
                                   "Plan Structure*" = "Capital_Project_Budget",
                                   "Is Inactive" = "n",
                                   "Entry Type" = "Active_Project",
                                   "Default Reporting Currency" = "USD",
                                   "Enable Multicurrency Reference" = "Y",
                                   "Currency Rate Type" = "Current",
                                   "Default Balanced Amendment Reference" = "n",
                                   "Enable For Allocations Reference" = "n",
                                   "Plan Calculation Type" = "Capital_Project_Active_Budget"
                                 )) {
  data <- data |>
    dplyr::filter(
      !is.na(.data[[project_code_col]]),
      .data[[amt_col]] > 0
    )

  data |>
    filter_project_end_date() |>
    # filter_project_start_date() |>
    dplyr::distinct(pick(all_of(project_code_col)), .keep_all = TRUE) |>
    dplyr::select(all_of(cols)) |>
    dplyr::mutate(
      "Spreadsheet Key*" := row_number()
    ) |>
    cbind_default_cols(plan_defaults)
}
