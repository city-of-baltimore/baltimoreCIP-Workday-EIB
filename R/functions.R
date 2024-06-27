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

#' Format CIP data by dropping select columns and formatting code/name column pairs
fmt_cip_data <- function(
    data,
    drop_cols = c(
      "PCode Code", "PCode Name",
      "Fund, Grant, Special Purpose Code",
      "Fund, Grant, Special Purpose Name",
      "RObject Code", "Grant_Detail Code"
    )) {
  data |>
    select(!any_of(drop_cols)) |>
    filter(!is.na(`Project Code`)) |>
    fmt_wd_code_name(
      "FGSFund Code",
      "FGSFund Name",
      "^[:digit:]+"
    ) |>
    fmt_wd_code_name(
      "Cost Center Code",
      "Cost Center Name",
      "^(CAP|RES|zzDNU_CAP|DNU_CAP)[:digit:]+"
    ) |>
    fmt_wd_code_name(
      "Project Code",
      "Project Name",
      "^PRJ[:digit:]+",
      new_name_col = "Project Name Short"
    ) |>
    fmt_wd_code_name(
      "Revenue Category Code",
      "Revenue Category Name",
      "^RC[:digit:]+"
    ) |>
    fmt_wd_code_name(
      "RAccount Code",
      "RAccount Name",
      "^([:digit:]|:)+"
    ) |>
    mutate(
      `RAccount Name` = str_remove(`RAccount Name`, "^:")
    )
}


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

#' Format EIB budget lines data
fmt_eib_budget <- function(
    data,
    project_code_col = "Project Code",
    revenue_col = "Revenue Category Code",
    amt_col = getOption("curr_fy_col", "FY2025"),
    header_key_col = "Header Key",
    line_key_col = "Line Key",
    debit_col = "Budget Debit Amount",
    credit_col = "Budget Credit Amount",
    ledger_account_col = "Ledger Account Summary",
    by = c(
      "FGSFund Code",
      "Cost Center Code",
      "Project Code"
    )) {
  credit_data <- data |>
    dplyr::summarise(
      "{ledger_account_col}" := "AllProjectBudgetRevenues",
      "{credit_col}" := sum(.data[[amt_col]], na.rm = TRUE),
      # "{debit_col}" := openxlsx2::na_strings(),
      .by = all_of(c(by, revenue_col))
    )

  debit_data <- data |>
    dplyr::summarise(
      "{ledger_account_col}" := "AllBudgetExpenses",
      # "{credit_col}" := 0,
      "{debit_col}" := sum(.data[[amt_col]], na.rm = TRUE),
      .by = all_of(by)
    )

  credit_data |>
    dplyr::bind_rows(
      debit_data
    ) |>
    dplyr::mutate(
      "{header_key_col}" := cur_group_id(),
      "{line_key_col}" := row_number(),
      .by = all_of(project_code_col),
      .before = all_of(project_code_col)
    ) |>
    dplyr::arrange({{ project_code_col }})
}

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

filter_project_start_date <- function(data) {
  project_late_start_date <- data |>
    filter(
      .data[["Project Start Date"]] > lubridate::date("2024-07-01 UTC")
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
        .data[["Project Start Date"]] <= lubridate::date("2024-07-01 UTC")
      )
  }

  data
}

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

#' Build a budget ammendment EIB
build_ammend_budget_eib <- function(data,
                                    budget_cols = c(
                                      "Budget Name" = "Project Name",
                                      "Fiscal Year*" = "Fiscal Year Start",
                                      "Header Key*" = header_key_col
                                    ),
                                    entry_cols = c(
                                      "Project" = "Project Code",
                                      "Fund" = "FGSFund Code",
                                      "Cost Center" = "Cost Center Code",
                                      "Revenue Category" = "Revenue Category Code",
                                      header_key_col,
                                      line_key_col,
                                      ledger_account_col,
                                      debit_col,
                                      credit_col
                                    ),
                                    budget_defaults = list(
                                      # FIXME: Figure out how the fiscal year is filled in
                                      # "Fiscal Year*" = getOption("curr_fy", 2024),
                                      "Amendment Date*" = getOption("curr_ammend_date"),
                                      "Description*" = getOption("curr_fy_memo"),
                                      "Budget Amendment Type*" = "Supplemental_Project_Funding",
                                      "Auto Complete" = "Y",
                                      "Submit" = "Y",
                                      "Budget Structure*" = "Capital_Project_Budget"
                                    ),
                                    entry_defaults = list(
                                      "Fiscal Time Interval*" = "Jul",
                                      "Account Set" = "Parent_Account_Set",
                                      "Memo" = getOption("curr_fy_memo")
                                    ),
                                    project_code_col = "Project Code",
                                    amt_col = getOption("curr_fy_col"),
                                    header_key_col = "Header Key",
                                    line_key_col = "Line Key",
                                    ledger_account_col = "Ledger Account Summary",
                                    debit_col = "Budget Debit Amount",
                                    credit_col = "Budget Credit Amount") {
  # Filter to positive amount values only
  data <- data |>
    dplyr::filter(
      .data[[amt_col]] > 0
    )

  # Filter invalid project start and end date values
  data <- data |>
    filter_project_end_date() |>
    filter_project_start_date()

  # Create first sheet (budget data)
  budget_data <- data |>
    fmt_header_key(
      project_code_col = project_code_col,
      header_key_col = header_key_col
    ) |>
    dplyr::select(
      all_of(budget_cols)
    ) |>
    dplyr::distinct() |>
    cbind_default_cols(budget_defaults) |>
    dplyr::arrange(
      # FIXME: Hard-coded header key value
      pick(
        all_of("Header Key*")
      )
    )

  # Create second sheet (entry data)
  entry_data <- data |>
    fmt_eib_budget(
      project_code_col = project_code_col,
      amt_col = amt_col,
      header_key_col = header_key_col,
      line_key_col = line_key_col,
      debit_col = debit_col,
      credit_col = credit_col
    ) |>
    dplyr::select(
      all_of(entry_cols)
    ) |>
    cbind_default_cols(entry_defaults) |>
    dplyr::arrange(
      pick(
        all_of(c(header_key_col, line_key_col))
      )
    )

  list(
    "Import Budget Amendment" = budget_data,
    "Amendment Entry Data" = entry_data
  )
}


#' Create an EIB for the new budget
build_new_budget_eib <- function(data,
                                 budget_cols = c(
                                   "Budget Name*" = "Project Name",
                                   "Header Key*" = header_key_col
                                 ),
                                 budget_defaults = list(
                                   "Import Mode" = "REPLACE_ALL",
                                   "Auto Complete" = "Y",
                                   "Submit" = "Y",
                                   "Budget Structure*" = "Capital_Project_Budget"
                                 ),
                                 line_order_col = "Line Order",
                                 line_cols = c(
                                   "Project" = "Project Code",
                                   ledger_account_col,
                                   header_key_col,
                                   line_key_col,
                                   debit_col,
                                   credit_col,
                                   "Fund" = "FGSFund Code",
                                   "Cost Center" = "Cost Center Code",
                                   "Revenue Category" = "Revenue Category Code"
                                 ),
                                 line_defaults = list(
                                   "Memo" = getOption("curr_fy_memo"),
                                   "Account Set" = "Parent_Account_Set",
                                   "Fiscal Time Interval*" = "Jul"
                                 ),
                                 project_code_col = "Project Code",
                                 amt_col = getOption("curr_fy_col"),
                                 header_key_col = "Header Key",
                                 line_key_col = "Line Key",
                                 debit_col = "Budget Debit Amount",
                                 credit_col = "Budget Credit Amount",
                                 ledger_account_col = "Ledger Account or Ledger Account Summary") {
  data <- data |>
    dplyr::filter(
      .data[[amt_col]] > 0
    )

  budget_data_keys <- data |>
    dplyr::distinct(pick(all_of(project_code_col)), .keep_all = TRUE) |>
    fmt_header_key(
      project_code_col = project_code_col,
      header_key_col = header_key_col
    )

  budget_data <- budget_data_keys |>
    dplyr::select(
      all_of(budget_cols)
    ) |>
    cbind_default_cols(budget_defaults)

  budget_lines_data <- data |>
    fmt_eib_budget(
      project_code_col = project_code_col,
      amt_col = amt_col,
      header_key_col = header_key_col,
      line_key_col = line_key_col,
      debit_col = debit_col,
      credit_col = credit_col,
      ledger_account_col = ledger_account_col
    ) |>
    dplyr::select(
      all_of(line_cols)
    ) |>
    dplyr::left_join(
      budget_data_keys |>
        dplyr::select(
          all_of(
            c(
              header_key_col,
              "Year" = "Fiscal Year Start"
            )
          )
        ),
      relationship = "many-to-one",
      by = header_key_col
    ) |>
    cbind_default_cols(line_defaults) |>
    dplyr::mutate(
      "{line_order_col}" := .data[[line_key_col]],
      .after = all_of(line_key_col)
    ) |>
    dplyr::arrange(
      pick(
        all_of(c(header_key_col, line_key_col))
      )
    )

  list(
    "Import Budget High Volume" = budget_data,
    "Budget Lines Data" = budget_lines_data
  )
}
