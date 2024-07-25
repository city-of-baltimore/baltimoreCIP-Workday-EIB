#' Create an EIB for the new budget
build_new_budget_eib <- function(data,
                                 budget_cols = c(
                                   "Budget Name*" = "Project Name",
                                   "Header Key*" = header_key_col
                                 ),
                                 budget_defaults = list(
                                   "Budget Memo" = getOption("curr_fy_memo"),
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
                                   # TODO: See below - set Fiscal Time Interval correctly
                                   # "Fiscal Time Interval*" = "Jul",
                                   "Fund" = "FGSFund Code",
                                   "Cost Center" = "Cost Center Code",
                                   "Revenue Category" = "Revenue Category Code"
                                 ),
                                 line_defaults = list(
                                   "Account Set" = "Parent_Account_Set",
                                   # FIXME: Fiscal time interval should be set
                                   # based on the month of the budget plan Date
                                   # From

                                   "Fiscal Time Interval*" = "Jul"
                                 ),
                                 project_code_col = "Project Code",
                                 amt_col = getOption("curr_fy_col"),
                                 header_key_col = "Header Key",
                                 line_key_col = "Line Key",
                                 debit_col = "Budget Debit Amount",
                                 credit_col = "Budget Credit Amount",
                                 ledger_account_col = "Ledger Account or Ledger Account Summary",
                                 fy_col = "Fiscal Year Start",
                                 default_fy_start = getOption("curr_fy")) {
  data <- data |>
    dplyr::filter(
      .data[[amt_col]] > 0
    ) |>
    tidyr::replace_na(
      replace = set_names(
        list(default_fy_start),
        fy_col
      )
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
              "Year" = fy_col
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
