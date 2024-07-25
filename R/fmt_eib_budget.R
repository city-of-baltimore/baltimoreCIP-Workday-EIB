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

  # FIXME: 2024-06-28 - reminder to add handling for the de-appropriations.
  # Then we can rerun the Plan report from Production, and rerun all the EIBs.
  # Bernie also noted that the projects that we are taking money from (the
  # deappropriations, or those with a negative amount) should be switched, such
  # that the positive amount for the revenues goes into the debit column and a
  # positive amount for expenses goes into the credit column

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
