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
                                    credit_col = "Budget Credit Amount",
                                    amt_min = 0) {
  if (is.numeric(amt_min)) {
    # Filter to positive amount values only
    data <- data |>
      dplyr::filter(
        .data[[amt_col]] > amt_min
      )
  }

  data <- data |>
    dplyr::filter(
      .data[[amt_col]] != 0
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
      pick(
        all_of(names(budget_cols)[budget_cols == header_key_col])
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
