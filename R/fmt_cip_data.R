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
