# Load packages
library(targets)
library(tarchetypes)
library(tidyverse)

# Set target options:
tar_option_set(
  tidy_eval = TRUE,
  packages = c("tidyverse", "openxlsx2"),
  format = "qs"
)

tar_source()

options(
  cip_data_filename = "Capital_Projects_-_Six-Year_CIP.xlsx",
  plan_info_filename = "Capital Projects with Plan Info 6.17.24.xlsx",
  invalid_date_report_filename = "2024-06-17_CIP-Projects_Invalid-Dates.xlsx",
  curr_fy_col = "FY2025",
  curr_fy = 2024,
  curr_fy_start_date_str = "2024-07-01",
  curr_ammend_date = as_date("2024-07-01"),
  curr_fy_memo = "FY-25 Capital Budget",
  openxlsx2.na.strings = ""
)

# Replace the target list below with your own:
tar_plan(

  # Get path to report supplied by Kristen Ahearn on 6.4.2024
  # NOTE: File is not included in repository
  prj_plan_info_report_path = here::here(
    "_targets", "user", "data",
    getOption("plan_info_filename")
  ),

  # Read Workday report on project with plan info
  prj_plan_info_report = readxl::read_excel(
    prj_plan_info_report_path,
    skip = 1
  ) |>
    # Categorize by type
    dplyr::mutate(
      put = is.na(`Plan Status`),
      new = `Plan Status` == "Draft",
      amend = `Plan Status` == "Available"
    ),

  # Make a list of project IDs for each type
  project_eib_type = list(
    "put" = filter(prj_plan_info_report, put) |> pull(`Project ID`),
    "new" = filter(prj_plan_info_report, new) |> pull(`Project ID`),
    "amend" = filter(prj_plan_info_report, amend) |> pull(`Project ID`)
  ),

  # Set file path for Capital Projects 6-Year CIP report
  # NOTE: File is not included in repository
  cip_data_path = here::here(
    "_targets", "user", "data",
    getOption("cip_data_filename")
  ),

  curr_fy_start_date = lubridate::date(getOption("curr_fy_start_date_str")),

  # Read CIP data from file
  cip_data = readxl::read_excel(cip_data_path) |>
    filter(
      `Cost Center Code` != "Total"
    ) |>
    fmt_cip_data() |>
    summarise_cip_data_fy(
      .by = c(
        "FGSFund Code",
        "FGSFund Name",
        "Cost Center Code",
        "Cost Center Name",
        "Revenue Category Code",
        "Revenue Category Name",
        "Grant_Detail Name",
        "Project Code",
        "Project Name"
      )
    ) |>
    dplyr::left_join(
      # Join a Fiscal Year column inferred from the Date From column
      prj_plan_info_report |>
        mutate(
          `Project Start Date` = `Project Start Date`,
          `Project End Date` = `Project End Date`,
          `Project Code` = `Project ID`,
          `Date From`,
          `Date To`,
          `Plan Status`,
          # NOTE: Fiscal year start is based on date from of the plan period
          # Not the Project start date
          `Fiscal Year Start` = year(`Date From`) + (month(`Date From`) >= 7),
          .keep = "none"
        ),
      by = "Project Code",
      na_matches = "never"
    ),
  cip_curr_fy_projects = cip_data |>
    filter(
      .data[[getOption("curr_fy_col")]] > 0
    ) |>
    distinct(`Project Code`) |>
    pull(`Project Code`),
  cip_data_project_date_validation = cip_data |>
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
    ) |>
    dplyr::select(
      !c(
        starts_with("FY"),
        starts_with("Revenue"),
        any_of(c("Plan Status", "Date From", "Date To", "Fiscal Year Start"))
      )
    ) |>
    distinct() |>
    mutate(
      # FIXME: Hard-coded fiscal year name
      "Has FY2025 Funding" := dplyr::if_else(
        .data[["Project Code"]] %in% cip_curr_fy_projects,
        "Y",
        "N"
      )
    ) |>
    relocate(
      starts_with("Project"),
      .before = everything()
    ),
  cip_data_project_date_validation_out = cip_data_project_date_validation |>
    filter(
      !is.na(.data[["Project Start Date Flag"]]) | !is.na(.data[["Project End Date Flag"]])
    ) |>
    openxlsx2::write_xlsx(
      getOption("invalid_date_report_filename")
    ),

  # Create EIB file for loading budget plans as draft
  put_budget_eib = cip_data |>
    filter(
      .data[["Project Code"]] %in% project_eib_type[["put"]]
    ) |>
    build_put_budget_eib(),

  # Export put budget EIB
  put_budget_eib_out = save_eib_wb_sheets(
    put_budget_eib,
    template = here::here("files", "Put_Budget_Template_Projects.xlsx"),
    file = here::here("_output", "Put_Budget_Projects.xlsx"),
    overwrite = TRUE
  ),

  # Create new capital projects EIB
  new_budget_eib = cip_data |>
    filter(
      # FIXME: Made this change from new only to new *and* put on 2024-06
      .data[["Project Code"]] %in% c(
        project_eib_type[["put"]],
        project_eib_type[["new"]]
      )
    ) |>
    build_new_budget_eib(),

  # Export new budget EIB
  new_budget_eib_out = save_eib_wb_sheets(
    new_budget_eib,
    template = here::here("files", "COB New Capital Project Budget EIB Load Template.xlsx"),
    file = here::here("_output", "COB New Capital Project Budget EIB Load.xlsx"),
    overwrite = TRUE
  ),

  # Create amended capital projects EIB
  amend_budget_eib = cip_data |>
    filter(
      .data[["Project Code"]] %in% project_eib_type[["amend"]]
    ) |>
    build_ammend_budget_eib(),

  # Export amend budget EIB
  amend_budget_eib_out = save_eib_wb_sheets(
    amend_budget_eib,
    template = here::here("files", "COB_Import_Project_Budget_Amendment Template.xlsx"),
    file = here::here("_output", "COB_Import_Project_Budget_Amendment.xlsx"),
    overwrite = TRUE
  )
)
