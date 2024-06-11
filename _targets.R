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
  curr_fy_col = "FY2025",
  curr_fy = 2024,
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
    "Capital_Projects_With_Plan_Info 6.4.2024.xlsx"
  ),

  # Read Workday report on project with plan info
  prj_plan_info_report = readxl::read_excel(prj_plan_info_report_path, skip = 1) |>
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
    "Capital_Projects_-_Six-Year_CIP.xlsx"
  ),

  # Read CIP data from file
  cip_data = readxl::read_excel(cip_data_path) |>
    dplyr::left_join(
      # Join a Fiscal Year column inferred from the Date From column
      prj_plan_info_report |>
        mutate(
          `Project Code` = `Project ID`,
          `Fiscal Year From` = year(`Date From`) + (month(`Date From`) >= 7),
          .keep = "none"
        ),
      by = "Project Code",
      na_matches = "never"
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
      .data[["Project Code"]] %in% project_eib_type[["new"]]
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
