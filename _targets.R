# Load packages
library(targets)
library(tarchetypes)
library(tidyverse)

# Set target options:
tar_option_set(
  tidy_eval = TRUE,
  # pak::pkg_install("elipousson/sharepointr"),
  packages = c("tidyverse", "openxlsx2", "here", "readxl"),
  format = "qs"
)

tar_source()

options(
  cip_data_filename = "Capital_Projects_-_Six-Year_CIP.xlsx",
  plan_info_filename = "Capital_Projects_With_Plan_Info 06282024 0916.xlsx",
  plan_info_skip = 1,
  # plan_info_filename = "Capital_Projects_With_Plan_Info_Sandbox-2024-06-21.xlsx",
  # plan_info_skip = 3,
  invalid_date_report_filename = "2024-06-17_CIP-Projects_Invalid-Dates.xlsx",
  curr_fy_col = "FY2025",
  curr_fy = 2025,
  curr_fy_start_date_str = "2024-07-01",
  curr_ammend_date = as_date("2024-07-01"),
  curr_fy_memo = "FY-25 Capital Budget",
  openxlsx2.na.strings = ""
)

# Replace the target list below with your own:
tar_plan(
  curr_fy_start_date = lubridate::date(getOption("curr_fy_start_date_str")),

  # Set file path for Capital Projects 6-Year CIP report
  # NOTE: File is not included in repository
  cip_data_path = here::here(
    "_targets", "user", "data",
    getOption("cip_data_filename")
  ),

  # Read CIP data from file
  cip_data_src = readxl::read_excel(cip_data_path) |>
    dplyr::filter(`Cost Center Code` != "Total"),

  # Get path to report supplied by Kristen Ahearn
  # NOTE: File is not included in repository
  prj_plan_info_report_path = here::here(
    "_targets", "user", "data",
    getOption("plan_info_filename")
  ),

  # Read Workday report on project with plan info
  prj_plan_info_report_src = readxl::read_excel(
    path = prj_plan_info_report_path,
    skip = getOption("plan_info_skip")
  ),
  prj_plan_info_report = prj_plan_info_report_src |>
    dplyr::filter(
      # Exclude denied Plan Status information
      is.na(`Plan Status`) | `Plan Status` != "Denied",
      .data[["Project ID"]] %in% cip_data_src[["Project Code"]]
    ) |>
    mutate(
      # Flag duplicate budget plan information
      budget_plan_id = row_number(),
      .by = `Project ID`
    ) |>
    filter(
      # Keep only the first budget plan if multiple exist
      budget_plan_id == 1
    ) |>
    mutate(
      `Project Start Date`,
      `Project End Date`,
      `Project Code` = `Project ID`,
      `Date From`,
      `Date To`,
      `Plan Status`,
      # FIXME: Added the following two lines on 2024-06-28 to try to trouble-shoot errors from the EIB
      # `Temp Date From` = dplyr::coalesce(`Date From`, `Project Start Date`),
      # `Fiscal Year Start` = year(`Temp Date From`) + (month(`Temp Date From`) >= 7),
      # NOTE: Fiscal year start is based on date from of the plan period
      # Not the Project start date
      `Fiscal Year Start` = year(`Date From`) + (month(`Date From`) >= 7),
      # Categorize by type
      put = is.na(`Plan Status`),
      # TODO: Document how the plan status is assigned
      new = is.na(`Plan Status`) | `Plan Status` %in% c("Draft", "In Progress"),
      amend = `Plan Status` %in% c("Available"),
      .keep = "none"
    ),

  # Make a list of project IDs for each type
  project_eib_type = list(
    "put" = filter(prj_plan_info_report, put) |> pull(`Project Code`),
    "new" = filter(prj_plan_info_report, new) |> pull(`Project Code`),
    "amend" = filter(prj_plan_info_report, amend) |> pull(`Project Code`)
  ),
  cip_data = cip_data_src |>
    fmt_cip_data() |>
    summarise_cip_data_fy(
      .by = c(
        "FGSFund Code", "FGSFund Name",
        "Cost Center Code", "Cost Center Name",
        "Revenue Category Code", "Revenue Category Name",
        "Grant_Detail Name", "Project Code", "Project Name"
      )
    ) |>
    dplyr::left_join(
      # Join a Fiscal Year column inferred from the Date From column
      prj_plan_info_report,
      relationship = "many-to-one",
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
    validate_project_dates(curr_fy_start_date) |>
    validate_budget_plan_dates(curr_fy_start_date) |>
    dplyr::select(
      !c(
        starts_with("FY"), starts_with("Revenue"),
        any_of(c("Fiscal Year Start"))
      )
    ) |>
    distinct() |>
    mutate(
      # FIXME: Hard-coded fiscal year name
      "Has FY2025 Funding" := dplyr::if_else(
        .data[["Project Code"]] %in% cip_curr_fy_projects,
        "Y", "N"
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
      .data[[getOption("curr_fy_col")]] != 0,
      put
    ) |>
    build_put_budget_eib(),

  # Export put budget EIB
  put_budget_eib_out = wb_save_eib_sheets(
    put_budget_eib,
    template = here::here("files", "Put_Budget_Template_Projects.xlsx"),
    file = here::here("_output", "Put_Budget_Projects.xlsx"),
    overwrite = TRUE
  ),

  # Create new capital projects EIB
  new_budget_eib = cip_data |>
    filter(
      .data[[getOption("curr_fy_col")]] != 0,
      new
    ) |>
    build_new_budget_eib(
      default_fy_start = getOption("curr_fy")
    ),

  # Export new budget EIB
  new_budget_eib_out = wb_save_eib_sheets(
    new_budget_eib,
    template = here::here("files", "COB New Capital Project Budget EIB Load Template.xlsx"),
    file = here::here("_output", "COB New Capital Project Budget EIB Load.xlsx"),
    overwrite = TRUE
  ),

  # Create amended capital projects EIB
  amend_budget_eib = cip_data |>
    filter(
      .data[[getOption("curr_fy_col")]] != 0,
      amend
    ) |>
    build_ammend_budget_eib(amt_min = NULL),

  # Export amend budget EIB
  amend_budget_eib_out = wb_save_eib_sheets(
    amend_budget_eib,
    template = here::here("files", "COB_Import_Project_Budget_Amendment Template.xlsx"),
    file = here::here("_output", "COB_Import_Project_Budget_Amendment.xlsx"),
    overwrite = TRUE
  ),
  curr_fy_cip_data_excluded = cip_data |>
    dplyr::filter(
      FY2025 > 0,
      is.na(`Fiscal Year Start`)
      # NOTE: Enable the following to filter to records that are included
      # !(
      #   `Project Name` %in% c(
      #     amend_budget_eib[["Import Budget Amendment"]][["Budget Name"]],
      #     new_budget_eib[["Import Budget High Volume"]][["Budget Name*"]]
      #   )
      # )
    ),
  # Render README
  tar_quarto(
    readme_qmd,
    path = here::here("README.qmd")
  )

  # TODO: Implement a summary report
  #   tarchetypes::tar_quarto(
  #     qto_report,
  #     here::here("report.qmd")
  #   )
)
