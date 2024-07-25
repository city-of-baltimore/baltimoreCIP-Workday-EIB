

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Baltimore City Capital Budget Workday EIB Generation

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

The goal of the `baltimoreCIP-Workday-EIB` project is to convert the
exported reports from Adaptive Planning into spreadsheets based on
templates for the Workday Enterprise Interface Builder (EIB).

For questions, please contact Eli Pousson, Data Lead with the Baltimore
City Department of Planning, at eli.pousson@baltimorecity.gov{.email}.

> [!TIP]
>
> This project is built using {targets} and {tarchetypes}: two R
> packages designed to support the reproducible analytical pipelines
> (RAPs). For more information on {targets}, see [The {targets} R
> package user manual](https://books.ropensci.org/targets/).

## Background

The [Capital Improvement
Program](https://planning.baltimorecity.gov/planning-capital-improvement/)
is a six-year plan for funding capital projects by City agencies. The
program is updated and adopted each year as part of the Baltimore City
Budget.

This repository holds the code used to prepare the Capital Improvement
Program data from Adaptive Planning for import into Workday as the
current year fiscal budget.

## Organization

Supporting functions for this pipeline are located in the `R` folder.
The source data is stored in the un-tracked folder `_targets/user/data`
and can be provided to authorized users where appropriate.

As of June 2024, this project uses the “Capital Projects - Six-Year CIP”
report from Adaptive Planning and the “Capital Projects With Plan Info”
report from Workday as the data sources used to fill three templates:

- `files/Put_Budget_Template_Projects.xlsx`: Creates a project plan or
  budget plan for existing Workday projects.
- `files/COB New Capital Project Budget EIB Load Template.xlsx`: Loads
  project budgets for Workday projects with “draft” budget plans.
- `files/COB_Import_Project_Budget_Amendment Template.xlsx`: Amends an
  existing project budgets for Workday projects with “available” budget
  plans.

Additional templates, if needed for future development, should be added
to the `files` folder.

## Usage

This project requires the `{tidyverse}`, `{openxlsx2}`, `{here}`,
`{readxl}`, `{targets}`, and `{tarchetypes}` R packages.

To run this pipeline, add the source files to `_targets/user/data` and
then run `targets::tar_make()`. The output files are stored in the
`_output` folder.

Note that access to Adaptive Planning and the required Workday report is
limited to select security roles.
