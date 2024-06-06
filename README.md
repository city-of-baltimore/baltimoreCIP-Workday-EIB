
# baltimoreCIP-Workday-EIB

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The goal of the `baltimoreCIP-Workday-EIB` project is to convert the reports from Adaptive Planning of other sources into spreadsheets that support the Workday Enterprise Interface Builder (EIB).

This project is structured as a reproducible analysis pipeline built using the `{targets}` R package together with the `{tidyverse}` family of R packages and the `{openxlsx2}` R package.

As of June 6, 2024, this project uses the Capital Projects Six-Year CIP report from Adaptive Planning and the Capital Projects With Plan Info report from Workday as the data sources used to fill three templates:

- `files/Put_Budget_Template_Projects.xlsx`: Creates a project plan or budget plan for existing Workday projects.
- `files/COB New Capital Project Budget EIB Load Template.xlsx`: Loads project budgets for Workday projects with "draft" budget plans.
- `files/COB_Import_Project_Budget_Amendment Template.xlsx`: Amends an existing project budgets for Workday projects with "available" budget plans.

Additional templates should be added to the `files` folder. The source data is stored in the un-tracked folder `_targets/user/data` and can be provided to authorized users where appropriate.

To run this pipeline, add the source files to this folder and then run `targets::tar_make()`. The output files are located in the `_output` folder.

For questions, please contact Eli Pousson, Data Lead with the Baltimore City Department of Planning, at eli.pousson@baltimorecity.gov.
