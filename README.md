# PEDSnet Data Quality Dashboard

## Purpose

The code in this repository generates a dashboard to visualize output from data quality checks.

## How to Use

### System Requirements

To execute the R code in this repository, users will need to install the packages named in the top lines of [driver.R](code/driver.R), of [server.R](reporting/server.R), and of [ui.R](reporting/ui.R). The shiny dashboard was developed on R version 4.2.0 (2022-04-22).

### Data Requirements

The data is expected to be in the format of the output from the dqa_processing step of the PEDSnet DQA process, which will be available on GitHub in the PEDSnet organization.

### Execution Process


1) Set up configurations for execution of PEDSnet standardized R framework code (including setting up srcr config file to successfully establish connection to the database containing the DQA results)
2) Edit run.R:
    - `config('results_schema', 'dqa_rox')`: change 'dqa_rox' to the name of the schema containing the DQA output 
    - `config('db_current', 'vxx')`: change 'vxx' to the name of the current version of the data. Should match the name assigned in the DQA library and processing steps
    - `config('db_previous', 'vyy')`: change 'vyy' to the name of the previous version of the data. Should match the name assigned in the DQA library and processing steps
    - `config('mask_site', FALSE)`: if you want to display individual site names on the dashboard (as they appear in the `site` column of the DQA output), set to FALSE. If you want the site names to be replaced with masked identifiers (e.g. Site A, Site B, etc.) set to TRUE
    - `config('results_name_tag', '_op_1510')`: set '_op_1510' to the suffix on the table names in the DQA results schema
3) Open ui.R (or server.R) in RStudio
4) Click `Run App`. The dashboard should open in a new tab in RStudio and can be viewed in a browser window by clicking `View in browser`




For questions contact dickinsokl@chop.edu
