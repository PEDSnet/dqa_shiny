#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(shinydashboard)
library(plotly)
library(gt)
source('../site/run.R')



shinyUI(fluidPage(tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
),
title = "PEDSnet DQA Dashboard",

# Application Title
navbarPage(dashboardHeader(title=span(img(src="logo.svg", height=32,width=34,
                                          style = "padding-top:0px; padding-bottom:4px;"),
                                      "PEDSnet DQA Dashboard",
                                      style = "font-family: Trebuchet MS; color: white; font-size: 200%")),
           tabPanel(title="Home", icon=icon("home"),
                    sidebarLayout(
                      sidebarPanel(
                        tags$h4("Welcome to the PEDSnet Data Quality Dashboard!"),
                        tags$h6("Report Refresh Date:", Sys.Date()),
                        tags$h6("Current Database Version:", config('db_current')),
                        radioButtons(inputId="largen_toggle",
                                     label="Display Type",
                                     choices=list('Individual Sites'=1,
                                               'Summary Metrics'=2),
                                     selected=1),
                        tags$p("Selecting Individual Sites will display the default dashboard, where each site is displayed individually and against each other site. Selecting Summary Metrics will modify the visualizations to compare each site against overall metrics across the other sites. This option is helpful to de-clutter if there are a large number of sites."),
                        tags$p("Contact pedsnetdcc@chop.edu for more information")
                      ),
                      mainPanel(
                        tags$p("The PEDSnet Data Quality Dashboard (PQD) provides output from the data quality program developed and maintained
                                                 by the PEDSnet Data Quality team."),
                        tags$p("The PQD is displayed by check type. A check type is a category of checks that can be
                                                 executed across a wide variety of fields, domains, tables, or other applications.
                                                 Below are descriptions of each check type."),
                        DT::dataTableOutput("dt_descriptions")
                      ))),
           navbarMenu(title="Check Type", icon=icon("chart-area"),
                      # DC ----
                      tabPanel("Data Cycle Changes (DC)",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="sitename_dc",
                                               label="Institution",
                                               choices = NULL),
                                   # only show comparison option for overall metrics
                                   conditionalPanel(
                                     condition="input.largen_toggle == 2",
                                     radioButtons(inputId="comp_dc_ln",
                                                  label="Comparison Type",
                                                  choices=list("Across Sites"=1,
                                                               "None"=0))
                                   ),
                                   selectInput(inputId = "dc_domain",
                                               label="Domain",
                                               choices = c('ADT')), # set a default to avoid empty string detection
                                   checkboxGroupInput(inputId="dc_subdomain",
                                                      label="Specific Check",
                                                      choices=NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     box(title="Overall Data Cycle Changes",
                                         plotlyOutput("dc_overall", height=500, width=1000)),
                                     box(title="Domain-Specific Data Cycle Changes (Records)",width=12,
                                         plotOutput("dc_domain_split")),
                                     box(title="Domain-Specific Data Cycle Changes (Persons)",width=12,
                                         plotOutput("dc_domain_split_persons")),
                                     box(title="Mapping Abbreviations and Descriptions", width=12,
                                         DT::dataTableOutput("dc_mappings"))
                                   )#fluidRow
                                 )#mainPanel
                               )#sidebarlayout
                      ),#tabpanel for changes between data cycles
                      # VS ----
                      tabPanel("Valueset Conformance",
                               sidebarLayout(
                                 sidebarPanel(
                                 selectInput(inputId = "sitename_vs_conf",
                                             label = "Institution",
                                             choices = NULL),
                                 # only show comparison option for overall metrics
                                 conditionalPanel(
                                   condition="input.largen_toggle == 2",
                                   radioButtons(inputId="comp_vs_ln",
                                                label="Comparison Type",
                                                choices=list("Across Sites"=1,
                                                             "None"=0))
                                 )),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     p("Note that sites only show up if there is at least one violation"),
                                     box(title="Valueset Violations Plot", width=12, plotlyOutput("vs_plot")),
                                     box(title="Violations Listings", width=12, DT::dataTableOutput("vs_table"))
                                     )
                                 )
                               )),
                      # VC ----
                      tabPanel("Vocabulary Conformance",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="sitename_vc_conf",
                                               label="Institution",
                                               choices=NULL),
                                   conditionalPanel("input.largen_toggle == 1",
                                                    radioButtons(inputId="vc_denom",
                                                                 label="Overall Vocabularies View",
                                                                 choices=list("Row-level"=1,
                                                                              "Concept-level"=2))),
                                   conditionalPanel(
                                     condition="input.largen_toggle == 2",
                                     radioButtons(inputId="comp_vc_ln",
                                                  label="Comparison Type",
                                                  choices=list("Across Sites"=1,
                                                               "None"=0))
                                   )),
                                 mainPanel(
                                     fluidRow(p("Note: Proportions are of the total rows in the table. Proportions that do not add up to 1 for the given column indicate missing values."),
                                              box(title="Overall Vocabularies",
                                                  plotlyOutput("vc_overall_plot", height=500, width=1000))),
                                     fluidRow(
                                       column(
                                         width=12,
                                         align='left',
                                              tabBox(width='100%',
                                                tabPanel("Vocabulary Conformance Violations Plot", plotlyOutput("vc_plot")),
                                            tabPanel("Violations Listings", DT::dataTableOutput("vc_table")),
                                            tabPanel("Acceptable Vocabularies",
                                                     DT::dataTableOutput("vc_vocabs")))
                                       )#column
                                   )#fluidrow
                                 )#mainpanel
                               )#sidebarlayout
                      ),#tabpanel
                      # UC ----
                      tabPanel("Unmapped Concepts",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "sitename_uc",
                                               label = "Institution",
                                               choices = NULL),
                                   # only show comparison option for overall metrics
                                   conditionalPanel(
                                     condition="input.largen_toggle == 2",
                                     radioButtons(inputId="comp_uc_ln",
                                                  label="Comparison Type",
                                                  choices=list("Across Sites"=1,
                                                               "None"=0))
                                   ),
                                   checkboxGroupInput(inputId="uc_measure",
                                                      label="Measure",
                                                      choices=NULL),
                                   sliderInput("date_uc_range",
                                               label="Date Range",
                                               min=1990L,
                                               max=as.integer(format(Sys.Date(), "%Y")),
                                               value=c(2010L,2022L),
                                               step=1L,
                                               sep="")
                                 ),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     h2("Unmapped Concepts Overall"),
                                     # Unmapped Concepts Proportions
                                     tabBox(
                                       tabPanel("Unmapped Concepts Plot",
                                            plotOutput("uc_overall_plot",height=600,width=1000)),
                                       tabPanel("Top Unmapped Source Values",
                                                h6("Top 10 unmapped source values per column per site"),
                                                p("proportion_of_unmapped is the count of the given source value divided by the number of unmapped rows for that column. Not displayed for Summary Metrics total"),
                                                DT::dataTableOutput("uc_top_tbl", width=1000)))),
                                    fluidRow(box(title="Unmapped Concepts by Year", width=12,
                                         plotOutput("uc_yr_plot", height=600)))
                                 )#mainpanel

                               )#sidebarlayout
                      ),
                      # CFD ----
                      tabPanel("Clinical Fact Documentation",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "sitename_cfd",
                                               label = "Institution",
                                               choices = NULL),
                                   # only show comparison option for overall metrics
                                   conditionalPanel(
                                     condition="input.largen_toggle == 2",
                                     radioButtons(inputId="comp_cfd_ln",
                                                  label="Comparison Type",
                                                  choices=list("Across Sites"=1,
                                                               "None"=0))
                                   )),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     # Clinical Fact Documentation overall
                                     box(title="Clinical Fact Documentation Overall", width=12,
                                         plotlyOutput("cfd_overall_heat_plot", height=750)),
                                     box(title="Mapping Abbreviations and Descriptions", width=12,
                                         DT::dataTableOutput("cfd_mappings")),
                                     # Person facts/records by site
                                     box(title="Clinical Fact Documentation By Site", width=12,
                                         plotOutput("cfd_overall_bysite_plot", height=1000))
                                   )#fluidrow
                                 )#mainpanel

                               )#sidebarlayout
                      ),#tabpanel
                      # BMC ----
                      tabPanel("Best Mapped Concepts",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "sitename_bmc",
                                               label = "Institution",
                                               choices = NULL),
                                   checkboxGroupInput(inputId="bmc_check",
                                                      label="Specific Check",
                                                      choices=NULL),
                                   # only show comparison option for overall metrics
                                   conditionalPanel(
                                     condition="input.largen_toggle == 2",
                                     radioButtons(inputId="comp_bmc_ln",
                                                  label="Comparison Type",
                                                  choices=list("Across Sites"=1,
                                                               "None"=0))
                                   )),
                                 mainPanel(
                                   fluidRow(
                                     # RxNorm
                                     box(title="Best Mapped Concept Proportions",
                                         #plotlyOutput("bmc_rxnorm_overall_plot"))
                                         plotlyOutput("bmc_overall_plot", height=500, width=1000)),
                                     tabBox(width=12,
                                            tabPanel("Concepts Considered Best",
                                                     "For each of the checks in the table below, the concepts listed are considered the best level to map to",
                                                     DT::dataTableOutput("bmc_conceptset_best")),
                                            tabPanel("Concepts Considered Not-Best",
                                                     "For each of the checks in the table below, the concepts listed are considered less than ideal levels to map to",
                                                     DT::dataTableOutput("bmc_conceptset_notbest")),
                                            tabPanel("Top 5 Not-Best-Mapped",
                                                     "For each site, the top 5 not-best-mapped concepts per check is displayed below",
                                                     DT::dataTableOutput("bmc_pp_top_nonbest")))
                                   )#fluidrow
                                 )#mainpanel

                               )#sidebarlayout
                      ),
                      # FOT ----
                      tabPanel("Facts Over Time",
                               sidebarLayout(
                                 sidebarPanel(
                                   dateInput("date_fot_min", label="Minimum Date", value="2010-01-01"),
                                   dateInput("date_fot_max", label="Maximum Date"),
                                   selectInput(inputId = "fot_domain",
                                               label="Choose Domain for Summary and Site-Specific Plots",
                                               choices = NULL),
                                   wellPanel(
                                     strong(helpText("Summary Plot")),
                                   selectInput(inputId='fot_subdomain_overall',
                                               label='Summary Plots: Specific Check',
                                               choices=NULL)),
                                   wellPanel(
                                     strong(helpText("Site Specific")),
                                   selectInput(inputId = "sitename_fot",
                                               label = "Select Site",
                                               choices =NULL),
                                   checkboxGroupInput(inputId="fot_subdomain_site",
                                                      label="Site Specific Plots: Specific Check",
                                                      choices=NULL),
                                   selectInput(inputId="fot_bounds",
                                               label="SD Bounds Option",
                                               choices=c('No Bounds',
                                                         1,
                                                         2,
                                                         3)))),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     #summary
                                     box(title="Summary Plot",width=12,
                                         plotlyOutput("fot_summary_plot")),
                                     box(title="Site Specific Facts Over Time",width=12,
                                         #plotlyOutput("fot_plot"))
                                         uiOutput("fot_plot"))
                                   )#fluidRow
                                 )#mainPanel
                               )#sidebarlayout
                      ),#tab panel fot
                      # DCON ----
                      tabPanel("Domain Concordance",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "sitename_dcon",
                                               label = "Select Site",
                                               choices = NULL),
                                   # only show comparison option for overall metrics
                                   conditionalPanel(
                                     condition="input.largen_toggle == 2",
                                     radioButtons(inputId="comp_dcon_ln",
                                                  label="Comparison Type",
                                                  choices=list("Across Sites"=1,
                                                               "None"=0))
                                   ),
                                   conditionalPanel(
                                     condition="input.largen_toggle == 1 || input.comp_dcon_ln ==0",
                                     selectInput(inputId = "denom_dcon",
                                                 label = "Select Denominator",
                                                 choices = c("Overall",
                                                             "Cohort 1",
                                                             "Cohort 2"))),
                                   checkboxGroupInput(inputId="dcon_check",
                                                      label="Check Description",
                                                      choices=NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     # description of cohorts
                                     box(title="Cohort Descriptions", width=12,
                                         DT::dataTableOutput("dcon_cohort_descr")),
                                     #plots
                                     box(title="Domain Concordance Overall",width=12,
                                         plotlyOutput("dcon_overall_plot"))
                                     )#fluidRow
                                 )#mainPanel
                               )#sidebarlayout
                      ),#tab panel fot
                      # MF ----
                      tabPanel("Facts with Associated Visit ID",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="sitename_mf_visitid",
                                               label="PEDSnet Institution",
                                               choices = NULL),
                                   checkboxGroupInput(inputId = "mf_visitid_domain",
                                               label="Choose Domain/s",
                                               choices = NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     box(title="Facts with Missing visit_occurrence_id", width=12,
                                         plotOutput("mf_visitid_overall")),
                                     box(title="Facts with Missing visit_occurrence_id", width=12,
                                         plotOutput("mf_visitid_bysite"))
                                   )#fluidRow
                                 )#mainPanel
                               )#sidebarlayout
                      ),
                      # ECP ----
                      tabPanel("Expected Concepts Present",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="sitename_ecp",
                                               label="PEDSnet Institution",
                                               choices = NULL),
                                   selectInput(inputId="ecp_check_cat",
                                               label="Concept Category",
                                               choices=c("Anthropometrics")),
                                 checkboxGroupInput(inputId="ecp_check",
                                                    label="Concept Group",
                                                    choices=NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     box(title="Overall Expected Concepts Present",
                                         width=12,
                                         plotlyOutput("ecp_plot", height=600)),
                                     box(title="Site-Specific Expected Concepts Present",
                                         width=12,
                                         plotOutput("ecp_plot_site"))
                                   )#fluidRow
                                 )#mainPanel
                               )#sidebarpanel
                      ),#tabpanel for ecp
                      # DP ----
                      tabPanel("Date Plausibility",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="sitename_dp",
                                               label="PEDSnet Institution",
                                               choices = NULL),
                                   # only show comparison option for overall metrics
                                   conditionalPanel(
                                     condition="input.largen_toggle == 2",
                                     radioButtons(inputId="comp_dp_ln",
                                                  label="Comparison Type",
                                                  choices=list("Across Sites"=1,
                                                               "None"=0))
                                   ),
                                   checkboxGroupInput(inputId="dp_check_desc",
                                                      label="Check Description",
                                                      choices=NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     box(title="Date Plausibility Distribution",
                                         width=12,
                                         plotlyOutput("dp_overall",height=600))
                                   )#fluidRow
                                 )#mainPanel
                               )#sidebarpanel
                      )#tabpanel for dp
           ),#navbarmenu
           tabPanel(title="Glossary",icon=icon("book"),
                    sidebarLayout(
                      sidebarPanel(tags$p("This page contains definitions for each of the DQ checks contained throughout the dashboard")),
                      mainPanel(DT::dataTableOutput("glossary"))))
)#navbarpage
)#fluidpage
)#shinyUI
