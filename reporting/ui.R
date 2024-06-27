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
                        tags$p("Contact pedsnetdcc@chop.edu for more information")
                      ),
                      mainPanel(
                        tags$p("The PEDSnet Data Quality Dashboard (PDQD) provides output from the data quality program developed and maintained
                                                 by the PEDSnet Data Quality team."),
                        tags$p("The PDQD is displayed by check type. A check type is a category of checks that can be
                                                 executed across a wide variety of fields, domains, tables, or other applications.
                                                 Below are descriptions of each check type."),
                        DT::dataTableOutput("dt_descriptions")
                      ))),
           navbarMenu(title="Check Type", icon=icon("chart-area"),
                      tabPanel("Data Cycle Changes (DC)",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="sitename_dc",
                                               label="Institution",
                                               choices = NULL),
                                   selectInput(inputId = "dc_domain",
                                               label="Domain",
                                               choices = c('adt')), # set a default to avoid empty string detection
                                   checkboxGroupInput(inputId="dc_subdomain",
                                                      label="Specific Check",
                                                      choices=NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     # DC changes - site comparison
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
                      tabPanel("Valueset Conformance",
                               sidebarLayout(
                                 sidebarPanel(
                                 selectInput(inputId = "sitename_vs_conf",
                                             label = "Institution",
                                             choices = NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     p("Note that sites only show up if there is at least one violation"),
                                     box(title="Valueset Violations Plot", width=12, plotOutput("vs_plot")),
                                     box(title="Violations Listings", width=12, DT::dataTableOutput("vs_table"))
                                     )
                                 )
                               )),
                      tabPanel("Vocabulary Conformance",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="sitename_vc_conf",
                                               label="Institution",
                                               choices=NULL)),
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
                      tabPanel("Unmapped Concepts",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "sitename_uc",
                                               label = "Institution",
                                               choices = NULL),
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
                                                p("proportion_of_unmapped is the count of the given source value divided by the number of unmapped rows for that column"),
                                                DT::dataTableOutput("uc_top_tbl", width=1000)))),
                                    fluidRow(box(title="Unmapped Concepts by Year", width=12,
                                         plotOutput("uc_yr_plot", height=600)))
                                 )#mainpanel

                               )#sidebarlayout
                      ),
                      tabPanel("Person Facts/Records",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "sitename_pf",
                                               label = "Institution",
                                               choices = NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     # Person facts/records overall
                                     box(title="Person Facts/Records Overall", width=12,
                                         plotlyOutput("pf_overall_heat_plot", height=750)),
                                     box(title="Mapping Abbreviations and Descriptions", width=12,
                                         DT::dataTableOutput("pf_mappings")),
                                     # Person facts/records by site
                                     box(title="Person Facts/Records By Site", width=12,
                                         plotOutput("pf_overall_bysite_plot", height=1000))
                                   )#fluidrow
                                 )#mainpanel

                               )#sidebarlayout
                      ),#tabpanel
                      tabPanel("Best Mapped Concepts",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "sitename_bmc",
                                               label = "Institution",
                                               choices = NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     # RxNorm
                                     box(title="Best Mapped Concept Proportions",
                                         #plotlyOutput("bmc_rxnorm_overall_plot"))
                                         plotlyOutput("bmc_overall_plot", height=750, width=1000)),
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
                      tabPanel("Facts Over Time",
                               sidebarLayout(
                                 sidebarPanel(
                                   dateInput("date_fot_min", label="Minimum Date", value="2010-01-01"),
                                   dateInput("date_fot_max", label="Maximum Date"),
                                   selectInput(inputId = "fot_domain",
                                               label="Choose Domain for Summary and Site-Specific Plots",
                                               choices = NULL),
                                   selectInput(inputId='fot_subdomain_overall',
                                               label='Summary Plots: Specific Check',
                                               choices=NULL),
                                   checkboxGroupInput(inputId="fot_subdomain_site",
                                                      label="Site Specific Plots: Specific Check",
                                                      choices=NULL),
                                   selectInput(inputId = "sitename_fot",
                                               label = "Select Site",
                                               choices =NULL),
                                   selectInput(inputId="fot_bounds",
                                               label="SD Bounds Option",
                                               choices=c('No Bounds',
                                                         1,
                                                         2,
                                                         3))),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     #summary
                                     box(title="Summary Plots",width=12,
                                         plotlyOutput("fot_summary_plot")),
                                     box(title="Site Specific Facts Over Time",width=12,
                                         plotlyOutput("fot_plot"))
                                   )#fluidRow
                                 )#mainPanel
                               )#sidebarlayout
                      ),#tab panel fot
                      tabPanel("Domain Concordance",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "sitename_dcon",
                                               label = "Select Site",
                                               choices = NULL),
                                   selectInput(inputId = "denom_dcon",
                                               label = "Select Denominator",
                                               choices = c("Overall",
                                                           "Cohort 1",
                                                           "Cohort 2")),
                                   checkboxGroupInput(inputId="dcon_check",
                                                      label="Specific Check",
                                                      choices=NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     # description of cohorts
                                     box(title="Cohort Descriptions", width=12,
                                         DT::dataTableOutput("dcon_cohort_descr")),
                                     #plots
                                     box(title="Domain Concordance Overall",width=12,
                                         plotlyOutput("dcon_overall_plot")),
                                     )#fluidRow
                                 )#mainPanel
                               )#sidebarlayout
                      ),#tab panel fot
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
                      tabPanel("Expected Concepts Present",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId="sitename_ecp",
                                               label="PEDSnet Institution",
                                               choices = NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     box(title="Overall Expected Concepts Present",
                                         width=12,
                                         plotOutput("ecp_plot")),
                                     box(title="Site-Specific Expected Concepts Present",
                                         width=12,
                                         plotOutput("ecp_plot_site"))
                                   )#fluidRow
                                 )#mainPanel
                               )#sidebarpanel
                      )#tabpanel for ecp
           )#,#navbarmenu
           # tabPanel(title="SSDQA Issues", icon=icon("square-check"),
           #          sidebarLayout(
           #            sidebarPanel(
           #              selectInput(inputId = "ssdqa_domain",
           #                          label = "Select Domain",
           #                          choices = NULL)
           #            ),
           #            mainPanel(
           #              DT::DTOutput("ssdqa_issues_table")
           #            )))
)#navbarpage
)#fluidpage
)#shinyUI
