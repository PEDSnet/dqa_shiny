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
                                               choices = NULL),
                                   checkboxGroupInput(inputId="dc_subdomain",
                                                      label="Specific Check",
                                                      choices=NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     # DC changes - site comparison
                                     box(title="Overall Data Cycle Changes", width=12,
                                         plotOutput("dc_overall")),
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
                      tabPanel("Conformance",
                               sidebarLayout(
                                 sidebarPanel(
                                 selectInput(inputId = "sitename_conf",
                                             label = "Institution",
                                             choices = NULL)),
                                 # Begin main
                                 mainPanel(
                                   fluidRow(
                                     # Valueset conformance
                                     box(title="Valueset Conformance", width=12,
                                         plotOutput("vs_plot")),
                                     box(title="Valueset Violations", width=12,
                                         p("Violations per table and vocabulary"),
                                         tableOutput("vs_table")),
                                     # Vocabulary conformance
                                     box(title="Vocabulary Conformance", width=12,
                                         plotOutput("vc_plot")),
                                     box(title="Vocabulary Violations", width=12,
                                         p("Violations per table and vocabulary"),
                                         tableOutput("vc_table")),
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
                                     # Unmapped Concepts Proportions
                                     box(title="Unmapped Concepts Overall", width=12,
                                         plotOutput("uc_overall_plot")),
                                     box(title="Unmapped Concepts by Year", width=12,
                                         plotOutput("uc_yr_plot")),
                                     box(title="Top Unmapped Concepts", width=12,
                                         p("Top 10 unmapped concepts per table application"),
                                         DT::dataTableOutput("uc_top_tbl"))
                                   )#fluidrow
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
                                         plotlyOutput("pf_overall_heat_plot", height=500)),
                                     box(title="Mapping Abbreviations and Descriptions", width=12,
                                         DT::dataTableOutput("pf_mappings")),
                                     # Person facts/records by site
                                     box(title="Person Facts/Records By Site", width=12,
                                         plotOutput("pf_overall_bysite_plot", height=750))
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
                                     box(title="RxNorm Specificity", width=12,
                                         p("Rxnorm Hierarchy Preference:"),
                                         tags$ol(
                                           tags$li("BPCK (Branded Pack)"),
                                           tags$li("GPCK (Clinical Pack)"),
                                           tags$li("SBD (Branded Drug, Quant Branded Drug)"),
                                           tags$li("SDC (Clinical Drug, Quant Clinical Drug)"),
                                           tags$li("SBDF (Branded Drug Form)"),
                                           tags$li("SCDF (Clinical Drug Form)"),
                                           tags$li("MIN (Ingredient)"),
                                           tags$li("SBDC"),
                                           tags$li("SCDC"),
                                           tags$li("PIN (Ingredient)"),
                                           tags$li("IN (Ingredient)")
                                         ),
                                         p("Preferred levels are 1-4"),
                                         plotlyOutput("bmc_rxnorm_overall_plot"))
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
                                         plotOutput("fot_summary_plot")),
                                     box(title="Site Specific Facts Over Time",width=12,
                                         plotOutput("fot_plot"))
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
                                   sliderInput("date_dcon_range",
                                               label="Date Range",
                                               min=1990L,
                                               max=as.integer(format(Sys.Date(), "%Y")),
                                               value=c(2010L,2022L),
                                               step=1L,
                                               sep=""),
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
                                         plotOutput("dcon_overall_plot")),
                                     box(title="Domain Concordance over Time",width=12,
                                         plotOutput("dcon_time_plot"))
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
                      )#tabpanel for mf
           )#navbarmenu
)#navbarpage
)#fluidpage
)#shinyUI
