library(shiny)
library(markdown)
library(shinydashboard)
library(plotly)
library(gt)
library(ggplot2)
library(sysfonts)
library(showtext)
library(stringr)
library(knitr)
library(scales)
library(DT)
library(tidyr)
library(RColorBrewer)
library(forcats)
source('../site/run.R')

#' Function for replacing site names with masked identifiers
#' if mask_site is set to TRUE in run.R
res <- function(tbl_name) {
  rslt <- results_tbl(tbl_name) %>% collect()
  if (config('mask_site')) {
    rslt <- mutate(rslt, site =
                     case_when(str_detect(site_anon, '[0-9]') ~
                                 paste0(str_extract(site_anon, '^[^0-9]+'),
                                        sprintf('%02d',
                                                as.integer(str_extract(site_anon,
                                                                       '[0-9]+')))),
                               TRUE ~ site_anon))
  }
  rslt
}

#' Function for plotting site-specific FOT plots
#' with counts on one axis and normalized counts on the other
plot_fot_fn <- function(data) {
  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "Deviance Heuristic")


  data%>%
    plotly::plot_ly(
      x = ~time_end,
      y = ~row_cts,
      yaxis="y1",
      name = "Row Counts",
      type="scatter",
      mode="lines") %>%
    plotly::add_trace(
      x=~time_end,
      y = ~check,
      type="scatter",
      mode="lines",
      yaxis="y2",
      name = "Deviance Heuristic",
      line=list(color='navy', dash='dot'))%>%
    layout(
      yaxis2 = ay,
      xaxis = list(title="Month End"),
      yaxis = list(title="Row Count"))

}
