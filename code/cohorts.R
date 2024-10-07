#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'

mask_site <- function(dat){
  site_nums <- dat %>%
    distinct(site)
  site_nums <- site_nums[sample(1:nrow(site_nums)),]%>% # re-order so not alphabetically arranged
    filter(site!='pedsnet_total'&site!='all')%>%
    mutate(sitenum=as.character(row_number()))
  
  dat %>%
    left_join(site_nums, by = 'site')%>%
    mutate(site_masked=case_when(site=='pedsnet_total'|site=='all'~'pedsnet_total',
                                 TRUE~paste0("Site ",sitenum)))%>%
    select(-site)%>%
    rename(site=site_masked)
}