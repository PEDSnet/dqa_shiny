library(shiny)
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
      x = ~month_end,
      y = ~row_cts,
      yaxis="y1",
      name = "Row Counts",
      type="scatter",
      mode="lines") %>%
    plotly::add_trace(
      x=~month_end,
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

shinyServer(function(input, output) {
  # DATA CAPTURE -------
  ## data cycle changes
  ### pp data
  dc_output_all <- reactive({
    if(input$largen_toggle==1){
      res('dc_output_pp')
        #%>%filter(domain!='measurement_anthro')
    }else{res('dc_output_ln')#%>%filter(domain!='measurement_anthro'
      }
  })

  ### adjust available site name
  observeEvent(dc_output_all(), {
    if(input$largen_toggle==1){
      choices_new<-c("total",unique(dc_output_all()$site)%>%sort())
    }else{choices_new<-unique(filter(dc_output_all(),site!='total')$site)%>%sort()
    }
    updateSelectInput(inputId="sitename_dc", choices=choices_new)
  })

  ### update choices for domain
  observeEvent(dc_output_all(), {
    choices_pp<-gsub("_.*","",dc_output_all()$domain)
    choices_new<-unique(choices_pp)%>%
      sort()
    updateSelectInput(inputId="dc_domain", choices=choices_new)
  })

  ### filter data for plotting
  dc_output <- reactive({
    dc_output_all()%>%
      filter(str_detect(domain,input$dc_domain))
  })

  ### update choices for subdomain
  observeEvent(dc_output(), {
    choices_new<-unique(dc_output()$domain)
    updateCheckboxGroupInput(inputId="dc_subdomain", choices=choices_new)
  })


  ## vocabulary conformance ------------
  ### pp data
  vc_output <- reactive({
    if(input$largen_toggle==1){
    res('vc_output_pp')%>%
      mutate(prop_viol=round(tot_prop,2))
    }else{res('vc_output_ln')}
  })
  vc_vocablevel<-reactive({
    res('vc_output_pp')%>%
      mutate(prop_viol=round(tot_prop,2))
  })
  # vc_violations <- reactive({
  #   res('vc_violations_pp') %>%
  #     mutate(prop_viol=round(tot_prop,2))
  # })
  ## adjust available site name
  observeEvent(vc_output(), {
    if(input$largen_toggle==1){
      choices_new<-c("total",unique(vc_output()$site)%>%sort())
    }else{choices_new<-unique(filter(vc_output(),site!='total')$site)%>%sort()
    }
    updateSelectInput(inputId="sitename_vc_conf", choices=choices_new)
  })

  ### acceptable vocabularies for vc
  vc_vocabs_accept <- results_tbl('dqa_check_metadata') %>%
    filter(check_type=='Vocabulary Conformance')%>%
    collect()%>%
    mutate(acceptable_vocabulary=gsub(".*:","",full_description))%>%
    select(check_domain, check_application, acceptable_vocabulary)


  ## valueset conformance ------
  vs_output <- reactive({
    if(input$largen_toggle==1){
      res('vs_output_pp')%>%
        mutate(prop_viol=round(tot_prop,2))
    }else{res('vs_output_ln')}
  })
  vs_vocablevel<-reactive({
    res('vs_output_pp')%>%
      mutate(prop_viol=round(tot_prop,2))
  })
  ### adjust available site name
  observeEvent(vs_output(), {
    if(input$largen_toggle==1){
      choices_new<-c("total",unique(vs_output()$site)%>%sort())
    }else{choices_new<-unique(filter(vs_output(),site!='total')$site)%>%sort()
    }
    updateSelectInput(inputId="sitename_vs_conf", choices=choices_new)
  })

## unmapped concepts -----
  ### pp data
  uc_output <- reactive({
    if(input$largen_toggle==1){
      res('uc_output_pp')
    }else{
      res('uc_output_ln')
    }
  })
  uc_yr_output <- reactive({
    if(input$largen_toggle==1){
    res('uc_by_year_pp') %>%
      mutate(year_date=as.integer(year_date))
    }else{
      res('uc_by_year_ln')%>%
        mutate(year_date=as.integer(year_date))
    }
  })
  uc_top_output <- reactive({
    results_tbl('uc_grpd_pp') %>% collect()%>%
      group_by(site, unmapped_description) %>%
      slice_max(order_by = src_value_ct, n=10) %>%
      ungroup()%>%
      mutate(src_value_ct=as.integer(src_value_ct))%>%
      inner_join(select(uc_output(),c(site, unmapped_rows, measure)),
                 by = c("site", "unmapped_description"="measure"))%>%
      mutate(proportion_of_unmapped=round(src_value_ct/unmapped_rows,2))
  })
  uc_top_output_overall <- reactive({
    results_tbl('uc_grpd_pp') %>%collect()%>%
      group_by(unmapped_description)%>%
      slice_max(order_by=src_value_ct, n=10)%>%
      ungroup()%>%
      mutate(src_value_ct=as.integer(src_value_ct))%>%
      inner_join(
        uc_output()%>%filter(site=='total')%>%select(unmapped_rows,measure),
        by = c('unmapped_description'='measure'))%>%
      mutate(proportion_of_unmapped=round(src_value_ct/unmapped_rows,2))
  })
  ### adjust available site name
  observeEvent(uc_output(), {
    if(input$largen_toggle==1){
      choices_new<-c("total",unique(uc_output()$site)%>%sort())
    }else{choices_new<-unique(filter(uc_output(),site!='total')$site)%>%sort()
    }
    updateSelectInput(inputId="sitename_uc", choices=choices_new)
  })

  ## person facts/records -------
  ### capture data
  pf_output <- reactive({
    if(input$largen_toggle==1){
      res('pf_output_pp')
    }else{res('pf_output_ln')}
  })
  ### adjust available site name
  observeEvent(pf_output(), {
    if(input$largen_toggle==1){
      choices_new<-c("total",unique(pf_output()$site)%>%sort())
    }else{choices_new<-unique(filter(pf_output(),site!='total')$site)%>%sort()
    }
    updateSelectInput(inputId="sitename_pf", choices=choices_new)
  })

  ## best mapped concepts ----
  ### fetch data
  top_rolled <-  results_tbl('bmc_gen_output_concepts_pp')%>%
    filter(include_new==0)%>%
    collect()%>%
    group_by(site, check_desc)%>%
    slice_max(., n=5, order_by=row_proportions) %>%
    select(check_desc,concept)%>%
    ungroup()%>%
    group_by(site, check_desc)%>%
    summarize(named_vec = list(concept))%>%
    unnest_wider(named_vec, names_sep="_")%>%
    unite("top5", -c(site, check_desc), sep=", ", na.rm=TRUE)

  bmc_pp <- reactive({
    if(input$largen_toggle==1){
      res('bmc_gen_output_pp')%>%
        left_join(top_rolled, by = c('site', 'check_desc'))
    }else{
      res('bmc_gen_output_ln')%>%
        left_join(top_rolled, by = c('site', 'check_desc'))
    }
  })
  bmc_pp_concepts <- reactive({
    res('bmc_gen_output_concepts_pp')
  })
  # adjust available site name
  observeEvent(bmc_pp(), {
    if(input$largen_toggle==1){
      choices_new<-c("total",unique(bmc_pp()$site)%>%sort())
    }else{choices_new<-unique(filter(bmc_pp(),site!='total')$site)%>%sort()
    }
    updateSelectInput(inputId="sitename_bmc", choices=choices_new)
  })
  # find the top 5 per check/site
  bmc_pp_top <- reactive({
    bmc_pp_concepts()%>%
      filter(site==input$sitename_bmc&include_new==0)%>%
      collect()%>%
      group_by(check_desc)%>%
      slice_max(., n=5, order_by=row_proportions) %>%
      select(check_desc, concept, row_proportions)
  })


  # pull in the listing of concepts that are considered best/notbest
  bmc_conceptset <- results_tbl('bmc_conceptset')%>%filter(!is.na(include))%>%collect()
  # set up the BMC tables that will be displayed
  output$bmc_conceptset_best<-DT::renderDT({

    bmc_conceptset%>%
      filter(include==1)%>%
      select(check_desc, concept)
  })

  output$bmc_conceptset_notbest<-DT::renderDT({
    bmc_conceptset%>%
      filter(include==0)%>%
      select(check_desc, concept)
  })

  output$bmc_pp_top_nonbest <- DT::renderDT({
    bmc_pp_top()%>%
      mutate(row_proportions=round(row_proportions,2))
  })

  ## facts over time ------
  # capture data
  fot_output_summary_ratio <- reactive({res('fot_output_mnth_ratio_pp')})
  # # update domain choices
  observeEvent(fot_output_summary_ratio(), {
    choices_new_fot<-unique(fot_output_summary_ratio()$domain)%>%sort()
    updateSelectInput(inputId="fot_domain", choices=choices_new_fot)
  })
  # # update subdomains
  observeEvent(input$fot_domain, {
    choices_new_fot<-unique((fot_output_summary_ratio()%>%filter(domain==input$fot_domain))$check_desc)%>%sort()
    updateSelectInput(inputId="fot_subdomain_overall", choices=choices_new_fot)
  })

  # fot_output_heuristic <- reactive({results_tbl('fot_heuristic_summary_pp') %>%
  #     filter(domain!='labs')%>% # remove in future versions
  #     collect()%>%
  #     mutate(site=case_when(config('mask_site')~site_anon,
  #                           TRUE~site))
  # })

  # update choices for domain
  # observeEvent(fot_output_heuristic(), {
  #   choices_new_fot<-unique(fot_output_heuristic()$domain)%>%sort()
  #   updateSelectInput(inputId="fot_domain", choices=choices_new_fot)
  # })

  # limit table to domain selected for specific check choices

  fot_output <- reactive({res('fot_heuristic_pp') %>%
      inner_join(res('fot_heuristic_summary_pp'),
                 by=c('domain','check_name', 'site','site_anon', 'sitenum')) %>%
      inner_join(select(res('fot_output_mnth_ratio_pp'),
                        c(row_cts, check_name, domain, site, month_end)),
                 by = c('check_name', 'domain', 'site', 'month_end'))%>%
      collect() %>%
      filter(domain==input$fot_domain)
  })
  fot_output_site <- reactive({
      fot_output()%>%
      filter(domain==input$fot_domain,
             site==input$sitename_fot,
             check_desc%in%input$fot_subdomain_site)
  })

  # adjust available site name
  observeEvent(fot_output(), {
    choices_new<-unique(filter(fot_output(),site!='all')$site)%>%sort()
    updateSelectInput(inputId = "sitename_fot", choices=choices_new)
  })
  # adjust available check description for site plots
  observeEvent(fot_output(), {
    choices_new_fot<-unique(fot_output()$check_desc)%>%sort()
    updateCheckboxGroupInput(inputId="fot_subdomain_site", choices=choices_new_fot)
  })
  # adjust available check description for overall plots
  observeEvent(fot_output(), {
    choices_new_fot<-unique(fot_output()$check_desc)%>%sort()
    updateSelectInput(inputId="fot_subdomain_overall", choices=choices_new_fot)
  })

  ## domain concordance -----
  # capture data
  dcon_output <- reactive({
    if(input$largen_toggle==1){
      indat<-res('dcon_output_pp')
    }else{
      indat<-res('dcon_output_ln')
    }
    indat%>%
      mutate(cohort=factor(cohort, levels=c("cohort_2_only", "combined", "cohort_1_only",
                                            "cohort_1_denom", "cohort_2_in_1",
                                            "cohort_2_denom", "cohort_1_in_2"),
                           labels=c("cohort 2 only", "combined", "cohort 1 only",
                                    "cohort 1 not 2", "cohort 2 in 1",
                                    "cohort 2 not 1", "cohort 1 in 2")))
  })

  observeEvent(dcon_output(), {
    choices_new<-unique(dcon_output()$check_name)
    updateCheckboxGroupInput(inputId="dcon_check", choices=choices_new)
  })

  dcon_meta <- reactive({
    results_tbl('dcon_meta')%>%
      collect()%>%
      select(-check_type)%>%
      filter(check_name%in%input$dcon_check)%>%
      pivot_wider(names_from='cohort_label',
                  values_from='cohort')
  })
  # adjust available site names
  observeEvent(dcon_output(), {
    if(input$largen_toggle==1){
      choices_new<-c("total",unique(dcon_output()$site)%>%sort())
    }else{choices_new<-unique(filter(dcon_output(),site!='total')$site)%>%sort()
    }
    updateSelectInput(inputId="sitename_dcon", choices=choices_new)
  })

  # descriptions of cohorts
  output$dcon_cohort_descr <- renderDT(
    DT::datatable(
      dcon_meta(),
      options=list(pageLength=5),
      rownames=FALSE
    )
  )
  ## facts with missing visit ids -------------
  # connect with data
  mf_visitid_output <- reactive({
    if(input$largen_toggle==1){
      res('mf_visitid_pp')
    }else{
      res('mf_visitid_ln')
    }
  })

  # update choices for site name
  observeEvent(mf_visitid_output(), {
    if(input$largen_toggle==1){
      choices_new<-c("total",unique(filter(mf_visitid_output(),site!='total')$site)%>%sort())
    }else{choices_new<-choices_new<-unique((mf_visitid_output()%>%filter(site!='total'))$site)%>%sort()}
    updateSelectInput(inputId="sitename_mf_visitid", choices=choices_new)
  })

  ### adjust available site name for large n site comparison
  observeEvent(mf_visitid_output(), {
    choices_new<-unique((mf_visitid_output()%>%filter(site!='total'))$site)%>%sort()
    updateSelectInput(inputId="sitename_mf_ln", choices=choices_new)
  })

  # update choices for domain
  observeEvent(mf_visitid_output(), {
    choices_new<-unique(mf_visitid_output()$domain)%>%sort()
    updateCheckboxGroupInput(inputId="mf_visitid_domain", choices=choices_new)
  })

  ## expected concepts present ---------------------
  ecp_output <- reactive({
    if(input$largen_toggle==1){res('ecp_output_pp')}else{res('ecp_output_ln')}
  })
  # update choices for site name
  observeEvent(ecp_output(), {
    choices_new<-unique(filter(ecp_output(),site!='total')$site)%>%sort()
    updateSelectInput(inputId="sitename_ecp", choices=choices_new)
  })


  # other configurations ----------------------

  # set site colors based on table where all sites expected
  site_list<-(res("dc_output_pp"))$site %>% unique() %>% append('allsite_mean') %>% append('allsite_median')
  #ramp_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(length(site_list))
  ramp_palette<-pedsn_dq_pal(palette="main", reverse=FALSE)(length(site_list))
  # randomized_palette <- ramp_palette[sample(1:length(ramp_palette))]
  site_colors <- setNames(ramp_palette, site_list)

  dc_mappings <- results_tbl('dc_mappings')%>%collect()

  pf_mappings <- results_tbl('pf_mappings') %>%collect()%>%
    mutate(`Visit Type`=case_when(str_detect(Label, "all")~"all",
                                  str_detect(Label, "op")~"outpatient",
                                  str_detect(Label, "long_ip")~"long_inpatient",
                                  str_detect(Label, "ip")~"inpatient",
                                  str_detect(Label, "ed")~"emergency"))%>%
    select(`Visit Type`, Description)

  df_check_descriptions <- read_codeset('check_type_descriptions','cc')



  # DESCRIPTIONS
  #http://cran.nexr.com/web/packages/DT/DT.pdf
  output$dt_descriptions <- DT::renderDT(
    DT::datatable(
      df_check_descriptions,
      options=list(pageLength=5),
      rownames=FALSE
    )
  )

  # PLOTS ------
  ## CHANGES BETWEEN DATA CYCLES ------------------------------
  output$dc_mappings <- DT::renderDT(
    DT::datatable(
      dc_mappings,
      options=list(pageLength=5),
      rownames=FALSE
    )
  )

  # changes between data cycles - record counts
  output$dc_domain_split <- renderPlot({
    if(length(input$dc_subdomain)==0){
      showplot <- ggplot()+
        geom_blank()+
        annotate("text", label="Select a domain and specific check", x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
    }else if(input$largen_toggle==2&input$comp_dc_ln==1){
      # summary metrics: comparison
      showplot<-ggplot(filter(dc_output_all(),domain%in%input$dc_subdomain&site==input$sitename_dc&application=="rows"),
                       aes(x=domain,y=prop_total_change))+
        geom_bar(stat="identity",aes(fill=site))+
        geom_errorbar(aes(ymin=q1,ymax=q3))+
        geom_point(aes(x=domain,y=median_val),shape=23,size=3)+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1,size=12),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16),
              legend.position = "none")+
        labs(x="Site",
             y="Record Proportion Change")
    }else if(input$sitename_dc=="total"){
      showplot<-ggplot(filter(dc_output(),domain%in%input$dc_subdomain&application=='rows'),
                       aes(x=site,y=prop_total_change, fill=site))+
        geom_bar(stat='identity')+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1,size=12),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16),
              legend.position = "none")+
        facet_wrap(~domain)+
        labs(x="Site",
             y="Record Proportion Change")
    }else{
      # individual sites OR summary metrics w/o comparison
      tc_prev<-paste0('total_ct_',config('db_previous'))
      tc_new<-paste0('total_ct_',config('db_current'))
      showplot<- ggplot(filter(dc_output(),domain%in%input$dc_subdomain&
                                 site==input$sitename_dc&
                                 application=='rows'), aes(x=site,y=prop_total_change))+
        geom_segment(aes(x=site,xend=site,y=0,yend=prop_total_change))+
        geom_label(aes(x=site, y=0, label=paste0("Previous Record Count: ",format(!!sym(tc_prev), big.mark = ",",scientific = FALSE))))+
        geom_label(aes(x=site, y=prop_total_change, label=paste0("New Record Count: ",format(!!sym(tc_new), big.mark = ",",scientific = FALSE))))+
        labs(x="Site",
             y="Records Proportion Change")+
        facet_wrap(~domain)+
        theme_bw()
    }
    return(showplot)
  })

  # changes between data cycles - person counts
  output$dc_domain_split_persons <- renderPlot({
    # blank plot if nothing selected
    if(length(input$dc_subdomain)==0){
      showplot <- ggplot()+
        geom_blank()+
        annotate("text", label="Select a domain and specific check", x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
      # blank plot if check not applicable to person level
    }else if(any(input$dc_subdomain=='specialty')|any(input$dc_subdomain=='care_site')){
      showplot <- ggplot()+
        geom_blank()+
        annotate("text", label="Check not applicable at person level", x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
      # overall (only applicable to individual site config)
    }else if(input$sitename_dc=="total"){
      showplot<-ggplot(filter(dc_output_all(),domain%in%input$dc_subdomain&
                                application=='person'),
                       aes(x=site,y=prop_total_change, fill=site))+
        geom_bar(stat='identity')+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1,size=12),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16),
              legend.position = "none")+
        facet_wrap(~domain)+
        labs(x="Site",
             y="Person Proportion Change")
      # comparison across sites
    }else if(input$largen_toggle==2&input$comp_dc_ln==1){
      showplot<-ggplot(filter(dc_output_all(),domain%in%input$dc_subdomain&site==input$sitename_dc&application=="person"),
                       aes(x=domain,y=prop_total_change))+
        geom_bar(stat="identity",aes(fill=site))+
        geom_errorbar(aes(ymin=q1,ymax=q3))+
        geom_point(aes(x=domain,y=median_val),shape=23,size=3)+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1,size=12),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16),
              legend.position = "none")+
        labs(x="Site",
             y="Person Proportion Change")
      # individual site distribution
    }else{
      tc_prev<-paste0('total_ct_',config('db_previous'))
      tc_new<-paste0('total_ct_',config('db_current'))
      # pc_prev<-paste0('total_pt_ct_', config('db_previous'))
      #pc_new<-paste0('total_pt_ct_',config('db_current'))
      showplot<- ggplot(filter(dc_output(),domain%in%input$dc_subdomain&
                                 site==input$sitename_dc&application=='person'),
                        aes(x=site,y=prop_total_change))+
        geom_segment(aes(x=site,xend=site,y=0,yend=prop_total_change))+
        geom_label(aes(x=site, y=0, label=paste0("Previous Person Count: ",format(!!sym(tc_prev), big.mark = ",",scientific = FALSE))))+
        geom_label(aes(x=site, y=prop_total_change, label=paste0("New Person Count: ",format(!!sym(tc_new), big.mark = ",",scientific = FALSE))))+
        labs(x="Site",
             y="Person Proportion Change")+
        facet_wrap(~domain)+
        theme_bw()
    }
    return(showplot)
  })

  # overall plot
  output$dc_overall <- renderPlotly({
    if(input$sitename_dc=='total'){
      indata <- filter(dc_output_all(), application=='rows')
    }else{
      indata <- filter(dc_output_all(),site==input$sitename_dc&
                         application=='rows')
    }
    tc_prev<-paste0('total_ct_',config('db_previous'))
    tc_new<-paste0('total_ct_',config('db_current'))
    if(input$largen_toggle==1|(input$largen_toggle==2&input$comp_dc_ln==0)){
      plt<-ggplot(indata%>%mutate(
        text=paste0("site: ",site,
                    "\ndomain: ",domain,
                    "\nproportion change: ",prop_total_change,
                    "\nprevious count: ", format(!!sym(tc_prev),big.mark=","),
                    "\ncurrent count: ", format(!!sym(tc_new),big.mark=","))),
        aes(x=site, y=domain, fill=plot_prop, text=text))+
        geom_tile()+
        scale_fill_pedsn_dq(palette="diverging", discrete=FALSE)+
        guides(fill=guide_colorbar(title="Proportion\nTotal Change"))+
        theme_bw()+
        theme(axis.text.y= element_text(hjust=1,size=9),
              axis.text.x = element_text(hjust=1,vjust=0.5,angle = 90,size=12),
              axis.title=element_text(size=16))
    }else{
      plt<-ggplot(indata%>%mutate(
        text=paste0("site: ",site,
                    "\ndomain: ",domain,
                    "\nproportion change: ",prop_total_change,
                    "\noverall median (Q1, Q3): ", median_val, " (", q1, ", ", q3, ")")),
        aes(x=domain, text=text)
      )+
        geom_bar(aes(y=prop_total_change, fill=site), stat="identity")+
        scale_fill_manual(values=site_colors)+
        geom_linerange(aes(ymin=q1, ymax=q3))+
        geom_point(aes(y=median_val), shape=23, size=1)+
        theme_bw()+
        labs(y="Proportion Total Change")+
        theme(legend.position="none")+
        coord_flip()

    }
    return(ggplotly(plt, tooltip="text"))
  })

  ## VALUE SET CONFORMANCE ------
  output$vs_plot <- renderPlotly({
    if(input$sitename_vs_conf=='total'){
      outplot<-ggplot(filter(vs_output(),!accepted_value),
                      aes(x=site, y=tot_prop, fill=vocabulary_id, text=tot_prop))+
        geom_bar(stat="identity",position="dodge")+
        scale_fill_pedsn_dq()+
        facet_wrap(~measurement_column, scales="free_x")+
        ylim(0,1)+
        theme_bw()+
        labs(y="Proportion of Total Records",
             title="Violating Records per Column")+
        theme(axis.text.x=element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.x=element_text(size=14),
              axis.title.y=element_text(size=14))
      }else if((input$largen_toggle==1|input$comp_vs_ln==0)&
                nrow(filter(vs_vocablevel(), site==input$sitename_vs_conf&!accepted_value))>0){
        outplot<-ggplot(filter(vs_vocablevel(),site==input$sitename_vs_conf&!accepted_value), aes(x=measurement_column, y = tot_prop, fill = vocabulary_id,text=tot_prop)) +
          geom_bar(stat="identity", position="dodge") +
          geom_label(aes(x=measurement_column, y=tot_prop, label=format(tot_ct, big.mark=",")),
                     position=position_dodge(),
                     show.legend = FALSE)+
          ylim(0, 1)+
          facet_wrap(~table_application, scales="free")+
          theme_bw()+
          labs(x="Column Name",
               y="Proportion of Total Records",
               title="Violating Records per Column")+
          theme(axis.text.x=element_text(size=14),
                axis.text.y=element_text(size=14),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14))
      }
    # else if(input$comp_vs_ln==0&
    #            nrow(filter(vs_output(), site==input$sitename_vs_conf&!accepted_value))>0){
    #     outplot<-ggplot(filter(vs_output(),site==input$sitename_vs_conf&!accepted_value), aes(x=measurement_column, y = prop_viol, fill = vocabulary_id,text=tot_prop)) +
    #       geom_bar(stat="identity", position="dodge") +
    #       geom_label(aes(x=measurement_column, y=prop_viol, label=format(prop_viol, big.mark=",")),
    #                  position=position_dodge(),
    #                  show.legend = FALSE)+
    #       ylim(0, 1)+
    #       theme_bw()+
    #       labs(x="Column Name",
    #            y="Proportion of Total Records",
    #            title="Violating Records per Column")+
    #       theme(axis.text.x=element_text(size=14),
    #             axis.text.y=element_text(size=14),
    #             axis.title.x=element_text(size=14),
    #             axis.title.y=element_text(size=14))
    #   }
    else if(input$largen_toggle==2&input$comp_vs_ln==1){
        outplot<-ggplot(filter(vs_output(), site==input$sitename_vs_conf)%>%
                          mutate(text=paste0("proportion: ",round(prop_viol, 3),
                                             "\nmedian (Q1, Q3): ",round(median_val,3), " (", round(q1,2), ", ", round(q3,3), ")")),
                        aes(x=measurement_column, text=text))+
          geom_bar(aes(y=prop_viol, fill=site),stat="identity")+
          geom_linerange(aes(ymin=q1, ymax=q3))+
          geom_point(aes(y=median_val), shape=23, size=1)+
         # facet_wrap(~table_application, scales="free")+
          labs(x = "Column",
               y="Proportion Violating Records")+
          theme_bw()+
          scale_fill_manual(values=site_colors)+
          coord_flip()+
          theme(legend.position = "none",
                axis.text.x=element_text(size=14),
                axis.text.y=element_text(size=14),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14))
      }else{
      outplot <- ggplot()+
        geom_blank()+
        annotate("text", label="No violations", x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
    }
    return(ggplotly(outplot,tooltip="text"))
  })
  output$vs_table <- DT::renderDT({
    if(input$sitename_vs_conf=='total'){
      outtable <-filter(vs_output(), !accepted_value)%>%
        mutate(total_violations=format(tot_ct, big.mark=','),
               total_rows=format(total_denom_ct, big.mark=','))%>%
        select(site, table_application, vocabulary_id, total_violations, total_rows)
    }
    else if(nrow(filter(vs_vocablevel(), site==input$sitename_vs_conf&!accepted_value))!=0){
      outtable <-filter(vs_vocablevel(), site==input$sitename_vs_conf&!accepted_value)%>%
        mutate(total_violations=format(tot_ct, big.mark=','),
               total_rows=format(total_denom_ct, big.mark=','))%>%
        select(table_application, vocabulary_id, total_violations, total_rows)
    }
    else{
      outtable <- tibble(None="")
    }
    return(outtable)
  })
  ## VOCABULARY CONFORMANCE -----
  output$vc_overall_plot <- renderPlotly({
    if(input$sitename_vc_conf=='total'){
      plt<-ggplot(vc_output()%>%
                    mutate(text=paste0("site: ",site,
                                       "\nvocabulary: ",vocabulary_id,
                                       "\nproportion: ",prop_viol)),
                  aes(x=site,y=tot_prop, fill=vocabulary_id, text=text))+
        geom_bar(stat="identity",position="stack")+
        scale_fill_pedsn_dq()+
        facet_wrap(~table_application*measurement_column)+
        theme_bw()+
        theme(axis.text.x = element_text(angle=90))
    }else if(input$largen_toggle==2&input$comp_vc_ln==1){
      plt<-ggplot(filter(vc_output(), site==input$sitename_vc_conf)%>%
                      mutate(text=paste0("proportion: ",round(prop_viol, 2),
                                         "\nmedian (Q1, Q3): ",round(median_val,2), " (", round(q1,2), ", ", round(q3,2), ")")),
                    aes(x=measurement_column, text=text))+
          geom_bar(aes(y=prop_viol, fill=site),stat="identity")+
          geom_linerange(aes(ymin=q1, ymax=q3))+
          geom_point(aes(y=median_val), shape=23, size=1)+
         # facet_wrap(~table_application, scales="free")+
          labs(x = "Column",
               y="Proportion Violating Records")+
          theme_bw()+
          scale_fill_manual(values=site_colors)+
          coord_flip()+
          theme(legend.position = "none")
    }else if(input$largen_toggle==2&input$comp_vc_ln==0){
      plt<-ggplot(filter(vc_output(),site==input$sitename_vc_conf)%>%
                    mutate(text=paste0("proportion with violation: ",round(prop_viol,3))))+
        geom_bar(aes(x=measurement_column,y=prop_viol,fill=site, text=text), stat="identity")+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        coord_flip()+
        labs(x = "Column",
             y="Proportion Violating Records")+
        theme(legend.position="none")
    }else{
      plt<-ggplot(filter(vc_output(), site==input$sitename_vc_conf)%>%
                    mutate(text=paste0("site: ",site,
                                       "\nvocabulary: ",vocabulary_id,
                                       "\nproportion: ",prop_viol)),
                  aes(x=measurement_column,y=tot_prop, fill=vocabulary_id, text=text))+
        geom_bar(stat="identity",position="stack")+
        theme_bw()+
        scale_fill_pedsn_dq()+
        theme(axis.text.x = element_text(angle=90))+
        labs(x="Column",
             y="Proportion of Records")
    }
    ggplotly(plt, tooltip="text")
  })


  output$vc_plot <- renderPlotly({
    if(input$sitename_vc_conf=='total'){
      outplot<-ggplot(filter(vc_output(),!accepted_value)%>%
                        mutate(text=paste0("vocabulary: ",vocabulary_id,
                                           "\nproportion: ",prop_viol)),
                      aes(x=site,y=tot_prop,fill=vocabulary_id, text=text))+
        geom_bar(stat="identity", position="stack")+
        ylim(0,1)+
        theme_bw()+
        scale_fill_pedsn_dq()+
        theme(axis.text.x = element_text(angle=90))+
        facet_wrap(~measurement_column)
    }else if(nrow(filter(vc_output(), site==input$sitename_vc_conf&!accepted_value))>0){
      if(input$largen_toggle==1){
      outplot<-ggplot(filter(vc_output(), site==input$sitename_vc_conf&!accepted_value)%>%
                        mutate(text=paste0("vocabulary: ",vocabulary_id,
                                           "\nproportion: ",prop_viol)), aes(x=measurement_column, y = tot_prop, fill = vocabulary_id, text=text)) +
        geom_bar(stat="identity", position="dodge") +
        ylim(0, 1)+
        facet_wrap(~table_application, scales="free")+
        scale_fill_pedsn_dq()+
        theme_bw()+
        labs(x="Column Name",
             y="Proportion of Total Records",
             title="Violating Records per Column")
      }else{
        outplot<-ggplot(filter(vc_vocablevel(), site==input$sitename_vc_conf&!accepted_value)%>%
                          mutate(text=paste0("vocabulary: ",vocabulary_id,
                                             "\nproportion: ",prop_viol)), aes(x=measurement_column, y = tot_prop, fill = vocabulary_id, text=text)) +
          geom_bar(stat="identity", position="dodge") +
          ylim(0, 1)+
          facet_wrap(~table_application, scales="free")+
          scale_fill_pedsn_dq()+
          theme_bw()+
          labs(x="Column Name",
               y="Proportion of Total Records",
               title="Violating Records per Column")
      }
    }
    else{
      outplot <- ggplot()+
        geom_blank()+
        annotate("text", label="No violations", x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
    }
    return(ggplotly(outplot, tooltip="text"))
  })
  output$vc_table <- DT::renderDT({
    if(input$sitename_vc_conf=='total'){
      outtable <-filter(vc_output(), !accepted_value)%>%
        mutate(total_violations=format(tot_ct, big.mark=','),
               total_rows=format(total_denom_ct, big.mark=','))%>%
        select(site, table_application, vocabulary_id, total_violations, total_rows)
    }else if(nrow(filter(vc_output(), site==input$sitename_vc_conf&!accepted_value))!=0){
      outtable <-filter(vc_vocablevel(), site==input$sitename_vc_conf&!accepted_value)%>%
        mutate(total_violations=format(tot_ct, big.mark=','),
               total_rows=format(total_denom_ct, big.mark=','))%>%
        select(table_application, vocabulary_id, total_violations, total_rows)
    }
    else{
      outtable <- tibble(None="")
    }
    return(outtable)
  })

  # vocabulary conformance table of vocabs
  output$vc_vocabs<-DT::renderDT({
    vc_vocabs_accept
  })

  ## UNMAPPED CONCEPTS --------
  output$uc_overall_plot <- renderPlot({
    if(input$sitename_uc=="total"){
        outplot <- ggplot(uc_output(), aes(x = site, y = unmapped_prop, fill=site)) +
          geom_bar(stat='identity')+
          facet_wrap(~measure, scales="free_x")+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
                axis.title=element_text(size=18),
                legend.position="none")+
          scale_fill_manual(values=site_colors)+
          labs(x="Site",
               y="Proportion Unmapped Concepts")
    }else if(input$largen_toggle==2&input$comp_uc_ln==1){
      outplot <- ggplot(filter(uc_output(),
                                 site==input$sitename_uc),
                          aes(x = measure, y = unmapped_prop, fill=site)) +
          geom_bar(stat='identity')+
          scale_fill_manual(values=site_colors)+
          geom_errorbar(aes(ymin=q1, ymax=q3))+
          geom_point(aes(y=median_val), shape=23, size=1)+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
                axis.title=element_text(size=18),
                legend.position="none")+
          labs(x="Site",
               y="Proportion Unmapped Concepts")
      }else{
      outplot <- ggplot(filter(uc_output(),site==input$sitename_uc), aes(x = measure, y = unmapped_prop, fill=site, label=unmapped_prop)) +
        geom_bar(stat='identity')+
        geom_label(fill="white")+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
              axis.title=element_text(size=18),
              legend.position="none")+
        labs(x="Table Application",
             y="Proportion Unmapped Concepts")
    }
    return(outplot)

  })

  output$uc_yr_plot <- renderPlot({
    if(input$sitename_uc=="total"){
      outplot <- ggplot(filter(uc_yr_output(), year_date>=input$date_uc_range[1],year_date<=input$date_uc_range[2]),
                        aes(x = year_date, y = prop_total, colour=site)) +
        geom_point()+
        geom_line()+
        facet_wrap(~unmapped_description, scales="free") +
        labs(x = "Year",
             y = "Proportion Unmapped")+
        theme_bw()+
        theme(axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title=element_text(size=18))+
        scale_color_manual(values=site_colors)+
        scale_x_continuous(breaks = pretty_breaks())+
        theme(legend.position = "none")
    }else if(input$largen_toggle==2&input$comp_uc_ln==1){
      outplot <- ggplot(filter(uc_yr_output(), site==input$sitename_uc,
                                  year_date>=input$date_uc_range[1],year_date<=input$date_uc_range[2]),
                                    aes(x = year_date)) +
          geom_ribbon(aes(ymin=q1,ymax=q3),fill="grey70")+
         geom_line(aes(y=median_val), linetype="dotted")+
          geom_line(aes(y=prop_total, colour=site),linewidth=2)+
          facet_wrap(~unmapped_description, scales="free") +
          labs(x = "Year",
               y = "Proportion Unmapped")+
          theme_bw()+
          theme(axis.text.x = element_text(size=12),
                axis.text.y = element_text(size=12),
                axis.title=element_text(size=18))+
          scale_color_manual(values=site_colors)+
          scale_x_continuous(breaks = pretty_breaks())+
         theme(legend.position = "none")
      }else{
      outplot <- ggplot(filter(uc_yr_output(),site==input$sitename_uc, year_date>=input$date_uc_range[1],year_date<=input$date_uc_range[2]), aes(x = year_date, y = prop_total, color=site)) +
        geom_point()+
        geom_line()+
        facet_wrap(~unmapped_description, scales="free") +
        labs(x = "Year",
             y = "Proportion Unmapped")+
        theme_bw()+
        scale_color_manual(values=site_colors)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
              axis.text.y = element_text(size=12),
              axis.title=element_text(size=18))+
        scale_x_continuous(breaks=pretty_breaks())
    }
    return(outplot)
  })

  output$uc_top_tbl <- DT::renderDT({
    if(input$sitename_uc=="total"){
      outtable <- uc_top_output_overall()%>%
        select(unmapped_description, src_value_name, src_value, src_value_ct, proportion_of_unmapped)
    }else{
      outtable <- filter(uc_top_output(), site==input$sitename_uc) %>%
        select(unmapped_description, src_value_name, src_value, src_value_ct, proportion_of_unmapped)
    }
    return(outtable)
  })

  ## PERSON FACTS/RECORDS ------
  ### barplot

  output$pf_mappings <- DT::renderDT(
    DT::datatable(
      pf_mappings,
      options=list(pageLength=5),
      rownames=FALSE
    )
  )
  output$pf_overall_bysite_plot <- renderPlot({
    if(input$sitename_pf=="total"){
      outplot <- ggplot()+
        geom_blank()+
        annotate("text", label="Select a site", x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
    }else{
      plotdat<-filter(pf_output(),site==input$sitename_pf)
      outplot <- ggplot(plotdat,
                        aes(x=check_desc_neat, y = fact_visits_prop, label=fact_visits_prop, fill=site)) +
        geom_bar(stat='identity')+
        geom_label(fill="white")+
        facet_wrap(~visit_type)+
        ylim(c(0,1))+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        labs(x="Check Description",
             y="Proportion Visits with Fact")+
        theme(axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title=element_text(size=18),
              legend.position="none")+
        coord_flip()
    }
    return(outplot)
  })
  ### heatmap (individual sites) or bar plot (large n)
  output$pf_overall_heat_plot <- renderPlotly({
    if(input$sitename_pf=="total"){
        outplot <- ggplot(pf_output()%>%
                            mutate(text=paste0("site: ",site,
                                               "\ncheck: ",check_desc_neat,
                                               "\nprop. visits: ",fact_visits_prop,
                                               "\nprop. patients: ",fact_pts_prop)),
                          aes(x=site, y=check_desc_neat, fill=fact_pts_prop, text=text))+
          geom_tile() +
          theme_bw()+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(x="",y="",
               fill="Proportion Patients\nwith Fact")+
          facet_wrap(~visit_type, scales = "free_x")+
          scale_fill_pedsn_dq(palette="sequential", discrete=FALSE)
        # large n, comparison
    }else if(input$largen_toggle==2&input$comp_pf_ln==1){
        outplot<-ggplot(pf_output()%>%filter(site==input$sitename_pf)%>%
                          mutate(text=paste0("\nprop. patients: ",fact_pts_prop,
                                             "\nprop. visits: ",fact_visits_prop,
                                             "\nprop. patients median (Q1, Q3): ", round(median_val_pts,2), " (",round(q1_pts,2), ", ", round(q3_pts,2), ")",
                                             "\nprop. visits median (Q1, Q3): ", round(median_val_visits,2), " (",round(q1_visits,2), ", ", round(q3_visits,2), ")")),
                        aes(x=check_desc_neat, fill=site, text=text))+
          geom_bar(aes(y=fact_pts_prop), stat="identity")+
          geom_linerange(aes(ymin=q1_pts, ymax=q3_pts))+
          geom_point(aes(x=check_desc_neat,y=median_val_pts), shape=23, size=1)+
          scale_fill_manual(values=site_colors)+
          facet_wrap(~visit_type)+
          theme_bw()+
          coord_flip()+
          labs(x="Fact Type",y="Proportion Patients with Fact")+
          theme(legend.position="none")
      }else{
      outplot <- ggplot(filter(pf_output(),site==input$sitename_pf)%>%
                          mutate(text=paste0("site: ",site,
                                             "\ncheck: ",check_desc_neat,
                                             "\nprop. visits: ",fact_visits_prop,
                                             "\nprop. patients: ",fact_pts_prop)),
                        aes(x=site, y=check_desc_neat, fill=fact_pts_prop, text=text))+
        geom_tile() +
        theme_bw()+
        labs(y="",
             fill="Proportion Patients\nwith Fact")+
        facet_wrap(~visit_type, scales = "free_x")+
        scale_fill_pedsn_dq(palette="sequential", discrete=FALSE)
    }
    return(ggplotly(outplot, tooltip="text"))
  })

  ## BEST MAPPED CONCEPTS --------
  output$bmc_overall_plot <- renderPlotly({
    if(input$sitename_bmc=="total"){
      # individual sites, overall
        outplot <-  ggplot(filter(bmc_pp(),include_new==1L&site!='total'),
                           aes(x=site, y=best_row_prop, fill=site,
                               text=round(best_row_prop,2)))+
          geom_bar(stat='identity')+
          scale_fill_manual(values=site_colors)+
          facet_wrap(~check_desc)+
          labs(x="Site",
               y="Proportion Best Mapped")+
          coord_flip()+
          theme_bw()+
          theme(legend.position="none")
    }else if(input$largen_toggle==2&input$comp_bmc_ln==1){
      # large n, overall
        outplot <- ggplot(filter(bmc_pp(),include_new==1L&site==input$sitename_bmc)%>%
                            mutate(text=paste("Proportion best mapped: ",round(best_row_prop,2),
                                              "\nOverall median (Q1, Q3): ",round(median_val,2), " (",round(q1,2),", ",round(q3,2), ")")),
                          aes(x=check_desc,fill=site,text=text))+
          geom_bar(aes(y=best_row_prop), stat="identity")+
          geom_linerange(aes(ymin=q1, ymax=q3))+
          geom_point(aes(y=median_val), shape=23, size=1)+
          scale_fill_manual(values=site_colors)+
          theme_bw()+
          theme(legend.position = "none")+
          labs(x="Check Application",
               y="Proportion Best Mapped")+
          coord_flip()
      }else{
      outplot <- ggplot(filter(bmc_pp(), site==input$sitename_bmc)%>%
                          mutate(include_new=case_when(include_new==0~"No",
                                                       include_new==1~"Yes")),
                        aes(x=check_desc, y=best_row_prop, fill=include_new, text=paste0("Proportion ",include_new, ": ", round(best_row_prop,2),
                                                                                         "\n",
                                                                                         "Top non-best: ", top5)))+
        geom_bar(stat='identity', position='stack')+
        scale_fill_pedsn_dq("boldtwo")+
        labs(x="Check Type",
             y="Proportion Best Mapped",
             fill="Best Mapped")+
        coord_flip()+
        theme_bw()
    }
    return(ggplotly(outplot,tooltip="text"))
  })
  ## FACTS OVER TIME -----
  ### overall plot
  output$fot_summary_plot <- renderPlotly({
    if(input$largen_toggle==1){
      indat<-fot_output_summary_ratio() %>%
        filter(check_desc==input$fot_subdomain_overall&!site%in%c('allsite_median',
                                                                  'allsite_mean'))
      allsite_avg<-fot_output_summary_ratio()%>%
        filter(check_desc==input$fot_subdomain_overall&site=='allsite_median')
    }else{
      indat<-fot_output_summary_ratio() %>%
        filter(check_desc==input$fot_subdomain_overall&site==input$sitename_fot)
      allsite_avg<-fot_output_summary_ratio()%>%
        filter(check_desc==input$fot_subdomain_overall&site%in%c('allsite_median','allsite_mean'))
    }
    showplot <- ggplot(indat,
                       aes(x=month_end,
                           y=row_ratio,
                           color=site,
                           group=site)
    )+
      geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5,formula=y~x)+
      geom_smooth(data=allsite_avg, aes(x=month_end,
                                        y=row_ratio,
                                        color=site,
                                        group=site),linewidth=1)+
      scale_color_manual(values=site_colors) +
      theme_bw()+
      labs(x="Time (month)",
           y="Fact Rate (records per 10,000 visits)",
           title="Fact Rate over Time")+
      scale_x_date(limits = c(input$date_fot_min, input$date_fot_max))
    return(ggplotly(showplot))
  })
  ### site-specific plots
  # Insert the right number of plot output objects into the web page
  # right now, not working when no checkboxes are selected...
  output$fot_plot <- renderUI({
    if(length(input$fot_subdomain_site)==0){
      maxi<-1L
    }else{
      maxi<-length(input$fot_subdomain_site)
    }
    plot_output_list <- lapply(1:maxi, function(i) {
      plotname <- paste("plot", i, sep="")
      plotlyOutput(plotname)
    })

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.

  fot_listen<-reactive({list(fot_output_site(), input$fot_bounds)})

  observeEvent(fot_listen(),{
    if(length(input$fot_subdomain_site)==0){
      plotname <- "plot1"
      showplot<-plotly_empty(type="scatter",mode="markers")%>%
        layout(
          title=list(
            text="Please make a selection for Site Specific Plots: Specific Check",
            yref="paper",
            y=0.5
          )
        )
      output[[plotname]] <- renderPlotly({showplot})
    }else{
      df_plots <- fot_output_site()%>%
        dplyr::nest_by(check_desc) %>%
        dplyr::mutate(plot = list(plot_fot_fn(data)))

      if(input$fot_bounds=='No Bounds'){
        for (i in 1:length(input$fot_subdomain_site)) {
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")

            plot_title<-as.character(input$fot_subdomain_site[i])
            output[[plotname]] <- renderPlotly({
              df_plots$plot[[my_i]]%>%
                layout(title=plot_title)
            })
          })
        }
      }else{
        for (i in 1:length(input$fot_subdomain_site)) {
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")

            plot_title<-as.character(input$fot_subdomain_site[i])
            output[[plotname]] <- renderPlotly({
              df_plots$plot[[my_i]]%>%
                layout(title=plot_title) %>%
                add_lines(x=~month_end,y=~m+std_dev*as.numeric(input$fot_bounds), yaxis='y2', name='Upper SD bound')%>%
                add_lines(x=~month_end,y=~m-std_dev*as.numeric(input$fot_bounds), yaxis='y2', name='Lower SD bound')
            })
          })
        }
      }
    }
  })


  ## DOMAIN CONCORDANCE -------
  output$dcon_overall_plot <- renderPlotly({
    if(length(input$dcon_check)==0){
      showplot <- ggplot() +
        geom_blank() +
        annotate("text",label='Select a specific check', x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
    }
    else if(input$sitename_dcon=='total'){
      if(input$denom_dcon=='Overall'){
          showplot <- ggplot(filter(dcon_output(),
                                    site!='total',
                                    check_name%in%input$dcon_check,
                                    cohort%in%c("cohort 2 only", "combined", "cohort 1 only"))) +
            geom_bar(aes(x=site,y=prop,fill=cohort,
                         text=paste0("Cohort: ", cohort,
                                     "\nProportion: ",round(prop,2))),
                     stat='identity') +
            scale_fill_pedsn_dq("triset")+
            theme_bw()+
            theme(axis.text.x=element_text(size=12),
                  axis.text.y=element_text(size=12),
                  axis.title=element_text(size=16))+
            labs(x="Site",
                 y="Proportion")+
            facet_wrap(~check_name)+
            coord_flip()
        }else if(input$denom_dcon=='Cohort 1'){
          showplot <- ggplot(filter(dcon_output(),
                                    site!='total',
                                    check_name%in%input$dcon_check,
                                    cohort%in%c("cohort 1 not 2", "cohort 2 in 1"))) +
            geom_bar(aes(x=site,y=prop,fill=cohort,
                         text=paste0("Cohort: ", cohort,
                                     "\nProportion: ",round(prop,2))),
                     stat='identity') +
            scale_fill_pedsn_dq("triset")+
            theme_bw()+
            theme(axis.text.x=element_text(size=12),
                  axis.text.y=element_text(size=12),
                  axis.title=element_text(size=16))+
            labs(x="Site",
                 y="Proportion")+
            facet_wrap(~check_name)+
            coord_flip()
        }else if(input$denom_dcon=='Cohort 2'){
          showplot <- ggplot(filter(dcon_output(),
                                    site!='total',
                                    check_name%in%input$dcon_check,
                                    cohort%in%c("cohort 2 not 1", "cohort 1 in 2"))) +
            geom_bar(aes(x=site,y=prop,fill=cohort,
                         text=paste0("Cohort: ", cohort,
                                     "\nProportion: ",round(prop,2))),
                     stat='identity') +
            scale_fill_pedsn_dq("triset")+
            theme_bw()+
            theme(axis.text.x=element_text(size=12),
                  axis.text.y=element_text(size=12),
                  axis.title=element_text(size=16))+
            labs(x="Site",
                 y="Proportion")+
            facet_wrap(~check_name)+
            coord_flip()
        }
        # large n
    }else if(input$largen_toggle==2&input$comp_dcon_ln==1){
      showplot <- ggplot(filter(dcon_output(),
                                site==input$sitename_dcon,
                                check_name%in%input$dcon_check,
                                cohort%in%c("cohort 1 only", "cohort 2 only", "combined")),
                         aes(x=cohort)) +
        geom_bar(aes(y=prop,fill=cohort,
                     text=paste0("Cohort: ", cohort,
                                 "\nProportion: ",round(prop,2),
                                 "\nMedian (Q1, Q3): ",round(median_val,2), " ( ", round(q1,2), ", ", round(q3,2), ")")),
                 stat='identity') +
        geom_linerange(aes(ymin=q1, ymax=q3))+
        geom_point(aes(y=median_val), shape=23, size=1)+
        scale_fill_pedsn_dq("triset")+
        theme_bw()+
        theme(axis.text.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16))+
        labs(x="",
             y="Proportion")+
        facet_wrap(~check_name)+
        coord_flip()
    }else{
      if(input$denom_dcon=="Overall"){
        showplot <- ggplot(filter(dcon_output(),
                                  site==input$sitename_dcon&
                                    check_name%in%input$dcon_check&
                                    cohort%in%c("cohort 2 only", "combined", "cohort 1 only"))) +
          geom_bar(aes(x=site,y=prop,fill=cohort,
                       text=paste0("Cohort: ", cohort,
                                   "\nProportion: ",round(prop,2))),
                   stat='identity') +
          scale_fill_pedsn_dq("triset")+
          theme_bw()+
          theme(axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title=element_text(size=16))+
          labs(x="Site",
               y="Proportion")+
          facet_wrap(~check_name)+
          coord_flip()
      }else if(input$denom_dcon=="Cohort 1"){
        showplot <- ggplot(filter(dcon_output(),
                                  site==input$sitename_dcon&
                                    check_name%in%input$dcon_check&
                                    cohort%in%c("cohort 1 not 2", "cohort 2 in 1"))) +
          geom_bar(aes(x=site,y=prop,fill=cohort,
                       text=paste0("Cohort: ", cohort,
                                   "\nProportion: ",round(prop,2))),
                   stat='identity') +
          scale_fill_pedsn_dq("triset")+
          theme_bw()+
          theme(axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title=element_text(size=16))+
          labs(x="Site",
               y="Proportion")+
          facet_wrap(~check_name)+
          coord_flip()
      }else if(input$denom_dcon=="Cohort 2"){
        showplot <- ggplot(filter(dcon_output(),
                                  site==input$sitename_dcon&
                                    check_name%in%input$dcon_check&
                                    cohort%in%c("cohort 2 not 1", "cohort 1 in 2"))) +
          geom_bar(aes(x=site,y=prop,fill=cohort,
                       text=paste0("Cohort: ", cohort,
                                   "\nProportion: ",round(prop,2))),
                   stat='identity') +
          scale_fill_pedsn_dq("triset")+
          theme_bw()+
          theme(axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title=element_text(size=16))+
          labs(x="Site",
               y="Proportion")+
          facet_wrap(~check_name)+
          coord_flip()
      }
    }
    return(ggplotly(showplot, tooltip="text"))
  })

  ## FACTS WITH MISSING VISIT IDS -----
  # overall
  output$mf_visitid_overall <- renderPlot({
    if(input$largen_toggle==1){
      ggplot(mf_visitid_output(), aes(x=site, y=domain, fill=prop_missing_visits_total))+
        geom_tile(color='white',lwd=0.5,linetype=1) +
        scale_fill_gradient(limits=c(0.000001,1))+
        guides(fill = guide_colourbar(barwidth=0.5,
                                      barheight = 15,
                                      title = 'Proportion Missing')) +
        theme_bw()+
        theme(axis.text.y= element_text(hjust=1, size=12),
              axis.text.x = element_text(hjust=1,vjust=0.5,angle = 90,size=12),
              axis.title=element_text(size=16))+
        facet_wrap(~domain, scales="free_y")+
        scale_fill_pedsn_dq(palette="sequential", discrete=FALSE)
    }else{
      ggplot(mf_visitid_output()%>%filter(site==input$sitename_mf_visitid),
             aes(x=domain))+
        geom_bar(aes(y=prop_missing_visits_total, fill=site),stat="identity")+
        scale_fill_manual(values=site_colors)+
        geom_linerange(aes(ymin=q1, ymax=q3))+
        geom_point(aes(y=median_val), shape=23, size=1)+
        theme_bw()+
        labs(x="Domain",
             y="Proportion missing visit_occurrence_id")
    }


  })

  # by site
  output$mf_visitid_bysite <- renderPlot({
    if(length(input$mf_visitid_domain)==0){
      showplot <- ggplot()+
        geom_blank()+
        annotate("text", label="Select a domain", x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
    }else{
      showplot<-ggplot(filter(mf_visitid_output(),domain%in%input$mf_visitid_domain&
                                site==input$sitename_mf_visitid),
                       aes(x=measure,y=prop_missing_visits_total, fill=site))+
        geom_bar(stat='identity')+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1,size=12),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16),
              legend.position = "none")+
        facet_wrap(~domain, scales="free_y", ncol=2)+
        labs(x="domain",
             y="Proportion Missing visit_occurrence_id")+
        coord_flip()
    }
    return(showplot)
  })

  ## EXPECTED CONCEPTS PRESENT -----
  output$ecp_plot <- renderPlotly({
    if(input$largen_toggle==1){
      plt<-ggplot(ecp_output()%>%
                    mutate(text=paste0("site: ",site,
                                       "\nproportion: ",round(prop_with_concept,2))),
                  aes(x=site, y=prop_with_concept, fill=site, text=text))+
        geom_bar(stat="identity")+
        scale_fill_manual(values=site_colors)+
        theme_bw()+
        labs(y="Proportion",
             title="Proportion of Patients with Expected Concept")+
        theme(legend.position = "none")+
        facet_wrap(~concept_group)+
        coord_flip()
    }else{
      plt<-ggplot(ecp_output()%>%
                    filter(site==input$sitename_ecp)%>%
                    mutate(text=paste0("Concept group: ",concept_group,
                                       "\nProportion with concept: ", round(prop_with_concept,2),
                                       "\nMedian (Q1, Q3): ",round(median_val,2), " (", round(q1,2), ", ", round(q3,2), ")")),
                  aes(x=concept_group, text=text, fill=site))+
        geom_bar(aes(y=prop_with_concept), stat="identity")+
        scale_fill_manual(values=site_colors)+
        geom_linerange(aes(ymin=q1, ymax=q3))+
        geom_point(aes(y=median_val), shape=23, size=1)+
        labs(x="Concept Group",
             y="Proportion with Concept",
             title="Proportion of Patients with Expected Concepts")+
        theme_bw()+
        coord_flip()+
        theme(legend.position="none")
    }
    ggplotly(plt, tooltip="text")
  })
  output$ecp_plot_site <- renderPlot({
    ggplot(filter(ecp_output(), site==input$sitename_ecp),
           aes(x=concept_group, y=round(prop_with_concept, 2), fill=site))+
      geom_bar(stat="identity")+
      geom_label(aes(x=concept_group, y=round(prop_with_concept, 2), label=round(prop_with_concept, 2)),
                 fill="white",
                 show.legend = FALSE)+
      scale_fill_manual(values=site_colors)+
      theme_bw()+
      labs(x="Concept Group",
           y="Proportion",
           title="Proportion of Patients with Expected Concept")+
      theme(legend.position = "none",
            axis.text.y = element_text(size=14),
            axis.text.x=element_text(size=14),
            axis.title.x=element_text(size=14),
            axis.title.y=element_text(size=14))+
      coord_flip()
  })

  # SSDQA ISSUES -------
  # adjust available domain

  #   ssdqa_issues <- reactive({results_tbl('ssdqa_issues_ops_226', results_tag=FALSE)%>%collect()})
  #
  #   observeEvent(ssdqa_issues(), {
  #     choices_new <- ssdqa_issues()%>%
  #       mutate(domains_wd=str_remove(domains," \\(labs, anthro, vitals\\)|\\(care site, provider\\)|(history, fips)"))%>%
  #       separate_rows(domains_wd, sep=", ")%>%distinct(domains_wd)%>%pull()%>%sort()
  #
  #     choices_new<-c("All",choices_new)
  #     updateSelectInput(inputId="ssdqa_domain", choices=choices_new)
  #   })
  #
  #   output$ssdqa_issues_table<-DT::renderDT({
  #     if(config('mask_site')){return_tbl<-data.frame()}
  #     else if(input$ssdqa_domain=='All'){
  #       return_tbl<-ssdqa_issues()
  #     }else if("adt_occurrence"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"adt_occurrence"))
  #     }else if("condition_occurrence"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"condition_occurrence"))
  #     }else if("device_exposure"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"device_exposure"))
  #     }else if("drug_exposure"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"drug_exposure"))
  #     }else if("fact_relationship"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"fact_relationship"))
  #     }else if("hash_token"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"hash_token"))
  #     }else if("immunization"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"immunization"))
  #     }else if("location"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"location"))
  #     }else if("measurement"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"measurement"))
  #     }else if("observation"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"observation"))
  #     }else if("person"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"person"))
  #     }else if("procedure_occurrence"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"procedure_occurrence"))
  #     }else if("specialty"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"specialty"))
  #     }else if("visit_occurrence"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"visit_occurrence"))
  #     }else if("visit_payer"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"visit_payer"))
  #     }else if("other"%in%input$ssdqa_domain){
  #       return_tbl<-ssdqa_issues()%>%filter(str_detect(domains,"other"))
  #     }
  #     if(!exists("return_tbl")){return_tbl<-NULL}
  #     DT::datatable(
  #       return_tbl,
  #       filter="top",
  #       options=list(pageLength=5),
  #       rownames=FALSE
  #     )
  # })

})



