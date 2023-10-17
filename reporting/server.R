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



shinyServer(function(input, output) {
  # changes between data cycles -------
  # capture data
  dc_output_all <- reactive({
    results_tbl('dc_output_pp')%>%filter(domain!='measurement_anthro')%>%collect() %>%
      mutate(site=case_when(site=='total'~'total',
                            TRUE ~ site)) %>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
  })

  # adjust available site name
  observeEvent(dc_output_all(), {
    choices_new<-c("total",unique(dc_output_all()$site)%>%sort())
    updateSelectInput(inputId="sitename_dc", choices=choices_new)
  })

  # update choices for domain
  observeEvent(dc_output_all(), {
    choices_pp<-gsub("_.*","",dc_output_all()$domain)
    choices_new<-unique(choices_pp)%>%
      sort()
    updateSelectInput(inputId="dc_domain", choices=choices_new)
  })

  # filter data for plotting
  dc_output <- reactive({
    results_tbl('dc_output_pp')%>%filter(domain!='measurement_anthro')%>%collect()%>%
      filter(str_detect(domain,input$dc_domain),
             application=='rows') %>% # might be temporary fix
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
  })

  # update choices for subdomain
  observeEvent(dc_output(), {
    choices_new<-unique(dc_output()$domain)
    updateCheckboxGroupInput(inputId="dc_subdomain", choices=choices_new)
  })


  # vocabulary and valueset conformance ------------
  # capture data
  vc_vs_output <- reactive({
    results_tbl('vc_vs_violations') %>%
      collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))%>%
      group_by(site, check_type, table_application, measurement_column, vocabulary_id)%>%
      summarise(tot_viol=sum(total_viol_ct),
                tot_rows=min(total_denom_ct))%>% # denom is the same for all in group
      ungroup() %>%
      mutate(prop_viol=round(tot_viol/tot_rows,2))
  })
  # adjust available site name
  observeEvent(vc_vs_output(), {
    choices_new<-unique(vc_vs_output()$site)%>%sort()
    updateSelectInput(inputId = "sitename_conf", choices=choices_new)
  })

  # unmapped concepts -----
  # capture data
  uc_output <- reactive({
    results_tbl('uc_output') %>%collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                                                TRUE~site))
  })
  uc_yr_output <- reactive({
    results_tbl('uc_by_year_pp') %>%collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))%>%
      mutate(year_date=as.integer(year_date))
  })
  uc_top_output <- reactive({
    results_tbl('uc_grpd') %>% collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))%>%
      group_by(site, unmapped_description) %>%
      slice_max(order_by = src_value_ct, n=10) %>%
      ungroup()%>%
      mutate(src_value_ct=as.integer(src_value_ct))
  })
  uc_top_output_overall <- reactive({
    results_tbl('uc_grpd')%>%collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))%>%
      group_by(unmapped_description)%>%
      slice_max(order_by=src_value_ct, n=10)%>%
      ungroup()%>%
      mutate(src_value_ct=as.integer(src_value_ct))
  })
  # adjust available site name
  observeEvent(uc_output(), {
    choices_new<-c("total", unique(uc_output()$site)%>%sort())
    updateSelectInput(inputId = "sitename_uc", choices=choices_new)
  })

  # person facts/records -------
  # capture data
  pf_output <- reactive({
    results_tbl('pf_output_pp') %>% collect() %>%
      mutate(visit_type = case_when(str_detect(check_description, "^ip")~ 'inpatient',
                                    str_detect(check_description, "^all")~'all',
                                    str_detect(check_description, "^op")~'outpatient',
                                    str_detect(check_description, "^ed")~'emergency'),
             check_description=str_remove(check_description, "^ip_|^all_|^op_|^ed_")) %>%
      mutate(check_description= case_when(check_description=='all_visits_with_procs_drugs_labs' ~ 'visits_with_procs_drugs_labs',
                                          TRUE ~ check_description))%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
  })
  # adjust available site name
  observeEvent(pf_output(), {
    choices_new<-c("total", unique(pf_output()$site)%>%sort())
    updateSelectInput(inputId = "sitename_pf", choices=choices_new)
  })

  # best mapped concepts ----
  # fetch data
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
    results_tbl('bmc_gen_output_pp')%>%
      collect()%>%
      left_join(top_rolled, by = c('site', 'check_desc'))%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
  })
  bmc_pp_concepts <- reactive({
    results_tbl('bmc_gen_output_concepts_pp')%>%
      collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
  })

  # adjust available site name
  observeEvent(bmc_pp(), {
    choices_new<-c("total", unique(bmc_pp()$site)%>%sort())
    updateSelectInput(inputId = "sitename_bmc", choices=choices_new)
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
  output$bmc_conceptset_best<-DT::renderDataTable({
    bmc_conceptset%>%
      filter(include==1)%>%
      select(check_desc, concept)
  })
  output$bmc_conceptset_notbest<-DT::renderDataTable({
    bmc_conceptset%>%
      filter(include==0)%>%
      select(check_desc, concept)
  })

  output$bmc_pp_top_nonbest <- DT::renderDataTable({
    bmc_pp_top()%>%
      mutate(row_proportions=round(row_proportions,2))
  })


  # facts over time ------
  # capture data
  fot_output_heuristic <- reactive({results_tbl('fot_heuristic_summary') %>%
      filter(domain!='labs')%>% # remove in future versions
      collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
  })

  # update choices for domain
  observeEvent(fot_output_heuristic(), {
    choices_new_fot<-unique(fot_output_heuristic()$domain)%>%sort()
    updateSelectInput(inputId="fot_domain", choices=choices_new_fot)
  })

  # limit table to domain selected for specific check choices
  fot_output <- reactive({results_tbl('fot_heuristic') %>%
      inner_join(results_tbl('fot_heuristic_summary'),
                 by=c('domain','check_name', 'site', 'site_anon')) %>% collect() %>%
      filter(domain==input$fot_domain)%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
    })

  fot_output_summary <- reactive({results_tbl('fot_output_distance') %>%
      collect() %>%
      filter(domain==input$fot_domain) %>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
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

  # domain concordance -----
  # capture data
  dcon_output <- reactive({
    results_tbl(name='dcon_output_pp')%>%collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
  })
  # dcon_output_byyr <- reactive({
  #   results_tbl(name='dcon_output_pp_byyr')%>%collect()%>%
  #     mutate(site=case_when(config('mask_site')~site_anon,
  #                           TRUE~site))
  #})
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
    choices_new<-c("total", unique(dcon_output()$site)%>%sort())
    updateSelectInput(inputId = "sitename_dcon", choices=choices_new)
  })
  # descriptions of cohorts
  output$dcon_cohort_descr <- renderDataTable(
    DT::datatable(
      dcon_meta(),
      options=list(pageLength=5),
      rownames=FALSE
    )
  )
  # facts with missing visit ids -------------
  # connect with data
  mf_visitid_output <- reactive({
    results_tbl(name='mf_visitid_pp')%>%collect()%>%
      mutate(site=case_when(config('mask_site')~site_anon,
                            TRUE~site))
  })

  # update choices for site name
  observeEvent(mf_visitid_output(), {
    choices_new<-c("total",unique(filter(mf_visitid_output(),site!='total')$site)%>%sort())
    updateSelectInput(inputId="sitename_mf_visitid", choices=choices_new)
  })

  # update choices for domain
  observeEvent(mf_visitid_output(), {
    choices_new<-unique(mf_visitid_output()$domain)%>%sort()
    updateCheckboxGroupInput(inputId="mf_visitid_domain", choices=choices_new)
  })


  # set site colors based on table where all sites expected
  site_list<-(results_tbl("dc_output") %>% select(site, site_anon)%>%distinct()%>%collect()%>%
                mutate(site=case_when(config('mask_site')~site_anon,
                                      TRUE~site)))$site
  ramp_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(length(site_list))
  randomized_palette <- ramp_palette[sample(1:length(ramp_palette))]
  site_colors <- setNames(randomized_palette, site_list)


  dc_mappings <- results_tbl('dc_mappings')%>%collect()

  pf_mappings <- results_tbl('pf_mappings') %>%collect()%>%
    mutate(`Visit Type`=case_when(str_detect(Label, "all")~"all",
                           str_detect(Label, "op")~"outpatient",
                           str_detect(Label, "ip")~"inpatient",
                           str_detect(Label, "ed")~"emergency"))%>%
    select(`Visit Type`, Description)

  df_check_descriptions <- read_codeset('check_type_descriptions','cc')



  # DESCRIPTIONS
  #http://cran.nexr.com/web/packages/DT/DT.pdf
  output$dt_descriptions <- DT::renderDataTable(
    DT::datatable(
      df_check_descriptions,
      options=list(pageLength=5),
      rownames=FALSE
    )
  )

  # PLOTS
  # CHANGES BETWEEN DATA CYCLES ---------------------------------------------------------------------------------------------------------
  output$dc_mappings <- DT::renderDataTable(
    DT::datatable(
      dc_mappings,
      options=list(pageLength=5),
      rownames=FALSE
    )
  )

  # changes between data cycles - record counts
  output$dc_domain_split <- renderPlot({
    if(input$sitename_dc=="total"){
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
      }else{
        showplot<-ggplot(filter(dc_output(),domain%in%input$dc_subdomain), aes(x=site,y=prop_total_change, fill=site))+
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
      }
    }
    else{
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
      }
      else{
        tc_prev<-paste0('total_ct_',config('db_previous'))
        tc_new<-paste0('total_ct_',config('db_current'))
        pc_prev<-paste0('total_pt_ct_', config('db_previous'))
        pc_new<-paste0('total_pt_ct_',config('db_current'))
        showplot<- ggplot(filter(dc_output(),domain%in%input$dc_subdomain&
                                   site==input$sitename_dc), aes(x=site,y=prop_total_change))+
          geom_segment(aes(x=site,xend=site,y=0,yend=prop_total_change))+
          geom_label(aes(x=site, y=0, label=paste0("Previous Record Count: ",format(!!sym(tc_prev), big.mark = ",",scientific = FALSE))))+
          geom_label(aes(x=site, y=prop_total_change, label=paste0("New Record Count: ",format(!!sym(tc_new), big.mark = ",",scientific = FALSE))))+
          labs(x="Site",
               y="Records Proportion Change")+
          facet_wrap(~domain)+
          theme_bw()
      }
    }
    return(showplot)
  })

  # changes between data cycles - person counts
  output$dc_domain_split_persons <- renderPlot({
    if(input$sitename_dc=="total"){
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
      }else{
        showplot<-ggplot(filter(dc_output(),domain%in%input$dc_subdomain), aes(x=site,y=prop_total_pt_change, fill=site))+
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
      }
    }
    else{
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
      }
      else{
        tc_prev<-paste0('total_ct_',config('db_previous'))
        tc_new<-paste0('total_ct_',config('db_current'))
        pc_prev<-paste0('total_pt_ct_', config('db_previous'))
        pc_new<-paste0('total_pt_ct_',config('db_current'))
        showplot<- ggplot(filter(dc_output(),domain%in%input$dc_subdomain&
                                   site==input$sitename_dc), aes(x=site,y=prop_total_pt_change))+
          geom_segment(aes(x=site,xend=site,y=0,yend=prop_total_change))+
          geom_label(aes(x=site, y=0, label=paste0("Previous Person Count: ",format(!!sym(pc_prev), big.mark = ",",scientific = FALSE))))+
          geom_label(aes(x=site, y=prop_total_change, label=paste0("New Person Count: ",format(!!sym(pc_new), big.mark = ",",scientific = FALSE))))+
          labs(x="Site",
               y="Person Proportion Change")+
          facet_wrap(~domain)+
          theme_bw()
      }
    }
    return(showplot)
  })

  output$dc_overall <- renderPlot({
    if(input$sitename_dc=='total'){
      indata <- dc_output_all()
    }
    else {
      indata <- filter(dc_output_all(),site==input$sitename_dc)
    }
    low.lim <- min(indata$prop_total_change, na.rm = TRUE)
    hi.lim  <- max(indata$prop_total_change, na.rm = TRUE)
    ggplot(indata, aes(x=site, y=domain, fill=prop_total_change))+
      geom_tile(color='white',lwd=0.5,linetype=1) +
      scale_fill_gradient(trans='log')+
      guides(fill = guide_colourbar(barwidth=0.5,
                                    barheight = 15,
                                    title = 'Proportion Total Change\n(log)')) +
      theme_bw()+
      theme(axis.text.y= element_text(hjust=1,size=12),
            axis.text.x = element_text(hjust=1,vjust=0.5,angle = 90,size=12),
            axis.title=element_text(size=16))
  })

  # value set conformance
  output$vs_plot <- renderPlot({
    if(nrow(filter(vc_vs_output(), check_type=='vs'&site==input$sitename_conf))>0){
      outplot<-ggplot(filter(vc_vs_output(), check_type=='vs'&site==input$sitename_conf), aes(x=measurement_column, y = prop_viol, fill = vocabulary_id)) +
        geom_bar(stat="identity", position="dodge") +
        geom_label(aes(x=measurement_column, y=prop_viol, label=format(tot_viol, big.mark=",")),
                   position=position_dodge(),
                   show.legend = FALSE)+
        ylim(0, 1)+
        facet_wrap(~table_application, scales="free")+
        theme_bw()+
        labs(x="Column Name",
             y="Proportion of Total Records",
             title="Violating Records per Column")}
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
    return(outplot)
  })
  output$vs_table <- DT::renderDataTable({
    if(nrow(filter(vc_vs_output(), check_type=='vs'&site==input$sitename_conf))!=0){
    outtable <-
    filter(vc_vs_output(), check_type=='vs'&site==input$sitename_conf)%>%
      group_by(site, check_type, table_application, vocabulary_id)%>%
      summarise(tot_viol=as.integer(sum(tot_viol)),
                tot_rows=as.integer(min(tot_rows)))%>% # denom is the same for all in group
      ungroup() %>%
      mutate(total_violations=format(tot_viol, big.mark=','),
              total_rows=format(tot_rows, big.mark=','))%>%
      select(-c(check_type, site, tot_viol, tot_rows))
    }
    else{
      outtable <- tibble(None="")
    }
    return(outtable)
})
  # vocabulary conformance
  output$vc_plot <- renderPlot({
    if(nrow(filter(vc_vs_output(), check_type=='vc'&site==input$sitename_conf))>0){
      outplot<-ggplot(filter(vc_vs_output(), check_type=='vc'&site==input$sitename_conf), aes(x=measurement_column, y = prop_viol, fill = vocabulary_id)) +
        geom_bar(stat="identity", position="dodge") +
        geom_label(aes(x=measurement_column, y=prop_viol, label=format(tot_viol, big.mark=",")),
                   position=position_dodge(width=0.9),
                   show.legend = FALSE)+
        ylim(0, 1)+
        facet_wrap(~table_application, scales="free")+
        theme_bw()+
        labs(x="Column Name",
             y="Proportion of Total Records",
             title="Violating Records per Column")
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
    return(outplot)
  })
  output$vc_table <- DT::renderDataTable({
    if(nrow(filter(vc_vs_output(), check_type=='vc'&site==input$sitename_conf))!=0){
    outtable <- filter(vc_vs_output(), check_type=='vc'&site==input$sitename_conf)%>%
      group_by(site, check_type, table_application, vocabulary_id)%>%
      summarise(tot_viol=as.integer(sum(tot_viol)),
                tot_rows=as.integer(min(tot_rows)))%>% # denom is the same for all in group
      ungroup() %>%
      mutate(total_violations=format(tot_viol, big.mark=','),
             total_rows=format(tot_rows, big.mark=','))%>%
      select(-c(check_type, site, tot_viol, tot_rows))
    }
    else{
      outtable <- tibble(None="")
    }
    return(outtable)
  })

  # unmapped concepts
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
    }
    else{
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
              axis.title=element_text(size=18),
              legend.position="none")+
        scale_color_manual(values=site_colors)+
        scale_x_continuous(breaks = pretty_breaks())
    }
    else{
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
              axis.title=element_text(size=18),
              legend.position="none")+
        scale_x_continuous(breaks=pretty_breaks())
        }
    return(outplot)
  })

  output$uc_top_tbl <- DT::renderDataTable({
    if(input$sitename_uc=="total"){
      outtable <- uc_top_output_overall()%>%
        select(unmapped_description, src_value_name, src_value, src_value_ct)
    }
    else{
      outtable <- filter(uc_top_output(), site==input$sitename_uc) %>%
        select(unmapped_description, src_value_name, src_value, src_value_ct)
    }
    return(outtable)
  }
  )

  # person records/facts plots
  ### barplot

  output$pf_mappings <- DT::renderDataTable(
    DT::datatable(
      pf_mappings,
      options=list(pageLength=5),
      rownames=FALSE
    )
  )
  output$pf_overall_bysite_plot <- renderPlot({
    if(input$sitename_pf=="total"){
      outplot <- ggplot(pf_output(), aes(x=visit_type, y = fact_visits_prop, fill=site)) +
        geom_bar(stat='identity', position="dodge")+
        facet_wrap(~check_description, scales="free")+
        theme_bw()+
        scale_fill_manual(values=site_colors)+
        labs(x="Check Description",
             y="Proportion Visits with Fact")+
        theme(axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              axis.title=element_text(size=18))+
        coord_flip()
    }
    else{
      outplot <- ggplot(filter(pf_output(),site==input$sitename_pf), aes(x=check_description, y = fact_visits_prop, label=fact_visits_prop, fill=site)) +
        geom_bar(stat='identity')+
        geom_label(fill="white")+
        facet_wrap(~visit_type, scales="free")+
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
  ### heatmap
  output$pf_overall_heat_plot <- renderPlotly({
    if(input$sitename_pf=="total"){
      outplot <- ggplot(pf_output(), aes(x=site, y=check_description, fill=fact_pts_prop))+
        geom_tile() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        labs(x="",y="",
             fill="Proportion Patients\nwith Fact")+
        facet_wrap(~visit_type, scales = "free_x")
    }
    else{
      outplot <- ggplot(filter(pf_output(),site==input$sitename_pf), aes(x=site, y=check_description, fill=fact_pts_prop))+
        geom_tile() +
        labs(y="",
             fill="Proportion Patients\nwith Fact")+
        facet_wrap(~visit_type, scales = "free_x")
    }
    return(outplot)
  })
  # best mapped concepts

  # ----- NEW BMC
  # best mapped concepts
  output$bmc_overall_plot <- renderPlotly({
    if(input$sitename_bmc=="total"){
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
    }
    else{
      outplot <- ggplot(filter(bmc_pp(), site==input$sitename_bmc)%>%
                          mutate(include_new=case_when(include_new==0~"No",
                                                       include_new==1~"Yes")),
                        aes(x=check_desc, y=best_row_prop, fill=include_new, text=paste0("Proportion ",include_new, ": ", round(best_row_prop,2),
                                                                                         "\n",
                                                                                         "Top non-best: ", top5)))+
  geom_bar(stat='identity', position='stack')+
        labs(x="Check Type",
             y="Proportion Best Mapped",
             fill="Best Mapped")+
        coord_flip()+
        theme_bw()
    }
    return(ggplotly(outplot,tooltip="text"))
  })

  #------------

  # facts over time plots
  # overall plot
  output$fot_summary_plot <- renderPlot({
        showplot <- ggplot(
          fot_output_summary() %>% filter(
            check_desc==input$fot_subdomain_overall
          )
        ) + geom_line(aes(x=month_end,y=distance,group=site,color=site), linewidth=1) +
           scale_color_manual(values=site_colors,
                              breaks=fot_output_summary() %>% distinct(site) %>% pull())+
          scale_x_date(limits = c(input$date_fot_min, input$date_fot_max))+
          theme_bw()+
          theme(axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title = element_text(size=16)) +
          labs(x="Month/Year")
  return(showplot)
  })
  # site-specific plot
  output$fot_plot <- renderPlot({
    if(length(input$fot_subdomain_site)==0){
      showplot <- ggplot() +
        geom_blank() +
        annotate("text",label='Select a domain and specific check', x=0,y=0)+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(x="",
             y="")
  }else{
    if(input$fot_bounds=='No Bounds'){
      showplot<-
        ggplot(
          filter(
            fot_output(),
            check_desc%in%c(input$fot_subdomain_site),
            site %in% c(input$sitename_fot)
            ))+
        geom_line(aes(x=month_end,y=check,color=site)) +
        theme_bw()+
        theme(axis.text.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title = element_text(size=16),
              legend.position='none') +
        scale_color_manual(values=site_colors,
                            breaks=fot_output() %>% distinct(site) %>% pull()) +
        scale_x_date(limits = c(input$date_fot_min, input$date_fot_max))+
        labs(x="Month/Year")+
        facet_wrap(~check_desc)
    }
    else{
      showplot<- ggplot(filter(fot_output(),check_desc%in%c(input$fot_subdomain_site), site==input$sitename_fot))+
        geom_line(aes(x=month_end,y=check, group=site,color=site)) +
        geom_line(aes(x=month_end, y=m+std_dev*as.numeric(input$fot_bounds))) +
        geom_line(aes(x=month_end, y=m-std_dev*as.numeric(input$fot_bounds))) +
        theme_bw()+
        theme(axis.text.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title = element_text(size=16),
              legend.position='none') +
        scale_color_manual(values=site_colors,
                            breaks=fot_output() %>% distinct(site) %>% pull()) +
        scale_x_date(limits = c(input$date_fot_min, input$date_fot_max))+
        labs(x="Month/Year")+
        facet_wrap(~check_desc)
    }

    return(showplot)
  }
  })

  # domain concordance
  observeEvent(dcon_output(), {
    choices_new<-unique(dcon_output()$check_name)
    updateCheckboxGroupInput(inputId="dcon_check", choices=choices_new)
  })
  # plot of domain concordance over time
  # output$dcon_time_plot <- renderPlot({
  #   if(length(input$dcon_check)==0){
  #     showplot <- ggplot() +
  #       geom_blank() +
  #       annotate("text",label='Select a specific check', x=0,y=0)+
  #       theme(axis.text.x=element_blank(),
  #             axis.ticks.x=element_blank(),
  #             axis.text.y=element_blank(),
  #             axis.ticks.y=element_blank(),
  #             panel.grid.major = element_blank(),
  #             panel.grid.minor = element_blank())+
  #       labs(x="",
  #            y="")
  #   }else{
  #     showplot <- ggplot(filter(dcon_output_byyr(),
  #                                 site==input$sitename_dcon&
  #                                 yr>=input$date_dcon_range[1],yr<=input$date_dcon_range[2]&
  #                                 check_name%in%input$dcon_check)) +
  #       geom_line(aes(x=yr,y=value_pts,group=cohort,color=cohort, linetype="patients"), linewidth=1) +
  #       geom_line(aes(x=yr,y=value_visits,group=cohort,color=cohort, linetype="visits"), linewidth=1) +
  #       scale_linetype_manual("concordance\ntype", values=c("patients"="solid", "visits"="dotted"))+
  #       theme_bw()+
  #       scale_x_continuous(breaks=pretty_breaks())+
  #       labs(x="Year",
  #            y="Count")+
  #       facet_wrap(~check_name)
  #   }
  #   return(showplot)
  # })

  # domain concordance over all time
  output$dcon_overall_plot <- renderPlot({
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
    }else{
      showplot <- ggplot(filter(dcon_output(),
                                  site==input$sitename_dcon&
                                  check_name%in%input$dcon_check)) +
        geom_bar(aes(x=cohort,y=yr_prop,fill=cohort), stat='identity') +
        geom_label(aes(x=cohort, y=yr_prop, label=round(yr_prop, 2)))+
        theme_bw()+
        theme(axis.text.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16),
              legend.position = "none")+
        labs(x="Cohort",
             y="Proportion")+
        facet_wrap(~check_name)
    }
    return(showplot)
  })

  # facts with missing visit id plots
  # overall
  output$mf_visitid_overall <- renderPlot({
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
        facet_wrap(~domain, scales="free_y")

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
        labs(x="Site",
             y="Proportion Missing visit_occurrence_id")+
        coord_flip()
    }
    return(showplot)
  })

})



