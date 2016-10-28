### time series ----
## data creation for time series chart
df.md_meta_ts <- df.db_meta %>% 
  filter(proc %in% "data_ts")

for (i in df.md_meta_ts$name) {
  local({
    vt.name_cur <- i
    vt.group_cur <- df.md_meta_ts %>% 
      filter(name %in% vt.name_cur) %>% 
      .[["group"]]
    
    vt.trigger <- paste(vt.group_cur, vt.name_cur, sep = "$")
    
    observeEvent(eval(parse(text = vt.trigger)), {
      vt.param_region <- md_base$param_region_select
      vt.param_project <- input$param_project_select
      
      if (vt.param_region %in% c("region_selected", "")) {
        md_base$data_ts <- df.db_input %>%
          filter(PROJECTION %in% vt.param_project) %>% 
          group_by(YEAR) %>%
          summarise(INDEX = sum(MD, na.rm = TRUE))
      }
      else {
        md_base$data_ts <- df.db_input %>%
          filter(TA %in% vt.param_region) %>% 
          filter(PROJECTION %in% vt.param_project) %>% 
          group_by(YEAR) %>%
          summarise(INDEX = sum(MD, na.rm = TRUE))
      }
    })
  })
}

output$md_ts <- renderHighchart({
  df.input <- md_base$data_ts
  vt.param_time <- input$param_time_select
  vt.param_region <- md_base$param_region_select
  
  vt.title_region <- ifelse(vt.param_region %in% c("", "region_selected"), "NZ", vt.param_region)
  vt.title_region <- gsub("district", "", vt.title_region)
  vt.title_region <- gsub("city", "", vt.title_region)
  vt.title_region <- convert_cap(vt.title_region)
  vt.title <- paste0("<b>Prevalence Prediction (45-85 Years) in ", 
                     vt.title_region, "</b>")
  
  df.plot <- df.input %>%
    mutate(color = ifelse(YEAR %in% vt.param_time, "red", "orange")) %>% 
    change_names("INDEX", "y", reminder = FALSE) %>% 
    highcharter::list_parse()
  
  hc.hist <- highchart(height = 250, width = 100) %>% 
    hc_chart(type = "column") %>% 
    hc_subtitle(text = vt.title) %>% 
    hc_xAxis(title = list(text = "Year"), 
             categories = df.input$YEAR) %>% 
    hc_yAxis(title = list(text = "Prevalence Prediction (#)")) %>% 
    hc_add_series(data = df.plot) %>% 
    hc_tooltip(
      headerFormat = "<b>{point.x}</b> <br>",
      pointFormat = paste0("Prevalence Prediction (#)", ": <b>{point.y:,.0f}</b>")
    ) %>% 
    hc_plotOptions(
      series = list(
        showInLegend = FALSE,
        pointPadding = 0,
        groupPadding = 0.05
      )
    )
  hc.hist
})