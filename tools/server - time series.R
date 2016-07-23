### time series ----

output$md_ts <- renderHighchart({
  df.input <- md_base$data_ts
  vt.param_time <- input$param_time_select
  
  df.plot <- df.input %>%
    mutate(color = ifelse(YEAR %in% vt.param_time, "red", "orange")) %>% 
    change_names("INDEX", "y", reminder = FALSE) %>% 
    highcharter::list.parse3()
  
  hc.hist <- highchart(height = 250, width = 100) %>% 
    hc_chart(type = "column") %>% 
    #hc_title(text = paste("Prevanlence Estimate", paste(range(vt.param_time_select), collapse = " - "))) %>%
    hc_subtitle(text = "<b>Prevanlence Estimate with European and Asian Combined (45-85 Years)</b>") %>% 
    hc_xAxis(title = list(text = "Time"), 
             categories = df.input$YEAR) %>% 
    hc_yAxis(title = list(text = "Prevanlence Estimate (#)")) %>% 
    hc_add_series(data = df.plot) %>% 
    hc_tooltip(
      headerFormat = "<b>{point.x}</b> <br>",
      pointFormat = paste0("Prevanlence Estimate (#)", ": <b>{point.y:,.0f}</b>")
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