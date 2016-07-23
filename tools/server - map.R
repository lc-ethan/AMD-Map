### spatial map ----
df.map_meta <- df.db_meta %>% 
  filter(proc %in% "data_map")

for (i in df.map_meta$name) {
  local({
    vt.name_cur <- i
    vt.group_cur <- df.map_meta %>% 
      filter(name %in% vt.name_cur) %>% 
      .[["group"]]
    
    vt.trigger <- paste(vt.group_cur, vt.name_cur, sep = "$")
    
    observeEvent(eval(parse(text = vt.trigger)), {
      ## assign selected widgets
      vt.param_time <- input$param_time_select
      vt.param_project <- input$param_project_select
      
      ## create data for spatial map
      md_base$data_map <- df.db_input %>% 
        filter(PROJECTION %in% vt.param_project) %>% 
        filter(YEAR %in% vt.param_time) %>% 
        mutate(INDEX = MD_STD * 100) %>% 
        group_by(TA) %>% 
        mutate(POPUP = paste(paste0("<b>", convert_cap(TA), " (", YEAR, ")</b><br/>"),
                             "Prevalence Estimate:  <b>", format(round(MD, 0), big.mark = ","), " (", round(INDEX, 2), "%)", "</b><br/>",
                             "Population: <b>",  format(BASE, big.mark = ","), "</b><br/>",
                             sep = "")) %>% 
        select(TA, INDEX, POPUP)
      
    })
  })
}

output$md_map <- renderLeaflet({
  df.plot <- df.initial_map
  spldf.plot <- spldf.db_nz
  
  spldf.plot@data <- spldf.plot@data %>% 
    left_join(df.plot, by = "TA")

  mat.bounds <- bbox(spldf.plot)
  
 leaflet(data = spldf.plot) %>% 
    addTiles() %>% 
    fitBounds(lng1 = mat.bounds[1, 1], lat1 = mat.bounds[2, 1],
              lng2 = mat.bounds[1, 2], lat2 = mat.bounds[2, 2]) %>% 
    addPolygons(
      stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5, popup = spldf.plot$POPUP,
      fillColor =  ~colorNumeric("YlOrRd", spldf.plot$INDEX)(INDEX),
      dashArray = "5, 5", color = "white") %>% 
    addLegend(position = "bottomright", pal = colorNumeric("YlOrRd", spldf.plot$INDEX),
              values = spldf.plot$INDEX , title = "Prevalence Estimate (%)")
})

observe({
  df.plot <- md_base$data_map
  spldf.plot <- spldf.db_nz

  spldf.plot@data <- spldf.plot@data %>% 
    left_join(df.plot, by = "TA")
  
  mat.bounds <- bbox(spldf.plot)
  
  ll.base <- leafletProxy("md_map", data = spldf.plot) %>%
    clearShapes() %>%
    clearControls() %>%
    clearPopups() %>% 
    fitBounds(lng1 = mat.bounds[1, 1], lat1 = mat.bounds[2, 1],
              lng2 = mat.bounds[1, 2], lat2 = mat.bounds[2, 2]) %>% 
    addPolygons(stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5, popup = spldf.plot$POPUP,
                fillColor =  ~colorNumeric("YlOrRd", spldf.plot$INDEX)(INDEX),
                dashArray = "5, 5", color = "white")
})


observeEvent(input$param_legend_loc, {
  vt.param_legend_loc <- input$param_legend_loc
  proxy <- leafletProxy("md_map") %>%
    clearControls()
  
  if (vt.param_legend_loc) {

    df.plot <- md_base$data_map
    spldf.plot <- spldf.db_nz

    spldf.plot@data <- spldf.plot@data %>%
      left_join(df.plot, by = "TA")

    mat.bounds <- bbox(spldf.plot)
    
    proxy <- proxy %>% 
      addLegend(position = "bottomright", pal = colorNumeric("YlOrRd", spldf.plot$INDEX),
                values = spldf.plot$INDEX , title = "Prevalence Estimate (%)")
  }
  proxy
})