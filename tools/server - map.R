### spatial map ----
output$test <- renderPrint({
  list(b = input$md_map_shape_mouseover$id,
       c = input$md_map_shape_mouseout$id,
       d = md_base$param_region_hover)
  #input$md_map_shape_mouseover
  # md_base$param_region_select
})

df.md_meta_map <- df.db_meta %>% 
  filter(proc %in% "data_map")

for (i in df.md_meta_map$name) {
  local({
    vt.name_cur <- i
    vt.group_cur <- df.md_meta_map %>% 
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
                             "Prevalence Prediction:  <b>", format(round(MD, 0), big.mark = ","), " (", round(INDEX, 2), "%)", "</b><br/>",
                             "Population Projection: <b>",  format(BASE, big.mark = ","), "</b><br/>",
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
      stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
      fillColor =  ~colorNumeric("YlOrRd", spldf.plot$INDEX)(INDEX),
      dashArray = "5, 5", color = "white", layerId = spldf.plot$TA) %>% 
    addLegend(position = "bottomright", pal = colorNumeric("YlOrRd", spldf.plot$INDEX),
              values = spldf.plot$INDEX , title = "MD Prevalence Estimate (%)")
})

observe({
  df.plot <- md_base$data_map
  spldf.plot <- spldf.db_nz

  spldf.plot@data <- spldf.plot@data %>% 
    left_join(df.plot, by = "TA")
  
  mat.bounds <- bbox(spldf.plot)
  
  ll.base <- leafletProxy("md_map") %>%
    clearShapes() %>%
    clearPopups() %>% 
    # fitBounds(lng1 = mat.bounds[1, 1], lat1 = mat.bounds[2, 1],
    #           lng2 = mat.bounds[1, 2], lat2 = mat.bounds[2, 2]) %>% 
    addPolygons(data = spldf.plot, stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5, 
                #popup = spldf.plot$POPUP,
                fillColor =  ~colorNumeric("YlOrRd", spldf.plot$INDEX)(INDEX),
                dashArray = "5, 5", color = "white", layerId = spldf.plot$TA)
})


## region highlights
observeEvent(input$md_map_shape_click, {
  md_base$param_region_select <- input$md_map_shape_click$id
}, ignoreNULL = FALSE)

observeEvent(input$param_time_select, {
  md_base$param_region_select <- ""
}, ignoreNULL = FALSE)


observeEvent(input$param_project_select, {
  md_base$param_region_select <- ""
}, ignoreNULL = FALSE)


observeEvent(md_base$param_region_select, {
  vt.param_region <- md_base$param_region_select

  if (!vt.param_region %in% c("region_selected", "")) {
      spldf.plot <- subset(spldf.db_nz, TA %in% vt.param_region)
      leafletProxy("md_map") %>% 
        addPolygons(data = spldf.plot, stroke = TRUE, fillOpacity = 0.7, smoothFactor = 0.5,
                    fillColor = "red",
                    dashArray = "5, 5", color = "black", layerId = "region_selected")
  }
  else {
    leafletProxy("md_map") %>%
      removeShape(layerId = "region_selected")
  }
}, ignoreNULL = FALSE)





### legend ----
observe({
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
      addLegend(position = "bottomleft", pal = colorNumeric("YlOrRd", spldf.plot$INDEX),
                values = spldf.plot$INDEX , title = "AMD Prevalence Prediction (%)")
  }
  proxy
})

### tooltip ----
observeEvent(input$md_map_shape_mouseover$id, {
  md_base$param_region_hover <- ifelse(is.null(input$md_map_shape_mouseover$id), "", 
                                       input$md_map_shape_mouseover$id)
})

# observeEvent(input$md_map_shape_mouseout$id, {
#   leafletProxy("md_map") %>%
#     removeShape(layerId = "region_hoveron")
# }, ignoreNULL = FALSE)
# 
# observeEvent(md_base$param_region_hover, {
#   #vt.param_region <- md_base$param_region_hover
# 
#   if (!md_base$param_region_hover %in% "") {
#     spldf.plot <- subset(spldf.db_nz, TA %in% md_base$param_region_hover)
#     leafletProxy("md_map") %>%
#       addPolygons(data = spldf.plot, stroke = TRUE, 
#                   fillOpacity = 0.1, smoothFactor = 0.5,
#                   color = "black", layerId = "region_hover")
#   }
# })


observeEvent(input$md_map_shape_mouseout$id, {
  md_base$tooltip <- NULL
}, ignoreNULL = FALSE)

observeEvent(input$md_map_shape_mouseover$id, {
  vt.param_region <- ifelse(input$md_map_shape_mouseover$id %in% "region_selected",
                            md_base$param_region_select, input$md_map_shape_mouseover$id)
  df.input <- md_base$data_map
  df.output <- df.input %>% 
    filter(TA %in% vt.param_region)
  md_base$tooltip <- df.output$POPUP
})

output$md_tooltip <- renderText({
  md_base$tooltip
})

### headlines ----
output$md_subtitle <- renderText({
  vt.param_time <- input$param_time_select
  vt.param_project <- input$param_project_select
  vt.param_region <- md_base$param_region_select
  df.input <- df.db_input
  
  if (!vt.param_region %in% c("", "region_selected")) {
    df.input <- df.input %>% 
      filter(TA %in% vt.param_region)
  }
  
  vt.predict <- df.input %>%
    ungroup() %>% 
    filter(YEAR %in% vt.param_time) %>% 
    filter(PROJECTION %in% vt.param_project) %>% 
    summarise(MD = format(round(sum(MD, na.rm = TRUE)), big.mark = ",")) %>% 
    .[["MD"]]
  
  vt.title_region <- ifelse(vt.param_region %in% c("", "region_selected"), "NZ", vt.param_region)
  vt.title_region <- gsub("district", "", vt.title_region)
  vt.title_region <- gsub("city", "", vt.title_region)
  vt.title_region <- convert_cap(vt.title_region)
  vt.output <- paste("Total prevalence predicted to be", vt.predict, "in", 
                     vt.param_time, "over", vt.title_region)
  
  return(vt.output)
})