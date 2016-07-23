### pckgs version control ----
#require(checkpoint)
#checkpoint("2016-07-23", checkpointLocation = "./")

### libraries ----
require(sp)
require(rgdal)
require(rgeos)
require(plyr)
require(dplyr)
require(purrr)
require(shiny)
require(shinydashboard)
require(shinyBS)
require(leaflet)

# environment cleanup
rm(list = ls()); gc()

### directories ----
dir.data <- "./data"

### data ----
load(file.path(dir.data, "dashboard input.rda"))

vt.param_time_select <- unique(df.db_input$YEAR)
vt.param_project_select <- unique(df.db_input$PROJECTION)

df.initial_map <- df.db_input %>% 
  filter(PROJECTION %in% vt.param_project_select[1]) %>% 
  filter(YEAR %in% vt.param_time_select[1])


