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
require(rgears)
require(shiny)
require(shinydashboard)
require(shinyBS)
require(leaflet)
require(highcharter)
require(readr)

# environment cleanup
rm(list = ls()); gc()

### directories ----
dir.data <- "./data"
dir.fn <- "./R"

### data ----
load(file.path(dir.data, "dashboard input.rda"))

### utility functions ----
source(file.path(dir.fn, "utility functions.R"))

### parameter setup ----
## control widgets
vt.param_time_select <- unique(df.db_input$YEAR)
vt.param_project_select <- unique(df.db_input$PROJECTION)

## initial data
df.initial_map <- df.db_input %>% 
  filter(PROJECTION %in% vt.param_project_select[1]) %>% 
  filter(YEAR %in% vt.param_time_select[1]) %>% 
  mutate(INDEX = MD_STD * 100) %>% 
  group_by(TA) %>% 
  mutate(POPUP = paste(paste0("<b>", convert_cap(TA), " (", YEAR, ")</b><br/>"),
                       "AMD Prevalence Estimate:  <b>", format(round(MD, 0), big.mark = ","), " (", round(INDEX, 2), "%)", "</b><br/>",
                       "Population: <b>",  format(BASE, big.mark = ","), "</b><br/>",
                       sep = "")) %>% 
  select(TA, INDEX, POPUP)

df.initial_ts <- df.db_input %>%
  filter(PROJECTION %in% vt.param_project_select[1]) %>% 
  group_by(YEAR) %>%
  summarise(INDEX = sum(MD, na.rm = TRUE))

vt.hl_md <- df.initial_ts %>% 
  filter(YEAR %in% vt.param_time_select[1])


