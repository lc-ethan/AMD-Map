
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


server <- function(input, output, session) {
  ### initial data setup ----
  md_base <- reactiveValues(data_map = df.initial_map,
                            data_ts = df.initial_ts,
                            hl_md = vt.hl_md,
                            hl_time_select = vt.param_time_select[1],
                            param_region_select = "",
                            param_region_hover = "",
                            tooltip = NULL)
  
  source("./tools/server - map.R", local = TRUE)
  source("./tools/server - time series.R", local = TRUE)
}
