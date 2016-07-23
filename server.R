
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


server <- function(input, output, session) {
  source("./tools/server - map.R", local = TRUE)
    # source("./tools/server - overview data creation.R", local = TRUE)
  # source("./tools/server - overview panel.R", local = TRUE)
  # source("./tools/server - comparative data creation.R", local = TRUE)
  # source("./tools/server - comparative panel.R", local = TRUE)
}
