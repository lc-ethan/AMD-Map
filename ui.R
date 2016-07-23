




shinyUI(
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("md_map", width = "100%", height = "100%"),
    absolutePanel(
      top = 3, right = 3,
      conditionalPanel(
        condition = "input.param_ts_loc",
        highchartOutput("md_ts", height = 250, width = 400)
      )

    ),
    
    absolutePanel(
      bottom = 10, left = 10,
      radioButtons(
        inputId = "param_project_select",
        label = "Projection",
        choices = vt.param_project_select,
        selected = vt.param_project_select[1], 
        inline = TRUE
      ),
      sliderInput(
        inputId = "param_time_select", label = "Year",
        min = min(vt.param_time_select), 
        max = max(vt.param_time_select),
        step = unique(diff(vt.param_time_select)),
        value = min(vt.param_time_select)
      ),
      checkboxInput(
        inputId = "param_legend_loc",
        label = "Show Legend",
        value = TRUE
      ),
      checkboxInput(
        inputId = "param_ts_loc",
        label = "Show Prevalence Projection",
        value = TRUE
      )
    )
  )
)


