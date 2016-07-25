




shinyUI(
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("md_map", width = "100%", height = "100%"),
    
    absolutePanel(
      top = 12, left = 50,
      div(
        style = "display:inline-block;vertical-align:top;margin-right:10px;",
        img(src = "./img/logo.png", height = "45px")
      ),

      div(
        style = "display:inline-block;vertical-align:top;margin-top: -10px;",
        p(h4(strong("Age-related Macular Degeneration (AMD) is a growing problem in NZ")),
          h5(strong(textOutput("md_subtitle"))))
      )
    ),
    
    # absolutePanel(
    #   top = 70, left = 5,
    #   div(
    #     style = "background-color: #fffff80;width:280px;text-align:left;",
    #     div(
    #       style = "margin:5px;",
    #       htmlOutput("md_tooltip") 
    #     )
    #   )
    # ),


    
    
    absolutePanel(
      top = 3, right = 5,
      
      div(
       style = "text-align:right;",
       conditionalPanel(
         condition = "input.param_ts_loc",
         highchartOutput("md_ts", height = 300, width = 450)
       )
      )
    ),
    
    
    
    absolutePanel(
      bottom = 10, left = 10,
      div(
        style = "background-color: #fffff80;width:280px;text-align:left;",
        div(
          #style = "margin:5px;",
          htmlOutput("md_tooltip") 
        )
      ),
      br(),

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


