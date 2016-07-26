




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
        style = "background-color: #ffffff80;width:280px;text-align:left;",
        div(
          #style = "margin:5px;",
          htmlOutput("md_tooltip") 
        )
      ),
      br(),
      
      radioButtons(
        inputId = "param_project_select",
        label = "Projection Series:",
        choices = vt.param_project_select,
        selected = vt.param_project_select[1], 
        inline = TRUE
      ),
      
      bsTooltip(id = "param_project_select", 
                title = "Different population projection type reflecting different assumptions sourced from Statistics New Zealand",
                "top", trigger = "hover"),
      
      sliderInput(
        inputId = "param_time_select", label = "Projection Year:",
        min = min(vt.param_time_select), 
        max = max(vt.param_time_select),
        step = unique(diff(vt.param_time_select)),
        value = min(vt.param_time_select)
      ),
      
      bsTooltip(id = "param_time_select", 
                title = "The year of the population projection that the AMD prevalence estimate is based on in the spatial heatmap",
                "top", trigger = "hover"),
      
      checkboxInput(
        inputId = "param_legend_loc",
        label = "Show Legend",
        value = TRUE
      ),
      checkboxInput(
        inputId = "param_ts_loc",
        label = "Show AMD Prevalence Projection",
        value = TRUE
      ),
      div(
        style = "text-align:center;",
        actionButton("param_help", "About AMD", width = "250px",
                     class = "btn btn-primary btn-md") 
      )
    ),
    bsModal(
      id = "md_help",
      title = "Age-related Macular Degeneration", size = "large", trigger = "param_help",
      HTML(
           '<p style="text-align:justify">The macula is the central part of the retina, 
           the light-sensitive tissue at the back of the eye. The retina processes all visual images. It is 
           responsible for your ability to read, recognise faces, drive and see colours clearly. Macular Degeneration (MD)
           causes progressive macular damage resulting in loss of central vision but the peripheral vision is not affected.</p>
           
           <p style="text-align:justify"> Age-related macular degeneration (AMD) is the leading cause of visual loss in individual
           older than 50 years in New Zealand, as it is for the developed world as a whole. 
           49% of blind registrations in New Zealand are for AMD. New Zealand is entering a period of 
           demographic shift to an ageing population; thus the ageing of the population implies a rising 
           prevalence of AMD with an associated treatment burden.</p>
           
           <p style="text-align:justify"> 
           Reference: <a href="http://www.nzma.org.nz/journal/read-the-journal/all-issues/2010-2019/2015/vol-128-no-1409/6438" target="_blank">
           Prevalence predictions for age-related macular degeneration in New Zealand have implications for provision of healtcare services</a>'
      )
    )
  )
)


