library(shiny)
library(tealeaves)
library(units)

ui <- fluidPage(
  
  titlePanel("Sliders"),
  
  # Make MathJax available
  withMathJax(),

  fluidRow(
    
    # Define all the tealeaves parameters. One column for leaf params, one
    # for env params.
    
    # Leaf parameters
    sidebarPanel(
      width = 2,
      style = "height: 90vh; overflow-y: auto;",
      
      sliderInput("abs_l", "Emissivity (unitless)",
                  min = 0, max = 1,
                  value = 0.97, step=0.01),
       
      sliderInput("abs_s", "Shortwave absorptance (unitless)",
                  min = 0, max = 1,
                  value = 0.5, step=0.01),
      
      sliderInput(
        "g_sw", 
        "Stomatal conductance (umol m\\(^{-2}\\) s\\(^{-1}\\) Pa\\(^{-1}\\))",
        min = 0, max = 100, value = 5, step=0.1
      ),
      
      sliderInput(
        "g_uw", 
        "Cuticular conductance (umol m\\(^{-2}\\) s\\(^{-1}\\) Pa\\(^{-1}\\))",
        min = 0, max = 10, value = 0.5, step=0.1
      ),
      
      sliderInput("leafsize", 
                  "Leaf size (cm)",
                  min = 0, max = 50,
                  value = 10, step=1),
      
    ),
    # Env parameters
    sidebarPanel(
      width = 2,
      style = "height: 90vh; overflow-y: auto;",
      
      sliderInput("P", 
                  "Atmospheric pressure (kPa)",
                  min = 0, max = 100,
                  value = 100, step=1),
      
      sliderInput("r", 
                  "Albedo (unitless)",
                  min = 0, max = 1,
                  value = 0.2, step=0.01),
      
      sliderInput("RH", 
                  "Relative humidity (%)",
                  min = 0, max = 100,
                  value = 50, step=1),
      
      sliderInput("S_sw", 
                  "Downwelling shortwave radiation (W m\\(^{-2}\\))",
                  min = 0, max = 1200,
                  value = 1000, step=10),
      
      sliderInput("T_air", 
                  "Air temperature (K)",
                  min = 275, max = 325,
                  value = 298, step=1),
      
      sliderInput("wind", 
                  "Wind speed (m s\\(^{-1}\\))",
                  min = 0, max = 20,
                  value = 2, step=0.2),
      
    ),
    # Tabs to plot leaf T as a function of a variable or look at the detailed
    # components of the energy balance budget.
    column(
      8,
      tabsetPanel(
        tabPanel(
          "Plot",
          plotOutput("plot")
        ),
        tabPanel(
          "Details",
          textOutput("leaf_t")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  get_t_leaf <- reactive({
    # Collect all the parameters
    env_parameters <- make_enviropar(
      list(
        P     = set_units(input$P, "kPa"),
        r     = set_units(input$r, "1"),
        RH    = set_units(input$RH/100, "1"),
        S_sw  = set_units(input$S_sw, "W/m^2"),
        T_air = set_units(input$T_air, "K"),
        wind  = set_units(input$wind, "m/s")
      )
    )
    
    leaf_parameters <- make_leafpar(
      replace = list(
        abs_l    = set_units(input$abs_l, "1"),
        abs_s    = set_units(input$abs_s, "1"),
        g_sw     = set_units(input$g_sw, "umol/m^2/s/Pa"),
        g_uw     = set_units(input$g_uw, "umol/m^2/s/Pa"),
        leafsize = set_units(input$leafsize, "m")
      )
    )
    
    # And get the modeled leaf temperature
    result <- tleaf(
      leaf_parameters, env_parameters, make_constants(), quiet=TRUE
    )
    
    # Drop units to play nice with renderText()
    drop_units(result$T_leaf)
    
  })
  
  output$leaf_t <- renderText({
    paste0(format(get_t_leaf(), digits=2), " K")
  })
  
}

shinyApp(ui, server)