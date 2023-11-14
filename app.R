library(shiny)
library(shinycssloaders)
library(ggplot2)
library(tidyr)
library(tealeaves)
library(units)
library(latex2exp)

theme_set(
  theme_bw() + theme(
    axis.text = element_text(size=16),
    axis.title = element_text(size=20),
    legend.position = "none"
  )
)

# Global vars that control the sensitivity plot
VAR_NAMES <- list(
  "Emissivity"="abs_l",
  "Shortwave absorptance"="abs_s",
  "Stomatal conductance (umol m-2 s-1 Pa-1)"="g_sw",
  "Cuticular conductance (umol m-2 s-1 Pa-1)"="g_uw",
  "Leaf size (cm)"="leafsize",
  "Atmospheric Pressure (kPa)"="P",
  "Albedo (unitless)"="r",
  "Relative humidity"="RH",
  "Downwelling shortwave radiation (umol m-2 s-1)"="S_sw",
  "Air temperature (K)"="T_air",
  "Wind speed (m/s)"="wind"
)
N_POINTS <- 10

ui <- fluidPage(
  
  titlePanel("Shiny tealeaves"),
  
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
                  min = 0.8, max = 1,
                  value = 0.95, step=0.01),
       
      sliderInput("abs_s", "Shortwave absorptance (unitless)",
                  min = 0, max = 1,
                  value = 0.5, step=0.01),
      
      sliderInput(
        "g_sw", 
        "Stomatal conductance (mmol m\\(^{-2}\\) s\\(^{-1}\\))",
        min = 0, max = 1000, value = 100, step=10
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
      
      sliderInput("RH", 
                  "Relative humidity",
                  min = 0, max = 1,
                  value = 0.5, step=0.01),
      
      sliderInput("SW_down", 
                  "Downwelling shortwave radiation (W m\\(^{-2}\\))",
                  min = 0, max = 1200,
                  value = 800, step=10),
      
      sliderInput("LW_down",
                  "Downwelling longwave radiation (W m\\(^{-2}\\))",
                  min = 0, max = 400,
                  value=300, step=10),
      
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
          "Energy budget",
          withSpinner(plotOutput("plot_energy_budget")),
          textOutput("single_leaf_t")
        ),
        
        tabPanel(
          "Plot",
          fluidRow(withSpinner(plotOutput("plot_param_change"))),
          fluidRow(
            column(
              width=6,
              # Selectors
              selectInput("plot_var", "Variable", names(VAR_NAMES)),
              numericInput("plot_var_lower", "Lower bound", 0.90),
              numericInput("plot_var_upper", "Upper bound", 1.00),
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  get_single_leaf_parameters <- reactive({
    # Stub
  })
  
  get_single_t_leaf <- reactive({
    
    list(
      Tleaf=0,
      LE=100,
      R_in=200,
      R_out=300
    )
    
  })
  
  output$single_leaf_t <- renderText({
    paste0("Leaf temperature is ",
           format(get_single_t_leaf()$Tleaf, digits=2), 
           " K")
  })
  
  output$plot_energy_budget <- renderPlot({
    ggplot(NULL, aes(x=1:10, y=1:10)) + geom_point()
  })
  
  
  output$plot_param_change <- renderPlot({
    ggplot(NULL, aes(x=1:10, y=1:10)) + geom_point()
  })
  
}

shinyApp(ui, server)