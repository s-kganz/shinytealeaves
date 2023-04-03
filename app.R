library(shiny)
library(shinycssloaders)
library(ggplot2)
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
                  "Relative humidity",
                  min = 0, max = 1,
                  value = 0.5, step=0.01),
      
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
          "Fluxes",
          withSpinner(plotOutput("energy_balance")),
          textOutput("single_leaf_t")
        ),
        
        tabPanel(
          "Plot",
          fluidRow(withSpinner(plotOutput("plot"))),
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
    
    list(leaf=leaf_parameters, env=env_parameters)
  })
  
  get_single_t_leaf <- reactive({
    
    parameters <- get_single_leaf_parameters()
    #print(parameters)
    
    # And get the modeled leaf temperature
    tleaf(
      parameters$leaf, parameters$env, make_constants(), quiet=TRUE
    )
    
  })
  
  output$single_leaf_t <- renderText({
    paste0("Leaf temperature is ",
           format(drop_units(get_single_t_leaf()$T_leaf), digits=2), 
           " K")
  })
  
  output$energy_balance <- renderPlot({
    # Collect fluxes and convert to table
    t_leaf_result <- get_single_t_leaf()

    t_leaf_df <- data.frame(
      var=c(
        "Latent heat", "Sensible heat",
        "Absorbed radiation", "Emitted radiation"
      ),
      value=c(
        -drop_units(t_leaf_result$L),
        -drop_units(t_leaf_result$H),
        drop_units(t_leaf_result$R_abs),
        -drop_units(t_leaf_result$S_r)
      )
    )
    
    #head(t_leaf_df)

    # Plot the different fluxes
    ggplot(t_leaf_df, aes(x=var, y=value)) + 
      geom_bar(aes(fill=var), stat="identity") +
      labs(y="Heat density (W m^-2)", x="") +
      ylim(-1500, 1500)
      

  })
  
  get_sensitivity_leaf_parameters <- reactive({
    # Start with the single leaf parameters and then edit the one
    # being used for sensitivity
    parameters <- get_single_leaf_parameters()
    
    # Collect variable to be plotted on the x-axis
    plot_var_code <- VAR_NAMES[[input$plot_var]] # varname in code
    plot_var_lower <- input$plot_var_lower
    plot_var_upper <- input$plot_var_upper
    
    plot_var_range <- seq(from=plot_var_lower, to=plot_var_upper,
                          length.out=N_POINTS)
    
    # Accessors vary slightly depending on if the variable is leaf or
    # environmental.
    if (plot_var_code %in% names(parameters$env)) {
      set_units(plot_var_range, units(parameters$env[[plot_var_code]]), mode="standard")
      parameters$env[[plot_var_code]] <- plot_var_range
    } else {
      set_units(plot_var_range, units(parameters$leaf[[plot_var_code]]), mode="standard")
      parameters$leaf[[plot_var_code]] <- plot_var_range
    }
    
    return(parameters)
  })
  
  get_sensitivity_t_leaf <- reactive({
    parameters <- get_sensitivity_leaf_parameters()
    tleaves(parameters$leaf, parameters$env, make_constants(), quiet=TRUE)
  })
  
  output$plot <- renderPlot({
    # We need the parameters and the varname for setting up the x-axis
    par <- get_sensitivity_leaf_parameters()
    plot_var_code <- VAR_NAMES[[input$plot_var]]
    
    yax <- drop_units(get_sensitivity_t_leaf()$T_leaf)
    xax <- ifelse(plot_var_code %in% names(par$env),
                  par$env[plot_var_code],
                  par$leaf[plot_var_code])[[1]]
    
    #print(yax)
    #print(xax)
    
    ggplot(NULL) +
      geom_point(aes(x=xax, y=yax), size=2) +
      labs(x=input$plot_var, y="Leaf temperature (K)")
  })
  
}

shinyApp(ui, server)