library(shiny)
library(shinycssloaders)
library(ggplot2)
library(tidyr)
library(latex2exp)
library(htmltools)
library(rmarkdown)
library(markdown) # just to be safe lol
source("leaf_eb_still.R")

theme_set(
  theme_bw() + theme(
    axis.text = element_text(size=16),
    axis.title = element_text(size=20),
    legend.title = element_text(size=16),
    legend.text = element_text(size=16),
    legend.position = "none"
  )
)

# Global vars that control the sensitivity plot
VAR_NAMES <- list(
  "Emissivity"="a_lw",
  "Shortwave absorptance"="a_sw",
  "Stomatal conductance (mol m-2 s-1)"="gs",
  "Atmospheric Pressure (kPa)"="Pa",
  "Relative humidity"="RH",
  "Downwelling shortwave radiation (W m-2 s-1)"="SW_dir",
  "Downwelling longwave radiation (W m-2 s-1)"="LW_down",
  "Air temperature (K)"="Ta",
  "Wind speed (m/s)"="wind"
)

N_POINTS <- 10

ui <- fluidPage(
  
  #titlePanel("Shiny tealeaves"),
  
  # Make MathJax available
  withMathJax(),

  fluidRow(
    
    # Tabs to plot leaf T as a function of a variable or look at the detailed
    # components of the energy balance budget.
    column(
      8,
      tabsetPanel(
        
        tabPanel(
          "About",
          includeMarkdown("info.md"),
          h4("Input variables"),
          tableOutput("input_var_description"),
          h4("Output variables"),
          tableOutput("output_var_description"),
          HTML("<p>Written by <a href=https://github.com/s-kganz>me</a> <a href=https://github.com/s-kganz/shinytealeaves>(source)</a>")
        ),
        
        tabPanel(
          "Energy budget",
          withSpinner(plotOutput("plot_energy_budget")),
          tableOutput("single_leaf_t")
        ),
        
        tabPanel(
          "Sensitivity plot",
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
    ),
    
    # Define all the model parameters. One column for leaf params, one
    # for env params.
    
    # Leaf parameters
    sidebarPanel(
      width = 2,
      style = "height: 90vh; overflow-y: auto;",
      
      sliderInput("a_lw", "Emissivity (unitless)",
                  min = 0.8, max = 1,
                  value = 0.95, step=0.01),
       
      sliderInput("a_sw", "Shortwave absorptance (unitless)",
                  min = 0, max = 1,
                  value = 0.5, step=0.01),
      
      sliderInput(
        "gs", 
        "Stomatal conductance (mol m\\(^{-2}\\) s\\(^{-1}\\))",
        min = 0, max = 1, value = 0.1, step=0.01
      ),
      
    ),
    # Env parameters
    sidebarPanel(
      width = 2,
      style = "height: 90vh; overflow-y: auto;",
      
      sliderInput("Pa", 
                  "Atmospheric pressure (kPa)",
                  min = 0, max = 100,
                  value = 100, step=1),
      
      sliderInput("RH", 
                  "Relative humidity",
                  min = 0, max = 1,
                  value = 0.5, step=0.01),
      
      sliderInput("SW_dir", 
                  "Downwelling shortwave radiation (W m\\(^{-2}\\))",
                  min = 0, max = 1200,
                  value = 800, step=10),
      
      sliderInput("LW_down",
                  "Downwelling longwave radiation (W m\\(^{-2}\\))",
                  min = 0, max = 400,
                  value=300, step=10),
      
      sliderInput("Ta", 
                  "Air temperature (K)",
                  min = 275, max = 325,
                  value = 298, step=1),
      
      sliderInput("u", 
                  "Wind speed (m s\\(^{-1}\\))",
                  min = 0, max = 20,
                  value = 2, step=0.2),
      
    )
  )
)

server <- function(input, output) {
  
  get_model_parameters <- reactive({
    
    # Later functions may need to modify these lists, so read all the values
    # of the input to make them editable later
    this_envpar <- eb_envpar
    this_constants <- eb_constants
    
    envpar_common <- intersect(names(eb_envpar), names(input))
    constants_common <- intersect(names(eb_constants), names(input))
    
    for (name in envpar_common) {this_envpar[name] <- input[[name]]}
    for (name in constants_common) {this_constants[name] <- input[[name]]}
    
    list(envpar=this_envpar, constants=this_constants)

  })
  
  get_single_t_leaf <- reactive({
    
    model_parameters <- get_model_parameters()
    envpar <- model_parameters$envpar
    constants <- model_parameters$constants
    
    leaf_temperature_isothermal(
      Ta=envpar$Ta, Pa=envpar$Pa, RH=envpar$RH, u=envpar$u, 
      gs=envpar$gs,
      SW_dir=envpar$SW_dir, SW_dif=0, SW_out=0, LW_down=envpar$LW_down,
      G=0, constants=constants
    )
  })
  
  output$single_leaf_t <- renderTable({
    
    model_output <- get_single_t_leaf()
    
    values <- model_output[c("Tl", "omega", "gtot", "LE", "Rn")] 
    
    parnames <- c(
      "Leaf temperature (K)", 
      "Decoupling coefficient",
      "Total conductance (mol / m^2 sec)",
      "Latent heat flux (W / m^2)",
      "Net absorbed radiation (W / m^2)"
    )
    
    tibble(
      Parameter=parnames,
      Value=values
    )
    
  }, align="lr", rownames=FALSE)
  
  output$plot_energy_budget <- renderPlot({
    
    model_output <- get_single_t_leaf()
    
    # Calculate the individual temperature forcings
    R_emit <- input$a_lw * 5.67e-8 * input$Ta^4
    R_abs  <- model_output$Rn + R_emit
    
    R_emit_forcing <- model_output$dT_coef * R_emit * -1
    LE_forcing <- model_output$dT_coef * model_output$LE * -1
    R_abs_forcing <- model_output$dT_coef * R_abs
    net_dt <- model_output$Tl - input$Ta
    
    plot_df <- data.frame(
      x=c("Absorbed radiation", "Emitted radiation", "Latent heat", 
          "Net leaf - air T difference"),
      y=c(R_abs_forcing, R_emit_forcing, LE_forcing, net_dt)
    )
    plot_df$label <- paste(sprintf("%+.2f", plot_df$y), "K")
    
    ggplot(plot_df, aes(x=x, y=y)) + 
      geom_bar(aes(fill=x), stat="identity") +
      geom_label(aes(y=0, label=label), size=5) +
      labs(x="", y="Temperature forcing (K)") +
      ylim(-10, 10)
    
  })
  
  output$plot_param_change <- renderPlot({
    # Get the variable that we are plotting over
    xax_var <- VAR_NAMES[[input$plot_var]]
    
    if (!is.finite(input$plot_var_lower)) {}
    
    xax_values <- seq(
      # Validate input
      from=ifelse(is.finite(input$plot_var_lower), input$plot_var_lower, 0),
      to=ifelse(is.finite(input$plot_var_upper), input$plot_var_upper, 0),
      length.out=N_POINTS
    )
    
    # Update the model input
    model_parameters <- get_model_parameters()
    envpar <- model_parameters$envpar
    constants <- model_parameters$constants
    
    if (xax_var %in% names(constants)) {
      constants[[xax_var]] <- xax_values
    } else {
      envpar[[xax_var]] <- xax_values
    }
    
    # Run the model
    model_output <- leaf_temperature_isothermal(
      Ta=envpar$Ta, Pa=envpar$Pa, RH=envpar$RH, u=envpar$u, 
      gs=envpar$gs, # mmol to mol 
      SW_dir=envpar$SW_dir, SW_dif=0, SW_out=0, LW_down=envpar$LW_down,
      G=0, constants=constants
    )
    
    output_tl <- model_output$Tl
    
    ggplot(NULL) +
      geom_line(aes(x=xax_values, y=output_tl, color="Leaf"), linewidth=2) +
      geom_point(aes(x=xax_values, y=output_tl, color="Leaf"), size=3) +
      geom_line(aes(x=xax_values, y=envpar$Ta, color="Air"), linewidth=2) +
      labs(x=input$plot_var,
           y="Temperature (K)",
           color="") +
      scale_color_manual(
        values=c("Leaf"="forestgreen", "Air"="black")
      ) +
      theme(legend.position = "right")
  })
  
  output$input_var_description <- renderTable({
    tab <- data.frame(
      matrix(
        c("Emissivity", "Fraction of infrared light absorbed by the leaf surface",
          "Shortwave absorptance", "Fraction of visible light absorbed by the leaf surface",
          "Stomatal conductance", "Rate at which water diffuses out of the leaf",
          "Atmospheric pressure", "Barometric pressure of air around the leaf",
          "Relative humidity", "Relative amount of moisture in the air",
          "Downwelling shortwave radiation", "Amount of energy in the form of visible light reaching the leaf surface",
          "Downwelling longwave radiation", "Amount of energy in the form of infrared light reaching the leaf surface",
          "Air temperature", "Absolute temperature of air around the leaf",
          "Wind speed", "Speed of air moving along the surface of the leaf"),
        ncol=2, byrow=TRUE
      )
    )
    names(tab) <- c("Parameter", "Description")
    return(tab)
  })
  
  output$output_var_description <- renderTable({
    tab <- data.frame(
      matrix(
        c("Leaf temperature", "Absolute temperature of the leaf surface",
          "Decoupling coefficient", "Number in range 0-1 indicating whether the latent heat flux is determined by stomata (0) or surrounding air (1)",
          "Total conductance", "Total conductance of water vapor, accounting for the leaf boundary layer and stomatal conductance",
          "Latent heat flux", "Amount of energy leaving the leaf due to evaporated water",
          "Net absorbed radiation", "Amount of energy reaching the leaf surface due to surrounding light"),
        ncol=2, byrow=TRUE
      )
    )
    names(tab) <- c("Parameter", "Description")
    return(tab)
  })
  
}

shinyApp(ui, server)