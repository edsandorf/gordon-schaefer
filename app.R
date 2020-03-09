# Load packages ----
pkgs <- c("shiny", "shinyjs", "shinyWidgets", "ggplot2")
invisible(lapply(pkgs, require, character.only = TRUE))

# Global functions ----
# The yield-effort curve
yield_effort <- function(K, E, q, r) {
  q * K * E * (1 - ((q / r) * E))
}

# Add segment and label to the graphical solution
add_segment_and_label <- function(x, x_int, y_end, color, label) {
  x + 
    geom_segment(aes(x = x_int, y = 0, xend = x_int, yend = y_end),
                 linetype = "dashed", color = color, size = 1.5) +
    geom_label(aes(x = x_int, y = y_end, label = label))
}

# Global values ----
# Set the global parameters
default <- list()
default$r <- 0.2
default$K <- 1000
default$q <- 0.002
default$p <- 20
default$c <- 10
default$th <- 0
default$te <- 0

color <- list()
color$default_revenue <- "blue"
color$default_cost <- "red"
color$default_solution <- "grey"
color$revenue <- "magenta"
color$cost <- "green"
color$solution <- "yellow"

# UI ----
ui <- fluidPage(
  # Specify the css file
  theme = "master.css",
  
  # Initialize shinyJS()
  shinyjs::useShinyjs(),
  
  # The application layout
  fluidRow(
    class = "top-panel",
    column(
      12,
      h1("The Gordon-Schaefer Model")
    )
  ),
  fluidRow(
    class = "main-panel",
    sidebarLayout(
      sidebarPanel(
        class = "sidebar-panel",
        width = 4,
        # Set the inputs
        div(
          id = "parameters",
          sliderInput("r", "Intrinsic growth rate", min = 0.1, max = 0.3, step = 0.01, value = default$r),
          sliderInput("K", "Carrying capacity of the stock", min = 250, max = 1750, step = 50, value = default$K),
          sliderInput("q", "Catchability coefficient", min = 0.001, max = 0.003, step = 0.0001, value = default$q),
          sliderInput("p", "Price per unit harvest", min = 0, max = 30, step = 0.5, value = default$p),
          sliderInput("c", "Cost per unit effort", min = 0, max = 30, step = 0.5, value = default$c),
          numericInput("th", "Tax per unit harvest", value = default$th),
          numericInput("te", "Tax per unit effort", value = default$te),
          # Add row of checkboxes for OA, MEA, MSY, when checked render vertical line
          checkboxGroupInput("solutions", "Show solution graphically", c("MEY", "MSY", "OA"), inline = TRUE)
        ),
        actionButton("reset", "Reset values to default")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Description",
            div(
              h2("This is a description")
            )
          ),
          tabPanel(
            "Model",
            uiOutput("ui_model")
          )
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Reset values when the "reset" button is clicked ----
  observeEvent(
    {
      input[["reset"]]
    },
    {
      reset("parameters")
    }
  )
  
  # Non-reactive calculations ----
  effort <- 1:100
  default_revenue <- (default$p + default$th) * yield_effort(default$K, effort, default$q, default$r)
  default_cost <- default$c * effort
  
  revenue <- default_revenue
  cost <- default_cost
  revenue_changed <- cost_changed <- FALSE
  
  
  # Render effort space graphical solution ----
  output[["ui_graphical"]] <- renderPlot(
    {
      # Define the default plot
      gs_plot <- ggplot() +
        geom_line(aes(x = effort, y = default_revenue), color = color$default_revenue, size = 1.5) +
        geom_line(aes(x = effort, y = default_cost), color = color$default_cost, size = 1.5) +
        xlab("Effort") +
        scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
        ylab("Total revenue / Total cost") +
        scale_y_continuous(breaks = seq(0, 2500, 100), labels = seq(0, 2500, 100), limits = c(0, NA)) +
        theme(
          panel.border = element_blank(), 
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = .5, linetype = "solid", color = "grey"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.text = element_text(size = 11),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 11)
          )
      
      # Check whether we have changes
      if (input$r != default$r || input$K != default$K || input$q != default$q || input$p != default$p || input$th != default$th) {
        revenue_changed <- TRUE
        revenue <- (input$p + input$th) * yield_effort(input$K, effort, input$q, input$r)
        gs_plot <- gs_plot +
          geom_line(aes(x = effort, y = revenue), color = color$revenue, size = 1.5) 
      }
      
      if (input$c != default$c || input$te != default$te) {
        cost_changed <- TRUE
        cost <- (input$c + input$te) * effort
        gs_plot <- gs_plot +
          geom_line(aes(x = effort, y = cost), color = color$cost, size = 1.5)
      }
      
      # MEY, MSY, OA ----
      if ("MEY" %in% input[["solutions"]]) {
        profit_d <- default_revenue - default_cost
        mey_d <- which(max(profit_d) == profit_d)[1]
        gs_plot <- add_segment_and_label(gs_plot, mey_d, default_revenue[mey_d], color$default_solution, "MEY")
        
        # Check if parameters have changed ----
        if (revenue_changed || cost_changed) {
          profit <- revenue - cost
          mey <- which(max(profit) == profit)[1]
          gs_plot <- add_segment_and_label(gs_plot, mey, revenue[mey], color$default_solution, "MEY*")
        }
      }
      
      if ("MSY" %in% input[["solutions"]]) {
        msy <- which(max(default_revenue) == default_revenue)[1]
        gs_plot <- add_segment_and_label(gs_plot, msy, default_revenue[msy], color$default_solution, "MSY")
        
        # Check if parameters have changed ----
        if (revenue_changed || cost_changed) {
          msy_d <- which(max(revenue) == revenue)[1]
          gs_plot <- add_segment_and_label(gs_plot, msy_d, revenue[msy_d], color$default_solution, "MSY*")
        }
      }
      
      if ("OA" %in% input[["solutions"]]) {
        oa <- which(default_revenue == default_cost)[1]
        gs_plot <- add_segment_and_label(gs_plot, oa, default_revenue[oa], color$default_solution, "OA")
        
        # Check if parameters have changed ----
        if (revenue_changed || cost_changed) {
          oa_d <- which(revenue == cost)[1]
          # gs_plot <- add_segment_and_label(gs_plot, oa_d, revenue[oa_d], color$default_solution, "OA*")
          gs_plot <- add_segment_and_label(gs_plot, oa_d, revenue[oa_d], color$default_solution, "OA*")
        }
      }
      
      # Render plot ----
      gs_plot
    }
  )
  
  # Numerical results ----
  ui_numerical <- reactive(
    {
      observe(
        {
          output[["check_answer"]] <- renderPrint(
            {
              str(input[["solutions"]])
            }
          )
        }
      )
      return(
        tags$div(
          verbatimTextOutput("check_answer")
        )
      )
      
      
    }
  )
  
  # UI Graphical and Numerical  ----
  output[["ui_model"]] <- renderUI(
    {
      return(
        withTags(
          div(
            h1("Graphical solution in effort space"),
            plotOutput("ui_graphical"),
            h1("Graphical solution in stock space"),
            h1("Numerical solution"),
            ui_numerical()
          )
        )
      )
    }
  )
  
}

# Combine into application ----
shinyApp(ui = ui, server = server)
