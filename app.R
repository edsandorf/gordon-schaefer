# Load packages ----
pkgs <- c("shiny", "shinyjs", "shinyWidgets", "ggplot2", "DT", "dplyr", "tidyr")
invisible(lapply(pkgs, require, character.only = TRUE))

# Global functions ----
# The yield-effort curve
yield_effort <- function(K, E, q, r) {
  q * K * E * (1 - ((q / r) * E))
}

# The solutions to the model 
mey_x <- function(K, q, cc, p) {(1/2) * (K + (cc / (p * q)))}
mey_e <- function(K, q, cc, p, r) {(1/2) * (r / q) * (1 - (cc / (p * q * K)))}
mey_h <- function(K, q, cc, p, r) {(r / 4) * (K - (cc^2) / ((p^2) * (q^2) * K))}
msy_x <- function(K) {K / 2}
msy_e <- function(q, r) {r / (2*q)}
msy_h <- function(r, K) {(r * K) / 4}
oa_x <- function(q, cc, p) {cc / (p * q)}
oa_e <- function(K, q, cc, p, r) {(r / q) * (1 - (cc / (p * q * K)))}
oa_h <- function(K, q, cc, p, r) {((r * cc) / (p * q)) * (1 - (cc / (p * q * K)))}

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
color$default_revenue <- "#147bba"
color$default_cost <- "#cf2950"
color$default_solution <- "grey"
color$revenue <- "#147bba"
color$cost <- "#cf2950"
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
        # Action button to reset the parameters
        actionButton("reset", "Reset values to default"),
        h1("Parameters"),
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
        )
      ),
      mainPanel(
        class = "content-panel",
        tabsetPanel(
          tabPanel(
            "Description",
            div(
              id = "description",
              h1("Introduction"),
              p("The Gordon-Schaefer model is essential to the bio-economic modeling of fisheries. The model uses a simple logistic growth function to describe the fish stock, a single exogenous price and a linear cost function. This simple teaching tool allows you to manipulate the parameters of the model to see how results change as a result. "),
              h3("References:"),
              p("Schaefer, M. B., 1957, Some Considerations of Poulation Dynamics and Economics in Relation to the Management of Marine Fisheries, Journal of the Fisheries Research Board of Canada, 14, 669-681"),
              p("Gordon, H.S., 1954, The Economic Theory of a Common Property Resource: The Fishery,Journal of Political Economy, 62, 124-142")
            )
          ),
          tabPanel(
            "Model",
            uiOutput("ui_model")
          )
        )
      )
    )
  ),
  fluidRow(
    class = "bottom-panel",
    column(
      12,
      p("Created by: Erlend Dancke Sandorf")
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
  effort <- 0:100
  default_revenue <- (default$p + default$th) * yield_effort(default$K, effort, default$q, default$r)
  default_cost <- default$c * effort
  
  revenue <- default_revenue
  cost <- default_cost
  revenue_changed <- cost_changed <- FALSE
  
  # db <- tibble(
  #   TR = default_revenue,
  #   TC = default_cost
  # ) %>%
  #   gather("type", "value")
  
  # Render effort space graphical solution ----
  output[["ui_graphical"]] <- renderPlot(
    {
      # Define the default plot
      gs_plot <- ggplot() +
        geom_line(aes(x = effort, y = default_revenue), color = color$default_revenue, size = 1.5) +
        geom_line(aes(x = effort, y = default_cost), color = color$default_cost, size = 1.5) +
        xlab("Effort") +
        scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10), expand = expand_scale(mult = c(0, 0.01))) +
        ylab("Total revenue / Total cost") +
        scale_y_continuous(breaks = seq(0, 3000, 100), labels = seq(0, 3000, 100), limits = c(0, NA), expand = expand_scale(mult = c(0, 0.05))) +
        theme(
          legend.position = "bottom",
          panel.border = element_blank(), 
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = .5, linetype = "solid", color = "grey"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.text = element_text(size = 11),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 11)
          )
      
      # Check whether we have changes
      if (input$r != default$r || input$K != default$K || input$q != default$q || input$p != default$p || input$th != default$th) {
        revenue_changed <- TRUE
        revenue <- (input$p - input$th) * yield_effort(input$K, effort, input$q, input$r)
        gs_plot <- gs_plot +
          geom_line(aes(x = effort, y = revenue), color = color$revenue, size = 1.5, linetype = "dashed") 
      }
      
      if (input$c != default$c || input$te != default$te) {
        cost_changed <- TRUE
        cost <- (input$c + input$te) * effort
        gs_plot <- gs_plot +
          geom_line(aes(x = effort, y = cost), color = color$cost, size = 1.5, linetype = "dashed")
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
        msy_d <- which(max(default_revenue) == default_revenue)[1]
        gs_plot <- add_segment_and_label(gs_plot, msy_d, default_revenue[msy_d], color$default_solution, "MSY")
        
        # Check if parameters have changed ----
        if (revenue_changed || cost_changed) {
          msy <- which(max(revenue) == revenue)[1]
          gs_plot <- add_segment_and_label(gs_plot, msy, revenue[msy], color$default_solution, "MSY*")
        }
      }
      
      if ("OA" %in% input[["solutions"]]) {
        oa_d <- floor(oa_e(default$K, default$q, default$c, default$p, default$r))
        gs_plot <- add_segment_and_label(gs_plot, oa_d, default_revenue[oa_d], color$default_solution, "OA")
        
        # Check if parameters have changed ----
        if (revenue_changed || cost_changed) {
          # oa <- which(revenue == cost)[2]
          oa <- floor(oa_e(input$K, input$q, (input$c + input$te), (input$p - input$th), input$r))
          gs_plot <- add_segment_and_label(gs_plot, oa, revenue[oa], color$default_solution, "OA*")
        }
      }
      
      # Render plot ----
      gs_plot
    }
  )
  
  # Numerical results ----
  output[["ui_numerical"]] <- DT::renderDataTable(
    {
      num_output <- matrix(0, nrow = 6, ncol = 6)
      rownames(num_output) <- c("X", "E", "H", "TR", "TC", "Profit")
      colnames(num_output) <- c("MEY", "MEY*", "MSY", "MSY*", "OA", "OA*")

      # MEY
      num_output[1, 1] <- mey_x(default$K, default$q, default$c, default$p)
      num_output[1, 2] <- mey_x(input$K, input$q, (input$c + input$te), (input$p - input$th))
      num_output[2, 1] <- mey_e(default$K, default$q, default$c, default$p, default$r)
      num_output[2, 2] <- mey_e(input$K, input$q, (input$c + input$te), (input$p - input$th), input$r)
      num_output[3, 1] <- mey_h(default$K, default$q, default$c, default$p, default$r)
      num_output[3, 2] <- mey_h(input$K, input$q, (input$c + input$te), (input$p - input$th), input$r)
      
      # MSY
      num_output[1, 3] <- msy_x(default$K)
      num_output[1, 4] <- msy_x(input$K)
      num_output[2, 3] <- msy_e(default$q, default$r)
      num_output[2, 4] <- msy_e(input$q, input$r)
      num_output[3, 3] <- msy_h(default$K, default$r)
      num_output[3, 4] <- msy_h(input$K, input$r)
      
      # OA
      num_output[1, 5] <- oa_x(default$q, default$c, default$p)
      num_output[1, 6] <- oa_x(input$q, (input$c + input$te), (input$p - input$th))
      num_output[2, 5] <- oa_e(default$K, default$q, default$c, default$p, default$r)
      num_output[2, 6] <- oa_e(input$K, input$q, (input$c + input$te), (input$p - input$th), input$r)
      num_output[3, 5] <- oa_h(default$K, default$q, default$c, default$p, default$r)
      num_output[3, 6] <- oa_h(input$K, input$q, (input$c + input$te), (input$p - input$th), input$r)
      
      # Profits
      num_output[4, c(1, 3, 5)] <- default$p * num_output[3, c(1, 3, 5)]
      num_output[4, c(2, 4, 6)] <- input$p * num_output[3, c(2, 4, 6)]
      num_output[5, c(1, 3, 5)] <- default$c * num_output[2, c(1, 3, 5)]
      num_output[5, c(2, 4, 6)] <- input$c * num_output[2, c(2, 4, 6)]
      num_output[6, ] <- num_output[4, ] - num_output[5, ]
      
      # Return the table
      round(num_output, digits = 2)
    },
    escape = FALSE, server = FALSE, selection = "none", class = c("nowrap"),
    callback = JS(
      paste0(
        "var last_col = null;
                table.on('mouseenter', 'td', function() {
                  var td = $(this);
                  var col_index = table.cell(this).index().columnVisible;
                  if (col_index !== last_col) {
                    $(table.cells().nodes()).removeClass('highlight');
                    $(table.column(col_index).nodes()).addClass('highlight');
                  }
                });
            
                table.on('mouseleave', function() {
                  $(table.cells().nodes()).removeClass('highlight');
                });"
      )
    ),
    options = list(
      dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
      columnDefs = list(
        list(
          className = "dt-center",
          targets = seq_len(6)
        )
      )
    )
  )

  # UI Graphical and Numerical  ----
  output[["ui_model"]] <- renderUI(
    {
      return(
        withTags(
          div(
            h1("Graphical solution in effort space"),
            plotOutput("ui_graphical"),
            # h1("Graphical solution in stock space"),
            h1("Numerical solution"),
            DT::dataTableOutput("ui_numerical")
          )
        )
      )
    }
  )
  
}

# Combine into application ----
shinyApp(ui = ui, server = server)
