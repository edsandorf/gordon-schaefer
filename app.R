# Load packages ----
pkgs <- c("shiny", "shinyWidgets")
invisible(lapply(pkgs, require, character.only = TRUE))

# Global functions ----
# The yield-effort ¨curve
yield_effort <- function(K, E, q, r) {
  q * K * E * (1 - ((q / r) * E))
}


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
        width = 1,
        # Set a dropdown menu for the inputs
        shinyWidgets::dropdownButton(
          tags$h3("Parameter values"),
          # Set the inputs
          numericInput(inputId = "r", label = "Intrinsic growth rate", value = 0.2),
          numericInput(inputId = "K", label = "Carrying capacity of the stock", value = 1000),
          numericInput(inputId = "q", label = "Catchability coefficient", value = 0.002),
          numericInput(inputId = "c", label = "Cost per unit effort", value = 10),
          numericInput(inputId = "p", label = "Price per unit harvest", value = 20),
          numericInput(inputId = "th", label = "Tax per unit harvest", value = 0),
          numericInput(inputId = "te", label = "Tax per unit effort", value = 0),
          
          # Define the reset button
          actionButton(inputId = "reset", label = "Reset"),
          # Define the options of the dropdown button
          circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
          tooltip = tooltipOptions(title = "Click to see and change parameter values")
        ),
        # Add row of checkboxes for OA, MEA, MSY, when checked render vertical line
        
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Description",
            uiOutput("description")
          ),
          tabPanel(
            "Graphical solution",
            plotOutput("graphical")
          ),
          tabPanel(
            "Numerical solution",
            uiOutput("numerical")
          )
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # 
  # total_revenue <- input$p * yield_effort(input$K, effort, input$q, input$r)
  
  
  
  # Get the input values
  # input$r
  # input$K
  # input$q
  # input$c
  # input$p
  # input$th
  # input$te
  
  # Description ----
  description <- reactive(
    {
      
    }
  )
  
  output[["description"]] <- renderUI(
    {
      description()
    }
  )
  
  # Graphical output ----
  
  
  # Numerical output ----
  
  
}

# Combine into application ----
shinyApp(ui = ui, server = server)
