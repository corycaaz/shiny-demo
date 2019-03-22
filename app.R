# Dependencies
library(shiny)
library(tidyverse)

# Import
employ <- read_csv("employees-wage.csv")

# Compose the user interface
ui <- fluidPage(
  titlePanel("City of Chicago Wage Employees"),
  sidebarLayout(
    sidebarPanel(sliderInput(inputId = "wage",
                             label = "Wage range",
                             min = 0,
                             max = max(employ$wage, na.rm = TRUE),
                             value = c(0, max(employ$wage, na.rm = TRUE)),
                             pre = "$"),
                 radioButtons(inputId = "full_time",
                              label = "Full or part-time",
                              choices = c("Full-Time", "Part-Time")),
                 selectInput(inputId = "department",
                             label = "Department",
                             choices = sort(unique(employ$department)),
                             multiple = TRUE)
    ),
    mainPanel(plotOutput("hourlyPlot"),
              tableOutput("employTable")
    )
  )
)

# Build the server logic
server <- function(input, output) {
  
  # Inputs
  
  # Outputs
  output$hourlyPlot <- renderPlot({
    employ %>%
      filter(full_time == input$full_time,
             wage >= input$wage[[1]],
             wage <= input$wage[[2]]) %>%
      ggplot(aes(wage)) +
      geom_histogram()
  })
  
  output$employTable <- renderTable({
    employ %>%
      filter(full_time == input$full_time,
             wage >= input$wage[[1]],
             wage <= input$wage[[2]]) %>%
      count(department)
  })
}

# Set the app arguments
shinyApp(ui = ui, server = server)