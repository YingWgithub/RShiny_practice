##### 12-15-2022 code1 ####

# install shiny 
install.packages("shiny")
library(shiny)

# example 1
ui <- fluidPage(
  "Hello, world!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)

# example 2
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table"),
  verbatimTextOutput("structure") # adding the information for "str", same format as "summary"
)

server <- function(input, output, session) {
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })
  
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    head(dataset) 
  })
  
  output$structure <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    str(dataset)
  })
}
shinyApp(ui, server)

# to improve the efficiency and reduce unwanted updates
server2 <- function(input, output, session) {
  # Create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(dataset())
  })
  
  output$table <- renderTable({
    head(dataset())
  })
  
  output$structure <- renderPrint({
    str(dataset())
  })
}
shinyApp(ui, server2)
