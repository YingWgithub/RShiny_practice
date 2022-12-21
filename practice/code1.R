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

#  example 3
ui <- fluidPage(
  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and, (x * y) + 10 is", textOutput("product_plus10") 
)

server <- function(input, output, session) {
  product <- reactive({
    input$x * input$y # no need to add "product <-"
  })
  output$product <- renderText({
    product()
  })
  output$product_plus5 <- renderText({
    product() + 5
  })
  output$product_plus10 <- renderText({
    product() + 10
  })
}
shinyApp(ui,server)

# example 4: to write an shiny app head all the package from "nlme" package 
install.packages("nlme")
library(nlme)
# ls("package:nlme"): all the items in this package, includes functions and datasets
# names(data(package = "nlme")): to check the information name 
datasets <- data.frame(data(package = "nlme")$results)$Item # all the datasets names, need to create before the app
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = datasets),
  verbatimTextOutput("dimension"),
  tableOutput("head"),
  verbatimTextOutput("structure")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:nlme")
  })
  
  # Display the dimensions of the selected dataset
  output$dimension <- renderPrint({
    dim(dataset())
  })
  
  # Display the first few rows of the selected dataset
  output$head <- renderTable({
    head(dataset())
  })
  
  # Display the structure of the selected dataset
  output$structure <- renderPrint({
    str(dataset())
  })
}

shinyApp(ui, server)


