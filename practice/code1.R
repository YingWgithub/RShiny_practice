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
