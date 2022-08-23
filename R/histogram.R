# # Origional ui code.
# ui <- fluidPage(
#   selectInput("var", "Variable", names(mtcars)),
#   numericInput("bins", "bins", 10, min = 1),
#   plotOutput("hist")
# )

# # Original server code
# server <- function(input, output, session) {
#   data <- reactive(mtcars[[input$var]])
#   output$hist <- renderPlot({
#     hist(data(), breaks = input$bins, main = input$var)
#   }, res = 96)
# }

histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}

# Module UI
# Accept id argument.
# Wrap variable names in NS(id, <blah>)
# Decided to return in a tagList, because we'll let the user decide layout.
histogramUI <- function(id) {
  tagList(
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}


histogramApp <- function(){
  ui <- fluidPage(histogramUI("hist1"))
  
  server <- function(input, output, session) {
    histogramServer("hist1")
  }
  
  shinyApp(ui, server)
}