multiOutApp <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        datasetInput("data", is.data.frame),
        selectVarInput2("var"),
      ),
      mainPanel(
        histogramOutput2("hist")    
      )
    )
  )
  
  # The main challenge with this sort of code is remembering when you use the
  # reactive (e.g. x$value) vs. when you use its value (e.g. x$value()). Just
  # remember that when passing an argument to a module, you want the module to
  # react to the value changing which means that you have to pass the reactive,
  # not it’s current value.
  server <- function(input, output, session) {
    data <- datasetServer("data")
    x <- selectVarServer2("var", data)
    histogramServer2("hist", x$value, x$name)
  }
  
  shinyApp(ui, server)
} 