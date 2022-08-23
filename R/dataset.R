# We’ll start with the module UI. Here I use a single additional argument so
# that you can limit the options to built-in datasets that are either data
# frames (filter = is.data.frame) or matrices (filter = is.matrix). I use this
# argument to optionally filter the objects found in the datasets package, then
# create a selectInput().
datasetInput <- function(id, filter = NULL) {
  names <- ls("package:datasets")
  if (!is.null(filter)) {
    data <- lapply(names, get, "package:datasets")
    names <- names[vapply(data, filter, logical(1))]
  }
  
  selectInput(NS(id, "dataset"), "Pick a dataset", choices = names)
}

# The module server is also simple: we just use get() to retrieve the dataset
# with its name. There’s one new idea here: like a function and unlike a regular
# server(), this module server returns a value. Here we take advantage of the
# usual rule that last expression processed in the function becomes the return
# value57. This value should always be a reactive.
datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(get(input$dataset, "package:datasets"))
  })
}

datasetApp <- function(filter = NULL) {
  ui <- fluidPage(
    datasetInput("dataset", filter = filter),
    tableOutput("data")
  )
  
  server <- function(input, output, session) {
    data <- datasetServer("dataset")
    output$data <- renderTable(head(data()))
  }
  
  shinyApp(ui, server)
}