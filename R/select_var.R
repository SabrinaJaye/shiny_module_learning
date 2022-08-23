# Create a control that allows the user to select variables of specified type
# from a given reactive dataset. Because we want the dataset to be reactive, we
# can’t fill in the choices when we start the app. This makes the module UI very
# simple:
selectVarInput <- function(id) {
  selectInput(NS(id, "var"), "Variable", choices = NULL) 
}

# The server function will have two arguments:
#
# 1. The data to select variables from. I want this to be reactive so it can
#    work with the dataset module I created above.
#
# 2. A filter used to select which variables to list. This will be set by the
#    caller of the module, so doesn’t need to be reactive. To keep the module
#    server simple, I’ve extracted out the key idea into a helper function
find_vars <- function(data, filter) {
  stopifnot(is.data.frame(data))
  stopifnot(is.function(filter))
  names(data)[vapply(data, filter, logical(1))]
}

# Then the module server uses observeEvent() to update the inputSelect choices
# when the data changes, and returns a reactive that provides the values of the
# selected variable.
selectVarServer <- function(id, data, filter = is.numeric) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "var", choices = find_vars(data(), filter))
    })
    
    reactive(data()[[input$var]])
  })
}

# To make our app, we again capture the results of the module server and connect
# it to an output in our UI. I want to make sure all the reactive plumbing is
# correct, so I use the dataset module as a source of reactive data frames.
selectVarApp <- function(filter = is.numeric) {
  ui <- fluidPage(
    datasetInput("data", is.data.frame),
    selectVarInput("var"),
    verbatimTextOutput("out")
  )
  
  server <- function(input, output, session) {
    data <- datasetServer("data")
    var <- selectVarServer("var", data, filter = filter)
    output$out <- renderPrint(var())
  }
  
  shinyApp(ui, server)
}

# When designing a module server, you need to think about who is going to
# provide the value for each argument: is it the R programmer calling your
# module, or the person using the app? Another way to think about this is when
# can the value change: is it fixed and constant over the life-time of the app,
# or is it reactive, changing as the user interacts with the app? This is an
# important design decision that determines whether or not an argument should be
# a reactive or not.
#
# Once you’ve made this decision, I think it’s good practice to check that each
# input to your module is either reactive or constant. If you don’t, and the
# user supplies the wrong type, they’ll get a cryptic error message. You can
# make the life of module user much easier with a quick and dirty call to
# stopifnot().
#
# For example, selectVarServer() could check that data is reactive and filter
# is not with the following code:
selectVarServer <- function(id, data, filter = is.numeric) {
  stopifnot(is.reactive(data))
  stopifnot(!is.reactive(filter))
  
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "var", choices = find_vars(data(), filter))
    })
    
    reactive(data()[[input$var]])
  })
}