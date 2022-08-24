# Another important use of modules is to give complex UI elements a simpler user
# interface. Here I’m going to create a useful control that Shiny doesn’t
# provide by default: a small set of options displayed with radio buttons
# coupled with an “other” field. The inside of this module uses multiple input
# elements, but from the outside it works as a single combined object.

# I’m going to parametrise the UI side with label, choices, and selected which
# get passed directly to radioButtons(). I also create a textInput() containing
# a placeholder, that defaults to “Other”. To combine the text box and the radio
# button, I take advantage of the fact that choiceNames can be a list of HTML
# elements, including other input widgets. Figure 19.3 gives you a sense of what
# it’ll look like.

radioExtraUI <- function(id, label, choices, selected = NULL, placeholder = "Other") {
  other <- textInput(NS(id, "other"), label = NULL, placeholder = placeholder)
  
  names <- if(is.null(names(choices))) choices else names(choices)
  values <- unname(choices)
  
  radioButtons(
    NS(id, "primary"),
    label = label,
    choiceValues = c(names, "other"),
    choiceNames = c(as.list(values), list(other)),
    selected = selected
  )
}

# On the server, I want to automatically select the “other” radio button if you
# modify the placeholder value. You could also imagine using validation to
# ensure that some text is present if other is selected.

radioExtraServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$other, ignoreInit = TRUE, {
      updateRadioButtons(session, "primary", selected = "other")
    })
    
    reactive({
      if (input$primary == "other") {
        input$other
      } else {
        input$primary
      }
    })
  })
}

# Then I wrap up both pieces in an app function so that I can test it.
# Here I use … to pass down any number of arguments into my radioExtraUI().
radioExtraApp <- function(...) {
  ui <- fluidPage(
    radioExtraUI("extra", ...),
    textOutput("value")
  )
  
  server <- function(input, output, server) {
    extra <- radioExtraServer("extra")
    output$value <- renderText(paste0("Selected: ", extra()))
  }
  
  shinyApp(ui, server)
}

# You could continue to wrap up this module for still more specific purposes.
# For example, one variable that requires a little care is gender, because there
# are many different ways for people to express their gender.
genderUI <- function(id, label = "Gender") {
  radioExtraUI(id, 
               label = label,
               choices = c(
                 male = "Male",
                 female = "Female",
                 na = "Prefer not to say"
               ), 
               placeholder = "Self-described", 
               selected = "na"
  )
}

# For a deeper dive on this issue, and a discussion of why many commonly used
# ways of asking about gender can be hurtful to some people, I recommend reading
# “Designing forms for gender diversity and inclusion” by Sabrina Fonseca or
# Standard for Sex, Gender, Variations of Sex Characteristics and Sexual
# Orientation Variables by The Australian Bureau of Statistics.