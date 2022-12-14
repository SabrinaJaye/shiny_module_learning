# Next we’ll tackle a pair of case studies that dive into some subtleties of
# namespacing, where the UI is generated at different time by different people.
# These situations are complex because you need to remember the details of how
# namespacing works.

# We’ll start with a module that wraps up a wizard interface, a style of UI
# where you break a complex process down into a series of simple pages that the
# user works through one-by-one. I showed how to create a basic wizard in
# Section 10.2.2. Now we’ll automate the process, so that when creating a wizard
# you can focus on the content of each page, rather on how they are connected
# together to form a whole.

# To explain this module I’m going to start from the bottom and we’ll work our
# way up. The main part of the wizard UI are the buttons. Each page has two
# buttons: one to take them on to the next page, and one to return them to the
# previous. We’ll start by creating helpers to build these buttons:
nextPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i + 1)), "next")
}

prevPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i - 1)), "prev")
}

# The only real complexity here is the id: since each input element needs to
# have a unique id, the id for each button needs to include both the current and
# the destination page.

# Next I write a function to generate a page of the wizard.
# This includes a “title” (not shown, but used to identify the page for
# switching), the contents of the page (supplied by the user), and the two
# buttons.

wrapPage <- function(title, page, button_left = NULL, button_right = NULL) {
  tabPanel(
    title = title,
    fluidRow(
      column(12, page)
    ),
    fluidRow(
      column(6, button_left),
      column(6, button_right)
    )
  )
}

# Then we can put it all together to generate the whole wizard. We loop over the
# list of pages provided by the user, create the buttons, then wrap up the user
# supplied page into a tabPanel, then combine all the panels into a tabsetPanel.
# Note that there are two special cases for buttons:
# 
# 1. The first page doesn’t have a previous button. Here I use a trick that if
#    returns NULL if the condition is FALSE and there is no else block.
# 
# 2. The last page uses an input control supplied by the user. I think this is
#    the simplest way to allow the user to control what happens when the wizard
#    is done.
wizardUI <- function(id, pages, doneButton = NULL) {
  stopifnot(is.list(pages))
  n <- length(pages)
  
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    # First page only has next; last page only has prev + done.
    lhs <- if (i > 1) prevPage(id, i)
    rhs <- if (i < n) nextPage(id, i) else doneButton
    wrapped[[i]] <- wrapPage(paste0("page_", i), pages[[i]], lhs, rhs)
  }
  
  wrapped$id <- NS(id, "wizard")
  wrapped$type <- "hidden"
  do.call("tabsetPanel", wrapped)
}

# Now that we’ve completed the module UI, we need to turn our attention to the
# module server. The essence of the server is straightforward: we just need to
# make buttons work, so that you can travel from page-to-page in either
# direction. To do that we need to setup a observeEvent() for each button that
# calls updateTabsetPanel(). This would be relatively simple if we knew exactly
# how many pages there were. But we don’t, because the user of the module gets
# to control that.

# So instead, we need to do a little functional programming to set up the
# (n - 1) * 2 observers (two observers for each page except for the first and
# last, which only need one). The server function below starts by extracting out
# the basic code we need for one button in the changePage() function. It uses
# input[[]], as in Section 10.3.2, so we can refer to control dynamically.
# Then we use lapply() to loop over all the previous buttons (needed for every
# page except the first), and all the next buttons (needed for every page except
# the last).

wizardServer <- function(id, n) {
  moduleServer(id, function(input, output, session) {
    changePage <- function(from, to) {
      observeEvent(input[[paste0("go_", from, "_", to)]], {
        updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })
    }
    
    ids <- seq_len(n)
    # don't use a for loop - map/lapply create their own environment for every i.
    lapply(ids[-1], function(i) changePage(i, i - 1))
    lapply(ids[-n], function(i) changePage(i, i + 1))
  })
}

wizardApp <- function(...) {
  pages <- list(...)
  
  ui <- fluidPage(
    wizardUI("whiz", pages)
  )
  
  server <- function(input, output, session) {
    wizardServer("whiz", length(pages))
  }
  
  shinyApp(ui, server)
}

realisticWizardApp <- function() {
  page1 <- tagList(
    textInput("name", "What's your name?")
  )
  
  page2 <- tagList(
    numericInput("age", "How old are you?", 20)
  )
  
  page3 <- tagList(
    "Is this data correct?",
    verbatimTextOutput("info")
  )
  
  ui <- fluidPage(
    wizardUI(
      id = "demographics",
      pages = list(page1, page2, page3),
      doneButton = actionButton("done", "Submit")
    )
  )
  
  server <- function(input, output, session) {
    wizardServer("demographics", 3)
    
    observeEvent(input$done, showModal(
      modalDialog("Thank you!", footer = NULL)
    ))
    
    output$info <- renderText(paste0(
      "Age: ", input$age, "\n",
      "Name: ", input$name, "\n"
    ))
  }
  
  shinyApp(ui, server)
  
}