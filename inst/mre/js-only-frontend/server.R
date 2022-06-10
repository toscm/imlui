library(shiny)

if (!exists("pdat")) {
  pdat <- new.env(parent=emptyenv())
}

options(
  shiny.trace=TRUE,
  shiny.autoreload=TRUE,
  shiny.reactlog=TRUE,
  shiny.minified=FALSE,
  shiny.fullstacktrace=TRUE,
  shiny.stacktraceoffset=TRUE
)

server <- function(input, output) {
  observeEvent(input$button, {
    pdat$numbers <- c(rnorm(1), pdat$numbers)
    output$text <- renderText(pdat$numbers)
  })
}

# shinyAppDir(".")
server
