library(shiny)

pdat <- new.env(parent=emptyenv())

## Only run examples in interactive R sessions
ui <- fluidPage(
  actionButton(inputId="button", label="Generate random number", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  verbatimTextOutput("text")
)
server <- function(input, output) {
  observeEvent(input$button, {
    pdat$numbers <- c(rnorm(1), pdat$numbers)
    output$text <- renderText(pdat$numbers)
  })
}

devmode()

shinyApp(ui, server)
