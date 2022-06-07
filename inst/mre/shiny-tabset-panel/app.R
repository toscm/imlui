# Show a tabset that includes a plot, summary, and
# table view of the generated distribution
library("shiny")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("controller", "Controller", 1:3, 1)
    ),
    mainPanel(
      tabsetPanel(
        id = "hidden_tabs",
        type = "hidden", # Hide the tab values. Switch tabs by using updateTabsetPanel().
        tabPanelBody("panel1", "Panel 1 content"),
        tabPanelBody("panel2", "Panel 2 content"),
        tabPanelBody("panel3", "Panel 3 content"),
        header=tagList(
            actionButton("asdf", "asdf"),
            actionButton("qwer", "qwer"),
            actionButton("yxcv", "yxcv")
        ),
        footer=tagList(
            "asdf",
            "qwer",
            "yxcv"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
  })
}

runApp(shinyApp(ui, server))
