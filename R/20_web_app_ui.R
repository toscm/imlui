web_app_ui <- function(data) {
  div(
    sidebarLayout(
      side_bar(),
      main_panel()
    ),
    includeHTML(system.file("assets/html/footer.html", package="imlui"))
  )
}

side_bar <- function() {
  sidebarPanel(
    width=3,
    pickerInput("MM", "Models", choices=list(), multiple=TRUE),
    pickerInput("DD", "Datasets", choices=list(), multiple=TRUE),
    tagList(
      selectInput(
        inputId="PASC",
        label="Plot Area Size",
        choices=c("Auto", "Fixed"),
        selected="Auto"
      ),
      sliderInput(
        inputId="MPAW",
        label="Plot Area Width",
        min=240, max=1920, step=60, value=240
      ),
      sliderInput(
        inputId="MPAH",
        label="Plot Area Height",
        min=240, max=1920, step=60, value=240
      )
    ),
    uiOutput(outputId="AI_TO"),
    verbatimTextOutput("auth_output")
  )
}

main_panel <- function() {
  mainPanel(
    width=9,
    actionButton(
      inputId="logout_button",
      label="Log out",
      icon=icon("sign-out-alt"),
      class="btn-danger pull-right",
      style="color: white; "
    ),
    tabsetPanel(
      id = "MP_CS",
      selected = "Model Analysis",
      tabPanel("Model Analysis", model_analysis_page()),
      tabPanel("Dataset Analysis", dataset_analysis_page()),
      shinyjs::hidden(tabPanel("Settings", uiOutput(outputId="S_T")))
    )
  )
}
