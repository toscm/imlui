settingsTabUI <- function(id) {
    ns <- NS(id)
    tabPanel(
        title="Settings",
        tabsetPanel(
            profileTabUI(id=ns("profile")),
            modelsTabUI(id=ns("models")),
            datasetsTabUI(id=ns("datasets")),
            usersTabUI(id=ns("users")),
            groupsTabUI(id=ns("groups"))
        )
    )
}

settingsTabServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        profileTabServer(id=ns("profile"))
        modelsTabServer(id=ns("models"))
        datasetsTabServer(id=ns("datasets"))
        usersTabServer(id=ns("users"))
        groupsTabServer(id=ns("groups"))
    })
}

profileTabUI <- function(id) {
    ns <- NS(id)
    tabPanel(
        title="Profile",
        div(
            fluidRow(column(12, textOutput(outputId=ns("table1")))), # DT::DTOutput(ns("table1"))
            fluidRow(column(12, textOutput(outputId=ns("table2")))) # DT::DTOutput(ns("table2"))
        )
    )
}

profileTabServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        output[[ns("table1")]] <- renderText("Table 1")
        output[[ns("table1")]] <- renderText("Table 2")
    })
}

modelsTabUI <- function(namespae="models_tab") {
    tabPanel(title="Models")
}

modelsTabServer <- function(id) {
    NULL
}

datasetsTabUI <- function(namespae="datasets_tab") {
    tabPanel(title="Datasets")
}

datasetsTabServer <- function(id) {
    NULL
}

usersTabUI <- function(namespae="users_tab") {
    tabPanel(title="Users")
}

usersTabServer <- function(id) {
    NULL
}

groupsTabUI <- function(namespae="groups_tab") {
    tabPanel(title="Groups")
}

groupsTabServer <- function(id) {
    NULL
}
