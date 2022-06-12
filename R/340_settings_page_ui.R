settings_page <- function() {
  tabsetPanel(
    id = "S_T_CS",
    tabPanel("Paper", div(fluidRow(column(12, DT::DTOutput("S_PA_TBL"))), fluidRow(column(12, DT::DTOutput("S_PA_TBL2"))))),
    tabPanel("Models", div(fluidRow(column(12, DT::DTOutput("S_MO_TBL"))), fluidRow(column(12, DT::DTOutput("S_MO_TBL2"))))),
    tabPanel("Datasets", div(fluidRow(column(12, DT::DTOutput("S_DS_TBL"))), fluidRow(column(12, DT::DTOutput("S_DS_TBL2"))))),
    tabPanel("Samples", div(fluidRow(column(12, DT::DTOutput("S_SA_TBL"))), fluidRow(column(12, DT::DTOutput("S_SA_TBL2"))))),
    tabPanel("Datatypes", div(fluidRow(column(12, DT::DTOutput("S_DT_TBL"))), fluidRow(column(12, DT::DTOutput("S_DT_TBL2"))))),
    tabPanel("Methods", div(fluidRow(column(12, DT::DTOutput("S_ME_TBL"))), fluidRow(column(12, DT::DTOutput("S_ME_TBL2"))))),
    tabPanel("Platforms", div(fluidRow(column(12, DT::DTOutput("S_PF_TBL"))), fluidRow(column(12, DT::DTOutput("S_PF_TBL2"))))),
    tabPanel("Settings", div(fluidRow(column(12, DT::DTOutput("S_SE_TBL"))), fluidRow(column(12, DT::DTOutput("S_SE_TBL2")))))
  )
}

settings_tab <- function(title, id_upper_table, id_lower_table) {
  tabPanel(title = title, div(
    fluidRow(column(width = 12, DT::DTOutput("S_PA_TBL"))),
    fluidRow(column(12, DT::DTOutput("S_PA_TBL2")))
  ))
}


