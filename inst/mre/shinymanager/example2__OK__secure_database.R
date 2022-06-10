# Example based on:
# https://datastorm-open.github.io/shinymanager/
# user: shiny / password: shiny
# user: shinymanager / password: shinymanager (Admin)
library(toscutil)
library(toscmask)
library(keyring)
library(shiny)
library(shinymanager)

# Init the database
sqlite_dir <- home() + "/.config/tosc-shinymanager-test"
sqlite_db <- sqlite_dir + "/database.sqlite"
if (!dir.exists(sqlite_dir)) {
  dir.create(sqlite_dir, recursive=T)
}
create_db(
  credentials_data=data.frame(
    user = c("shiny", "shinymanager", "a"), # mandatory
    password = c("shiny", "shinymanager", "a"), # mandatory
    admin = c(FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  ),
  sqlite_path=sqlite_db, 
  passphrase="asdf"
)

# Wrap your UI with secure_app
ui <- secure_app(
  fluidPage(
    tags$h2("My secure application"),
    verbatimTextOutput("auth_output")
  ),
  enable_admin=TRUE
)

server <- function(input, output, session) {
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials=check_credentials(
      db=sqlite_db,
      passphrase="asdf"
    )
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
}

shinyApp(ui, server)

