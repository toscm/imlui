imlui_server <- function(input, output, session) {
  ses <- init_data(input, output, session)
  print_startup_message()
  configure_bookmarking()
  init_observers(ses)
  # web_app_ui_server(data)
  # login_page_server(data)
  # onStop(act_store_appstate_in_db(), session = getDefaultReactiveDomain())
  # onSessionEnded(db$disconnect(), session = getDefaultReactiveDomain())
  # output$webapp <- webappUI(data)
}
