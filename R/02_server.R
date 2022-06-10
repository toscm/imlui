server <- function(input, output, session) {
	server__print_startup_message()
	server__configure_bookmarking()
	data <- server__init_data(input, output, session)
	server__setup_observers(data)
	server__web_app_ui_server(data)
	server__login_page_server(data)
	onStop(server__store_appstate_in_db(), session = getDefaultReactiveDomain())
	onSessionEnded(db$disconnect(), session = getDefaultReactiveDomain())
	output$webapp <- webappUI(data)
}
