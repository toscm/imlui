server <- function(input, output, session) {
	print_startup_message()
	configure_bookmarking()
	data <- init_server_data(input, output, session)
	# setup_observers(data)
	# web_app_ui_server(data)
	# login_page_server(data)
	# onStop(store_appstate_in_db(), session = getDefaultReactiveDomain())
	# onSessionEnded(db$disconnect(), session = getDefaultReactiveDomain())
	# output$webapp <- webappUI(data)
}
