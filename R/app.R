runserver <- function(port=8080) {
	logsne("Function runserver called from process ID:", Sys.getpid())
	runApp(
		appDir=imluiApp(port=port),
		host="0.0.0.0",
		port=port,
		launch.browser=FALSE,
		quiet=TRUE
	)
}

imluiApp <- function(port=8080, config_file=NULL, config_dir=NULL) {
	logsne("Starting imluiApp from process ID:", Sys.getpid(), "...")
	shinyApp(
		ui=ui, # object like fluidPage or function(request)
		server=server, # function(input, output, session)
		onStart=function() { # function()
			.onStart(port=port, config_file=config_file, config_dir=config_dir)
			shiny::onStop(.onStop)
		},
		options=list( # list(port, launch.browser, host, quiet, display.mode, test.mode)
			port=port,
			launch.browser=FALSE,
			host="0.0.0.0",
			quiet=TRUE,
			display.mode="normal", # c("auto", "normal", "showcase")
			test.mode=FALSE # used for recording or running automated tests
		),
		uiPattern="/", # <regex>
		enableBookmarking="url" # url, server or disable
	)
}

.onStart <- function(port, config_file, config_dir) {
	logsne("Starting shinyApp from process ID:", Sys.getpid(), "...")
	shiny::addResourcePath(prefix="imlui/assets", system.file("assets", package="imlui"))
	util__read_imlui_config() # sets options("imlui.config.dbms.{type|hostname|port|database|username|password}")
	imlui_db <- connect_imlui_db(init_if_empty=TRUE) # call here before server starts to initialize if required
	DBI::dbDisconnect(imlui_db)
	log0ne("ShinyApp started successfully.")
	log0ne("Open http://localhost:", port, " to try the app.")
}

.onStop <- function() {
	logsne("shinyApp stopped")
}