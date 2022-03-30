runserver <- function(port=8080) {
	logsn("Function runserver called from process ID:", Sys.getpid())
	logsn(paste0("Open http://localhost:", port, " to try the app."))
	runApp(
		appDir=shinyApp(
			ui=.shinyUI, # object like fluidPage or function(request)
			server=.shinyServer, # function(input, output, session)
			enableBookmarking="url"
		),
		host="0.0.0.0",
		port=port,
		launch.browser=FALSE,
		quiet=TRUE
	)
}
