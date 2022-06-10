imluiApp <- function(port = PORT, config_file = NULL, config_dir = NULL) {
  logsne("ImluiApp started from process ID:", Sys.getpid(), "...")
  shiny::addResourcePath(
		prefix = "imlui/assets",
		system.file("assets", package = "imlui")
	)
  db <- DB$new()$connect()
  shinyApp(
    ui = ui,
    server = server,
    onStart = function() {
      logsne("ShinyApp started from process ID:", Sys.getpid())
      shiny::onStop(logsne("ShinyApp stopped"))
    },
    options = list(
      port = port,
      launch.browser = FALSE,
      host = "0.0.0.0",
      quiet = TRUE,
      display.mode = "normal", # c("auto", "normal", "showcase")
      test.mode = FALSE # used for recording or running automated tests
    ),
    uiPattern = ".*", # regex
    enableBookmarking = "url" # url, server or disable
  )
}

runImluiApp <- function(port = APP_DEFAULTS$port) {
  logsne("Function runserver called from process ID:", Sys.getpid())
  runApp(
    appDir = imluiApp(port = port),
    host = "0.0.0.0",
    port = port,
    launch.browser = FALSE,
    quiet = TRUE
  )
}
