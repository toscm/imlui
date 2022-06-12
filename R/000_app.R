imluiApp <- function(config_file = NULL,
                     config_dir = NULL,
                     host = "0.0.0.0",
                     port = PORT,
                     quiet = TRUE,
                     display.mode = "normal", # c("auto", "normal", "showcase")
                     test.mode = FALSE # used for automated tests
) {
  logsne("ImluiApp started from process ID:", Sys.getpid(), "...")
  shiny::addResourcePath(
    prefix = "imlui/assets",
    system.file("assets", package = "imlui")
  )
  shinyApp(
    ui = ui,
    server = server,
    onStart = function() {
      logsne("ShinyApp started from process ID:", Sys.getpid())
      log0ne("Open http://localhost:", port, " to preview the app")
      shiny::onStop(function() logsne("ShinyApp stopped"))
    },
    options = list(
      port = port,
      launch.browser = FALSE,
      host = host,
      quiet = quiet,
      display.mode = "normal",
      test.mode = FALSE
    ),
    uiPattern = ".*", # regex
    enableBookmarking = "url" # url, server or disable
  )
}

runImluiApp <- function(...) {
  runApp(imluiApp(...))
}
