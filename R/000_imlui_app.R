imlui_app <- function(config_file = NULL,
                      config_dir = NULL,
                      host = "0.0.0.0",
                      launch.browser = FALSE,
                      port = PORT,
                      quiet = TRUE,
                      display.mode = "normal", # c("auto", "normal", "showcase")
                      test.mode = FALSE) { # used for automated tests
  infomsg("ImluiApp started from process ID:", Sys.getpid(), "...")
  shinyApp(
    ui = imlui_ui,
    server = imlui_server,
    uiPattern = ".*", # regex
    enableBookmarking = "url", # "url", "server" or "disable"
    onStart = function() {
      pkg$imlui_config <- read_imlui_config_file(create_if_missing = TRUE)
      pkg$db <- DB$new() # ok: tested dynamically
      pkg$sid <- 1 # for each session: use as session number and increase by 1
      shiny::addResourcePath(
        prefix = "imlui/assets",
        directoryPath = system.file("assets", package = "imlui")
      )
      activate_func_tracing()
      infomsg("ShinyApp started from process ID:", Sys.getpid())
      infomsg("Open http://localhost:" + port + " to preview the app")
      shiny::onStop(function() infomsg("ShinyApp stopped"))
    },
    options = list(
      port = port,
      launch.browser = launch.browser,
      host = host,
      quiet = quiet,
      display.mode = "normal",
      test.mode = FALSE
    )
  )
}
