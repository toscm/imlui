serve_imlui_app <- function(config_file = NULL,
                            config_dir = NULL,
                            host = "0.0.0.0",
                            launch.browser = TRUE,
                            port = PORT,
                            quiet = TRUE,
                            display.mode = "normal",
                            test.mode = FALSE) {
  app <- imlui_app(
    config_file = config_file,
    config_dir = config_dir,
    host = host,
    launch.browser = launch.browser,
    port = port,
    quiet = quiet,
    display.mode = display.mode,
    test.mode = test.mode
  )
  runApp(app)
}
