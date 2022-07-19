dyn_serve_app_in_dev_mode <- function() {
  # See [README.md](README.md#shiny-bugfixworkaround-autoreload-for-packages)
  # for a detailed documentation of this function.
  global_env <- .GlobalEnv
  assign("IMLUI_DEV_MODE", TRUE, envir = global_env)
  devtools::load_all()
  repo_root <- dirname(system.file(package = "imlui"))
  options("shiny.autoreload" = TRUE)
  shiny::runApp(repo_root)
}

