dyn_serve_app_in_dev_mode <- function() {
  # Serves current directory as shiny app with working autoreload. See
  # [README.md](README.md#shiny-bugfixworkaround-autoreload-for-packages) for a
  # detailed documentation of this function.
  shiny_env <- environment(shiny:::cachedFuncWithFile)
  unlockBinding("cachedFuncWithFile", shiny_env)
  body(shiny_env$cachedFuncWithFile) <- body(patched_cached_func_with_file)
  lockBinding("cachedFuncWithFile", shiny_env)
  global_env <- .GlobalEnv
  assign("IMLUI_DEV_MODE", TRUE, envir = global_env)
  devtools::load_all()
  repo_root <- dirname(system.file(package = "imlui"))
  options("shiny.autoreload" = TRUE)
  shiny::runApp(repo_root)
}
