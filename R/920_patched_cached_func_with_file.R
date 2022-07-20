#' @title Patched version of `shiny::cachedFuncWithFile`
#' @description Patched version of `shiny::cachedFuncWithFile. Code changes are
#' inspired by function `shiny::initAutoReloadMonitor` from file "shinyapp.R".
#' See [README.md](README.md#shiny-bugfixworkaround-autoreload-for-packages)
#' for details. Used by `dyn_serve_app_in_dev_mode`.
#' @param dir required by original func
#' @param file required by original func
#' @param func required by original func
#' @param case.sensitive required by original func
patched_cached_func_with_file <- function(dir,
                                          file,
                                          func,
                                          case.sensitive = FALSE) {
  message(red("Entered patched version of `cachedFuncWithFile`"))
  dir <- normalizePath(dir, mustWork = TRUE)
  value <- NULL
  filePattern <- getOption(
    "shiny.autoreload.pattern",
    ".*\\.(r|html?|js|css|png|jpe?g|gif)$"
  )
  last_mtimes <- NULL
  function(...) {
    file.path.func <- if (case.sensitive) file.path else file.path.ci
    fname <- file.path.func(dir, file)
    files <- list.files(dir, filePattern, recursive = TRUE, ignore.case = TRUE)
    files <- sort_c(files)
    mtimes <- file.info(files)$mtime
    names(mtimes) <- files
    if (!identical(last_mtimes, mtimes)) {
      value <<- func(fname, ...)
      last_mtimes <<- mtimes
    }
    value
  }
}
