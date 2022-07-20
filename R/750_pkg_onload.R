if (exists("IMLUI_DEV_MODE") && isTRUE(IMLUI_DEV_MODE)) {
  global_env <- .GlobalEnv
  if (!exists("pkg", envir=global_env)) {
    assign("pkg", new.env(parent = emptyenv()), envir=global_env)
  }
} else {
  pkg <- new.env(parent = emptyenv())
  mock <- new.env(parent = emptyenv())
}

.onLoad <- function(libname, pkgname) {
  pkg$imlui_config <- read_imlui_config_file(create_if_missing = TRUE)
  pkg$db <- DB$new() # ok: tested dynamically
  pkg$sid <- 1 # for each session: use as session number and increase by 1
  shiny::addResourcePath(
    prefix = "imlui/assets",
    directoryPath = system.file("assets", package = "imlui")
  )
}
