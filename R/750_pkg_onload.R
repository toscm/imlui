if (exists("IMLUI_DEV_MODE") && isTRUE(IMLUI_DEV_MODE)) {
  global_env <- .GlobalEnv
  if (!exists("pkg", envir = global_env)) {
    assign("pkg", new.env(parent = emptyenv()), envir = global_env)
  }
} else {
  pkg <- new.env(parent = emptyenv())
  mock <- new.env(parent = emptyenv())
}

.onLoad <- function(libname, pkgname) {}
