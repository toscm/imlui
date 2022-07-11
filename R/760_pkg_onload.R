pkg <- new.env(parent = emptyenv())
mock <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  pkg$imlui_config <- read_imlui_config_file(create_if_missing = TRUE)
  shiny::addResourcePath(
    prefix = "imlui/assets",
    system.file("assets", package = "imlui")
  )
}
