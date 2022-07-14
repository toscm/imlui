pkg <- new.env(parent = emptyenv())
mock <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  message("Date       Time         S UID : Message")
  #       "2022-07-14 10:37:03.427 0 null: Reading C:/Users/tobi/.config/iml..."
  pkg$imlui_config <- read_imlui_config_file(create_if_missing = TRUE)
  pkg$db <- DB$new() # ok: tested dynamically
  pkg$sid <- 1 # for each session: use as session number and increase by 1
  shiny::addResourcePath(
    prefix = "imlui/assets",
    system.file("assets", package = "imlui")
  )
}
