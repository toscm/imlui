if (FALSE) {
	options(shiny.reactlog=TRUE)
	options(warn=2)
}
if (TRUE) {
	shiny::addResourcePath(prefix="imlui/assets", system.file("assets", package="imlui"))
	imlui_config.yml <- rlang::new_environment()
}
