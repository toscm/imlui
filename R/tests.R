library(shiny)

test_ImluiApp <- function() {
	options(
		shiny.trace=TRUE,
		shiny.autoreload=TRUE,
		shiny.reactlog=TRUE,
		shiny.minified=FALSE,
		shiny.fullstacktrace=TRUE,
		shiny.stacktraceoffset=TRUE
	)
	shinyAppDir(appDir = toscutil::getpd("DESCRIPTION"))
}
