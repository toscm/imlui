.shinyUI <- function(request) {
	logsn("Started .shinyUI from process ID:", Sys.getpid())
	###### Load Config #################################################################################################
	# NOTE: config is currently loaded twice. Once in UI (readonly) and once in Server (readwrite). Maybe improve later.
	logsn("Loading user config from UI...")
	filter <- dplyr::filter
	config <- readUserCnf()
	settings <- as.list(config$Settings$Value)
	appstate <- as.list(config$Appstate$Value)
	names(settings) <- config$Settings$ID
	names(appstate) <- config$Appstate$ID
	model_names <- dplyr::filter(config$Models, Symbol != "")$Name
	dataset_names <- dplyr::filter(config$Datasets, Symbol != "")$ID
	##### Actual UI ####################################################################################################
	tagList(
		tags$head(
			tags$style(
				HTML("
					html, body {
						height: 100%;
						margin: 0;
					}
				")
			)
		),
		fluidPage(
			title="IML-UI",
			style="min-height: calc(100vh - 50px);",
			htmltools::htmlDependency(
				# loads js/css files in folder <R-Library-Path>/imlui/assets
				name = "imlui-assets",
				version = packageVersion("imlui"),
				package = "imlui",
				src = "assets",
				script = c(
					"js/js-cookie-3.0.1/js.cookie.min.js",
					"js/window-dimensions-1.0.0/window-dimensions-1.0.0.js"
				),
				stylesheet = c(
					"css/styles.css"
				)
			),
			shinyjs::useShinyjs(), # loads js/css files in folder <R-Library-Path>/shiny/srcjs
			shinyjs::extendShinyjs(
				text = shinyauthr:::js_cookie_to_r_code("login_jscookie", expire_days = 7),
				functions = c("getcookie", "setcookie", "rmcookie")
			),
			# shinyjs::extendShinyjs(
			# 	script = system.file(
			# 		"assets/js/js-cookie-3.0.1-shinyjs-wrapper/js-cookie-3.0.1-shinyjs-wrapper.js",
			# 		package = "imlui"
			# 	),
			# 	functions = c("getcookie", "setcookie", "rmcookie")
			# ),
			shinyjs::extendShinyjs(
				text = shinyauthr:::js_return_click("login_password", "login_button"),
				functions = c()
			),
			shinycssloaders::withSpinner(uiOutput(outputId="web_app"))
		),
		includeHTML(system.file("assets/html/footer.html", package="imlui"))
	)
}
