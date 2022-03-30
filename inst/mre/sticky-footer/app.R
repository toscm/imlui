library("shiny")
library("htmltools")

.shinyUI2 <- function(request) {
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
			sidebarLayout(
				sidebarPanel(
					width=3,
					style="min-height: 100%;",
					pickerInput("MM", "Models", choices=c("a", "b"), multiple=TRUE),
					pickerInput("DD", "Datasets", choices=c("c", "d"), multiple=TRUE)
				),
				mainPanel(width=9,
					actionButton(
						inputId="logout_button",
						label="Log out",
						icon=shiny::icon("sign-out-alt"),
						class="btn-danger pull-right",
						style="color: white; "
					),
					tabsetPanel(
						id = "MP_CS",
						tabPanel("Database Overview", div("DBO_T")),
						tabPanel("Dataset Analysis", div("DA_T")),
						tabPanel("Model Analysis", div("MA_T"))
					)
				)
			)
		),
		includeHTML(system.file("assets/html/footer.html", package="imlui"))
		# HTML('
		# 	<footer class="page-footer font-small blue pt-4 well" style="height: 50px; margin: 0; padding: 0;">
		# 		<span style="float:left; display:inline-block; padding-right:3px; padding-top: 3px;">
		# 			<img src="https://www.spang-lab.de/images/logo/ur.png" alt="ur.png"">
		# 		</span>
		# 		<span style="float:left; vertical-align: middle; align-items: center;">
		# 			<a href="https://www.spang-lab.de/contact">Impressum/Contact</a> ·
		# 			<a href="https://www.spang-lab.de/disclaimer">Legal Notice</a> ·
		# 			<a href="https://www.spang-lab.de/privacy">Privacy</a>
		# 		</span>
		# 		<span style="float:right;">
		# 			<a href="#!">Institute of Functional Genomics</a> -
		# 			Statistical Bioinformatics Department -
		# 			<a href="#!">University of Regensburg</a>
		# 		</span>
		# 		<span style="float:left; display:inline-block;">
		# 			<img src="https://www.spang-lab.de/images/logo/logo_only_transparent.png" style="max-height: 100%; max-width: 100%;">
		# 		</span>
		# 	</footer>
		# ')
			# <div class="col-md-4 mt-md-0 mt-3">
			# 	<a href="#!">Impressum/Contact</a>
			# </div>
			# <div class="col-md-4 mb-md-0 mb-3">
			# 	<a href="#!">Link 2</a>
			# </div>
			# <div class="col-md-4 mb-md-0 mb-3">
			# 	<a href="#!">Link 3</a>
			# </div>
	)
}

.shinyServer2 <- function(input, output, session) {

}

runserver2 <- function(port=8080) {
	logsn("Function runserver called from process ID:", Sys.getpid())
	logsn(paste0("Open http://localhost:", port, " to try the app."))
	runApp(
		appDir=shinyApp(
			ui=.shinyUI2, # object like fluidPage or function(request)
			server=.shinyServer2, # function(input, output, session)
			enableBookmarking="url"
		),
		host="0.0.0.0",
		port=port,
		launch.browser=FALSE,
		quiet=TRUE
	)
}

runserver2(8081)

