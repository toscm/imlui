# See README/#ui-layout for an overview
.ui <- function(request) {
	logsne("Starting web app from process ID:", Sys.getpid(), "...")
	tagList(
		tags$head(tags$style(HTML("html, body {height: 100%; margin: 0;}"))),
		fixedPage(
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
				text = js_cookie_to_r_code(),
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
				text = js_return_click(),
				functions = c()
			),
			uiOutput(outputId="web_app")
		)
		# , includeHTML(system.file("assets/html/footer.html", package="imlui"))
	)
}

.web_app <- function(data) {
	if (is.null(data$appstate$user$is_authenticated)) {
		return(NULL)
	} else if (isTRUE(data$appstate$user$is_authenticated)) {
		.home_page(data)
	} else if (isFALSE(data$appstate$user$is_authenticated)) {
		.login_page(data)
	} else {
		stop(
			"data$appstate$user$is_authenticated must be either NULL, TRUE or FALSE, but is",
			dput2(data$appstate$user$is_authenticated)
		)
	}
}

.home_page <- function(data) {
	div(
		sidebarLayout(
			.side_bar(data)
			.main_panel(data)
		)
		includeHTML(system.file("assets/html/footer.html", package="imlui"))
	)
}

.side_bar <- function(data) {
	sidebarPanel(
		width=3,
		# pickerInput("Paper", "Paper", choices=names(database$Papers), multiple=TRUE),
		# pickerInput("Method", "Method", choices=MODEL_NAMES, multiple=TRUE),
		pickerInput("MM", "Models", choices=MODEL_NAMES(), multiple=TRUE),
		pickerInput("DD", "Datasets", choices=DATASET_SYMBOLS(), multiple=TRUE),
		uiOutput(outputId="PAS_W"),
		uiOutput(outputId="AI_TO"),
		# actionButton("CC_B", "Clear Cache"),
		# actionButton("R_B", "Redraw")
		verbatimTextOutput("auth_output")
	)
}

.main_panel <- function(data) {
	mainPanel(
		width=9,
		actionButton(
			inputId="logout_button",
			label="Log out",
			icon=icon("sign-out-alt"),
			class="btn-danger pull-right",
			style="color: white; "
		),
		tabsetPanel(
			id = "MP_CS",
			selected = "Model Analysis",
			tabPanel("Model Analysis", uiOutput(outputId="MA_T")),
			tabPanel("Dataset Analysis", uiOutput(outputId="DA_T")),
			if (grepl("admin", GROUP_IDS())) {
				tabPanel("Settings", uiOutput(outputId="S_T"))
			}
		)
	)
}

models_menu <- function(data) {

}

datasets_menu <- function(data) {

}

plot_area_size_widget <- function(data) {

}

app_info_text_output <- function(data) {

}

clear_cache_button <- function(data) {

}

redraw_button <- function(data) {

}


settings_tab <- function(data) {

}

papers_tab <- function(data) {

}

models_tab <- function(data) {

}

datasets_tab <- function(data) {

}

settings_tab <- function(data) {

}

samples_tab <- function(data) {

}

datatypes_tab <- function(data) {

}

methods_tab <- function(data) {

}

platforms_tab <- function(data) {

}

data_analysis_tab <- function(data) {

}

descriptions_tab <- function(data) {

}

msd_plot_tab <- function(data) {

}

model_analysis_tab <- function(data) {

}

descriptions_tab <- function(data) {

}

predictions_tab <- function(data) {

}

survival_curves_tab <- function(data) {

}

feature_effect_plot_tab <- function(data) {

}

session_info_tab <- function(data) {

}

login_page <- function(data) {
	div(
		id="login_panel",
		style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
		tags$h1("Login"),
		wellPanel(
			textInput("login_user_name", tagList(icon("user"), "Username or email")),
			passwordInput("login_password", tagList(icon("unlock-alt"), "Password")),
			div(
				style = "text-align: center;",
				actionButton("login_button", "Sign in", class="btn btn-primary btn-block")
			),
			shinyjs::hidden(
				div(
					id = "login_error",
					tags$p(
						"Login failed!",
						style = "color: red; font-weight: bold; padding-top: 5px;",
						class = "text-center"
					)
				)
			)
		),
		wellPanel(
			div(
				style = "text-align: left;",
				tags$div(
					tagList(
						icon("sign-in-alt"),
						tags$b("Sign in with")
					),
					style = "text-align: left;"
				),
				actionButton(
					inputId = "login_button_auth_spang_lab",
					label = tagList(
						img(
							src="imlui/assets/png/spang-lab-logo-64x64.png",
							alt="spang-lab-logo.png",
							style="height: 1.5em;"
						),
						"Spang Auth"
					),
					class = "btn-block"
				),
				actionButton(
					inputId = "login_button_gitlab_spang_lab",
					label = tagList(
						img(
							src="imlui/assets/png/spang-lab-logo-64x64.png",
							alt="spang-lab-logo.png",
							style="height: 1.5em;"
						),
						"Spang Gitlab"
					),
					class = "btn-block"
				),
				actionButton(
					inputId = "login_button_google",
					label = tagList(
						img(
							src="imlui/assets/png/google-logo-48x48.png",
							alt="google-logo.png",
							style="height: 1.5em;"
						),
						"Google"
					),
					class = "btn-block"
				),
				actionButton(
					inputId = "login_button_github",
					label = tagList(
						img(
							src="imlui/assets/png/github-mark-32px.png",
							alt="github-mark.png",
							style="height: 1.5em;"
						),
						"GitHub"
					),
					class = "btn-block"
				),
				actionButton(
					inputId = "login_button_gitlab",
					label = tagList(
						img(
							src="imlui/assets/png/gitlab-icon-rgb.svg",
							alt="gitlab-icon.svg",
							style="height: 1.5em;"
						),
						"Gitlab"
					),
					class = "btn-block"
				)
			)
		)
	)
}

