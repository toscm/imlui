.shinyServer <- function(input, output, session) {

######################################## ON SESSION START ##############################################################
	######################################## 0. Spec ###################################################################
		# 1. Print short log message that server has started
		# 2. Read in imlui_config.yml
		# 3. Read settings from database
		# 4. Initialize global variables incl. authentication state, i.e.
		#
		# 	 Check if parameter `code` is present in URL. If yes, we're coming back from the the redirection to Github
		#    as part of the Oauth2 flow and need to set credentials$user_auth as well as the login cookie.
		#
		#    Maybe better: store cookie "OAuth2_Flow_Started=TRUE; OAuth2_Target=https://github.com/auth" or similar
		#    before starting OAuth2 flow. Then, not only check for param `code` in URL but also for cookie. If both are
		#    present, check if authentication worked and delete cookie.

	####################################### 1. Print log message #######################################################
	logsn("Started .shinyServer from process ID:", Sys.getpid())

	####################################### 2. Read imlui_config.yml ###################################################
		# TODO: config is currently loaded twice. Once in UI (readonly) and once in Server (readwrite). Make whole UI
		# a single uiOutput() and put all logic into server.
		config <- readUserCnf()

	####################################### 3. Read settings from database #############################################
		appstate <- as.list(config$Appstate$Value)
		settings <- as.list(config$Settings$Value)
		names(appstate) <- config$Appstate$ID
		names(settings) <- config$Settings$ID
		models <- dplyr::filter(config$Models, Symbol != "")$Symbol
		model_names <- dplyr::filter(config$Models, Symbol != "")$Name
		dataset_names <- dplyr::filter(config$Datasets, Symbol != "")$ID

	####################################### 4. Init globals incl. auth state ###########################################



		######################################## AUTHENTICATION VIA USERNAME PASSWORD ######################################
			if (file.exists("my_db_file")) {
				DB <- DBI::dbConnect(RSQLite::SQLite(), "my_db_file")
			} else {
				DB <- DBI::dbConnect(RSQLite::SQLite(), "my_db_file")
				DBI::dbCreateTable(DB, "sessionids", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))
			}
			DATA <- tibble::tibble(
				user = c("user1", "user2"),
				password = c("pass1", "pass2"),
				permissions = c("admin", "standard"),
				name = c("User One", "User Two")
			)
			user_col=rlang::sym("user")
			pwd_col=rlang::sym("password")
			sessionid_col=rlang::sym("sessionid")
			sodium_hashed = FALSE
			reload_on_logout = TRUE
			cookie_getter <- function(conn=DB, expiry=7) {
				# This function must return a data.frame with columns user and sessionid. Other
				# columns are also okay and will be made available to the app after log in as
				# columns in credentials()$user_auth
				DBI::dbReadTable(conn, "sessionids") %>%
					mutate(login_time = ymd_hms(login_time)) %>%
					as_tibble() %>%
					dplyr::filter(login_time > now() - days(expiry))
			}
			cookie_setter <- function(user, sessionid, conn=DB) {
				# This function must accept two parameters: user and sessionid. It will be
				# called whenever the user successfully logs in with a password. This function
				# saves to your database.
				tibble(
					user=user,
					sessionid=sessionid,
					login_time=as.character(now())
				) %>%
				DBI::dbWriteTable(conn, "sessionids", ., append=TRUE)
			}

		######################################## AUTHENTICATION VIA GITHUB #################################################
			shinygithub_oauth_app <- httr::oauth_app(
				appname="shinygithub", # TODO: take from imlui_db/settings
				key="51d46f96810d1fd182a2", # TODO: take from imlui_db/settings
				secret="66eec8782825eeb61007dbef32f91afc9c3587aa", # imlui_db/settings
				redirect_uri=paste0(
					isolate(session$clientData$url_protocol), # http:
					"//",
					isolate(session$clientData$url_hostname), # localhost
					":",
					isolate(session$clientData$url_port)
				)
			)


	params <- parseQueryString(isolate(session$clientData$url_search))
	if (!is.null(params$code)) {
		github_oauth2_access_token <- httr::oauth2.0_access_token(
			endpoint=oauth_endpoints("github"),
			app=shinygithub_oauth_app,
			code=params$code
		)
		github_oauth2_token <- httr::oauth2.0_token(
			app=shinygithub_oauth_app,
			endpoint=oauth_endpoints("github"),
			credentials=github_oauth2_access_token,
			cache=FALSE
		)
		resp <- GET("https://api.github.com/user", config(token=github_oauth2_token))
		if (httr::status_code(resp) == 200) {
			logsn("Github login successful")
			credentials$user_auth <- TRUE
			credentials$info <- httr::content(resp)
			# List of: 32
			# Colnames: login: id, node_id, avatar_url, gravatar_id, url, html_url, followers_url, following_url, gists_url,
			#           starred_url, subscriptions_url, organizations_url, repos_url, events_url, received_events_url,
			#           type, site_admin, name, company, blog, location, email, hireable, bio, twitter_username,
			#           public_repos, public_gists, followers, following, created_at, updated_at
			# Important: login:      "toscm"
			#            avatar_url: "https://avatars.githubusercontent.com/u/12760468?v=4"
			#            name:       "Tobias Schmidt"
			#            email:      NULL
			.sessionid <- randomString()
			shinyjs::js$setcookie(.sessionid)
			cookie_setter(httr::content(resp)$login, .sessionid)
			cookie_data <- dplyr::filter(dplyr::select(cookie_getter(), -{{user_col}}), {{sessionid_col}} == .sessionid)
			# if (nrow(cookie_data) == 1) {
			# 	credentials$info <- dplyr::bind_cols(credentials$info, cookie_data)
			# }
		} else {
			shinyjs::show(id="login_error")
		}
	}

######################################## REACTIVE VALUES ###############################################################
	II <- input
	OO <- output
	credentials <- shiny::reactiveValues(user_auth=FALSE, info=NULL, cookie_already_checked=FALSE)
	RV <- reactiveValues(oauth2_flow_started=FALSE, redirected_to=NULL)
	# session$clientData$
	# 	url_protocol: http
	# 	url_hostname: localhost
	# 	url_port: 8080
	# 	url_pathname:
	# 	url_search:
	# 	url_hash_initial:
	# 	url_hash:
	# 	output_web_app_width:
	# 	output_web_app_height:

######################################## REACTIVES #####################################################################

	# Duplicated letters like MM indicate lists, so you can do `for (M in MM)`
	# Tripled letters like YYY indicate list of lists `for (M in MM) {for (D in DD) {YYY[[M]][[D]]}}`
	r  <- reactive
	l  <- clapply

	APAH      <- r({ PAHS[max(which(PAHS < (MPH() - 120)), 1)] }) # APAH = Automatic Plot Area Height = num
	APAW      <- r({ PAWS[max(which(PAWS < (MPW() -   0)), 1)] }) # APAW = Automatic Plot Area Width = num
	BH        <- r({ if (is.null(II$dim[2])) 480 else II$dim[2] })  # BH = Browser Height = num
	BW        <- r({ if (is.null(II$dim[1])) 640 else II$dim[1] })  # BW = Browser Width = num
	MPAH      <- r({ II$MPAH })  # MPAH = Manual Plot Area Height = num
	MPAW      <- r({ II$MPAW })  # MPAW = Manual Plot Area Width = num
	MPH       <- r({ BH() -  42.00 }) # MPH = Main Panel Height = num
	MPW       <- r({ BW() *   0.75 }) # MPW = Main Panel Width = num
	PAH       <- r({ if ( PASC() == "Fixed" && !(is.null(MPAH())) ) MPAH() else APAH() }) # PAH = Plot Area Height = num
	PAHPX0    <- r({ paste0(PAH() -   0, "px") }) # PAH = Plot Area Height in Pixels = num
	PAHPX1    <- r({ paste0(PAH() - 120, "px") }) # PAH = Plot Area Height in Pixels = num
	PAHPX1_05 <- r({ paste0((PAH() - 120) * 0.5, "px") }) # PAH = Plot Area Height in Pixels = num
	PAHPX2    <- r({ paste0(PAH() - 240, "px") }) # PAH = Plot Area Height in Pixels = num
	PAHPX3    <- r({ paste0(PAH() - 360, "px") }) # PAH = Plot Area Height in Pixels = num
	PASC      <- r({ II$PASC })  # PASC = Plot Area Size Calculation = char
	PAW       <- r({ if ( PASC() == "Fixed" && !(is.null(MPAW())) ) MPAW() else APAW() }) # PAW = Plot Area Width = num
	PAWPX     <- r({ paste0(PAW(), "px") }) # PAW = Plot Area Width in Pixels = num

	CXX  <- r({ l(XX(), function(X) select(X, where(is.factor))) }) # CXX = Categorical covariate dataframe[dataset] = list(data.frame)
	CXXR <- l( datasets, function(d) { r({ select(XXR[[d]](),  where(is.factor)) }) } ) # CXXR = Categorical covariate dataframe[dataset] = list(r(data.frame))
	DD   <- r({ if (!is.null(II$DD)) II$DD else list() })  # DD = Datasets = list(char)
	FF   <- r({ l(PP(), names) }) # FF = Features[model] = list(vector(char))
	FFR  <- l( models, function(m) { r({ names(PPR[[m]]()) }) } ) # FFR = Feaures[model] = list(r(vector(char)))

	MM   <- r({ if (!is.null(II$MM)) config$Models$Symbol[match(II$MM, config$Models$Name)] else list() })  # MM = Models = list(char)

	NXX  <- r({ l(XX(), function(X) select(X, !where(is.factor))) }) # NXX = Numerical covariate dataframe[dataset] = list(data.frame)
	NXXR <- l( datasets, function(d) { r({ select(XXR[[d]](), !where(is.factor)) }) } ) # NXXR = Numerical covariate dataframe[dataset] = list(r(data.frame))
	PP   <- r({ l(MM(), function(m) PPR[[m]]()) }) # PP = Parameters[model] = list(num/obj)
	PPR  <- l( models, function(m) { r({ getdata(m, t="m") }) } ) # PPR = Parameters[model] = list(r(num/obj))
	SS   <- r({ l(XX(), rownames) }) # SS = Samples[dataset] = list(vector(char))
	XX   <- r({ l(DD(), function(d) XXR[[d]]() ) }) # XX = covariate dataframe[dataset] = list(data.frame)
	XXR  <- l( datasets, function(d) { r({ do.call(rename, list(XXR.[[d]](), FEATURE_MAPPINGS[[d]]) ) }) } ) # XXR = covariate dataframe[dataset] = list(r(data.frame))
	XXR. <- l( datasets, function(d) { r({ getdata(d) }) } ) # XXR = covariate dataframe[dataset] = list(r(data.frame))
	YYY  <- r({ l(MM(), function(M) { l(DD(), function(D) { YYYR[[M]][[D]]() }) }) }) # YYY = Outcomes[model][dataset] = r(list(list(num)))
	YYYR <- l( models, function(m) { l(datasets, function(d) { r({ possibly(predict, NULL)(PPR[[m]](), XXR[[d]]()) }) } ) } ) # YYYR = Outcomes[model][dataset] = list(list(reactive(num)))


######################################## OBSERVERS #####################################################################
	######################################## ON SESSION END ############################################################
		# store used config
		onSessionEnded( function() {
			logsn("Storing user config...")
			rownames(config$Appstate) <- config$Appstate$ID
			for (ID in config$Appstate$ID) {
				config$Appstate[[ID, "Value"]] <- isolate(II[[ID]]) %||% appstate[[ID]] %||% settings[[ID]]
			}
			rownames(config$Appstate) <- NULL
			storeUserCnf(config)
		})

	######################################## ON STOP ###################################################################
		# do nothing
		onStop(
			function() {
				# logsn("Shiny stopped!")
			}
		)

	######################################## ON FLUSH ##################################################################
		# do nothing
		onFlush(
			function() {
				# logsn("Shiny flushes now!")
			},
			once = FALSE,
			session = getDefaultReactiveDomain()
		)

	######################################## ON FLUSHED ################################################################
		# do nothing
		onFlushed(
			function() {
			# logsn("Shiny flushed!")
			},
			once = FALSE,
			session = getDefaultReactiveDomain()
		)

	######################################## ON LOGOUT_BUTTON CLICK ####################################################
		# remove cookie and reload session
		shiny::observeEvent(
			input$logout_button,
			{
				shinyjs::js$rmcookie()
				if (reload_on_logout) {
					session$reload()
				} else {
					shiny::updateTextInput(session, "password", value = "")
					credentials$user_auth <- FALSE
					credentials$info <- NULL
				}
			}
		)

	######################################## ON CHANGE OF CREDENTIALS$USER_AUTH ########################################
		# show/hide login panel
		shiny::observe({
			if (credentials$user_auth) {
				# waiter_hide()
				# shinyjs::hide(id = "login_panel")
				# shinyjs::show(id = "web_app")
			} else if (credentials$cookie_already_checked) {
				# waiter_hide()
				# shinyjs::show(id = "login_panel")
				# shinyjs::hide(id = "web_app")
			}
		})

	######################################## WHEN JAVASCRIPT IS READY ##################################################
		# retrieve cookie
		shiny::observeEvent(
			shiny::isTruthy(shinyjs::js$getcookie()), {
				shinyjs::js$getcookie()
				logsn("Called shinyjs::js$getcookie()")
			}
		)

	######################################## WHEN VALID COOKIE IS PRESENT ##############################################
		# update expiry date
		shiny::observeEvent(
			input$login_jscookie, {
				credentials$cookie_already_checked <- TRUE
				# if already logged in or cookie missing, ignore change in input$login_jscookie
				shiny::req(credentials$user_auth == FALSE, is.null(input$login_jscookie) == FALSE, nchar(input$login_jscookie) > 0)
				cookie_data <- dplyr::filter(cookie_getter(), {{sessionid_col}} == input$login_jscookie)
				# logsn(sprintf("login cookie detected (user: %s, sessionid: %s, login_time: %s)",
				# 		input$login_jscookie$user, input$login_jscookie$sessionid, input$login_jscookie$login_time))
				if (nrow(cookie_data) != 1) {
					shinyjs::js$rmcookie()
				} else {
					# if valid cookie, we reset it to update expiry date
					.userid <- dplyr::pull(cookie_data, {{user_col}})
					.sessionid <- randomString()
					shinyjs::js$setcookie(.sessionid)
					cookie_setter(.userid, .sessionid)
					cookie_data <- utils::head(dplyr::filter(cookie_getter(), {{sessionid_col}} == .sessionid, {{user_col}} == .userid))
					credentials$user_auth <- TRUE
					credentials$info <- dplyr::bind_cols(dplyr::filter(DATA, {{user_col}} == .userid), dplyr::select(cookie_data, -{{user_col}}))
				}
			}
		)

	######################################## WHEN LOGIN BUTTON IS CLICKED ##############################################
		# check user/pass and set cookie
		shiny::observeEvent(
			input$login_button, {
				logsn("login button clicked:", input$login_button)
				# check for match of input username to username column in DATA
				row_username <- which(dplyr::pull(DATA, {{user_col}}) == input$login_user_name)
				if (length(row_username)) {
					row_password <- dplyr::filter(DATA, dplyr::row_number() == row_username)
					row_password <- dplyr::pull(row_password, {{pwd_col}})
					if (sodium_hashed) {
						password_match <- sodium::password_verify(row_password, input$login_password)
					} else {
						password_match <- identical(row_password, input$login_password)
					}
				} else {
					password_match <- FALSE
				}
				if (length(row_username) == 1 && password_match) {
					# if user name row and password name row are same, credentials are valid
					credentials$user_auth <- TRUE
					credentials$info <- dplyr::filter(DATA, {{user_col}} == input$login_user_name)
					.sessionid <- randomString()
					shinyjs::js$setcookie(.sessionid)
					cookie_setter(input$login_user_name, .sessionid)
					cookie_data <- dplyr::filter(dplyr::select(cookie_getter(), -{{user_col}}), {{sessionid_col}} == .sessionid)
					if (nrow(cookie_data) == 1) {
						credentials$info <- dplyr::bind_cols(credentials$info, cookie_data)
					}
					logsn("Authenticated successfully")
				} else {
					# if not valid temporarily show error logsn to user
					shinyjs::toggle(id = "login_error", anim = TRUE, time = 1, animType = "fade")
					shinyjs::delay(5000, shinyjs::toggle(id = "login_error", anim = TRUE, time = 1, animType = "fade"))
				}
			}
		)

	######################################## WHEN LOGIN_GITHUB BUTTON IS CLICKED #######################################
		# start OAuth2.0 authentication, i.e. redirect to
		# <https://github.com/login/oauth/{authorize/access_token}>
		shiny::observeEvent(
			eventExpr = input$login_button_github,
			handlerExpr = {
				logsn("input$login_button_github clicked:", input$login_button_github)
				url <- httr::oauth2.0_authorize_url(
					endpoint = httr::oauth_endpoints("github"),
					app = shinygithub_oauth_app,
					scope = ""
				)
				OO$web_app <- sprintf('location.replace("%s");', url) %>%
					shiny::HTML() %>%
					shiny::tags$script() %>%
					shiny::renderUI()
				RV$oauth2_flow_started <- TRUE # TODO: remove if not used
				RV$redirected_to <- "Github" # TODO: remove if not used
			}
		)

	######################################## ON CHANGE OF URL ##########################################################
		# print URL details for logging
		observe({
			logsn("session$clientData changed:")
			logsn("url_protocol:", session$clientData$url_protocol)
			logsn("url_hostname:", session$clientData$url_hostname)
			logsn("url_port:", session$clientData$url_port)
			logsn("url_pathname:", session$clientData$url_pathname)
			logsn("url_search:", session$clientData$url_search)
			logsn("url_hash_initial:", session$clientData$url_hash_initial)
			logsn("url_hash:", session$clientData$url_hash)
			logsn("output_web_app_width:", session$clientData$output_web_app_width)
			logsn("output_web_app_height:", session$clientData$output_web_app_height)
		})

	######################################## ON ANY INPUT CHANGE: ######################################################
		# update the query string
		# (based on <https://mastering-shiny.org/action-bookmark.html#action-bookmark>)
		observe({
		reactiveValuesToList(input) #1
		#1 Not quite sure why this is necessary. Maybe to trigger this on any input change, however if this is the case,
		# converting to all reactive values to a list seems a bit costly in terms of runtime. Maybe improve later.
		session$doBookmark() # triggers onBookmark and onBookmarked callback functions
		})
		onBookmarked(updateQueryString) # Update the query string

######################################## BOOKMARKING ###################################################################
	# TODO: remove the following elements from bookmarking: action button, cookies, credentials (usernames, token passwords)
	setBookmarkExclude(c(
		"login_button",
		"login_button_github",
		"login_button_google",
		"login_button_auth_spang_lab",
		"login_button_gitlab_spang_lab",
		"DBO_DS_TBL_cell_clicked",
		"DBO_DS_TBL_cells_selected",
		"DBO_DS_TBL_columns_selected",
		"DBO_DS_TBL_rows_all",
		"DBO_DS_TBL_rows_current",
		"DBO_DS_TBL_rows_selected",
		"DBO_DS_TBL_search",
		"DBO_DS_TBL_state",
		"DBO_MO_TBL_cell_clicked",
		"DBO_MO_TBL_cells_selected",
		"DBO_MO_TBL_columns_selected",
		"DBO_MO_TBL_rows_all",
		"DBO_MO_TBL_rows_current",
		"DBO_MO_TBL_rows_selected",
		"DBO_MO_TBL_search",
		"DBO_MO_TBL_state",
		"DBO_PA_TBL_cell_clicked",
		"DBO_PA_TBL_cells_selected",
		"DBO_PA_TBL_columns_selected",
		"DBO_PA_TBL_rows_all",
		"DBO_PA_TBL_rows_current",
		"DBO_PA_TBL_rows_selected",
		"DBO_PA_TBL_search",
		"DBO_PA_TBL_state",
		"DBO_SE_TBL_cell_clicked",
		"DBO_SE_TBL_cells_selected",
		"DBO_SE_TBL_columns_selected",
		"DBO_SE_TBL_rows_all",
		"DBO_SE_TBL_rows_current",
		"DBO_SE_TBL_rows_selected",
		"DBO_SE_TBL_search",
		"DBO_SE_TBL_state",
		"login_jscookie",
		"logout_button"
	))

######################################## UI WEBAPP #####################################################################

	OO$web_app <- renderUI({
		if (credentials$user_auth) {
			div(
				sidebarLayout(
					sidebarPanel(width=3,
						# pickerInput("Paper", "Paper", choices=names(config$Papers), multiple=TRUE),
						# pickerInput("Method", "Method", choices=model_names, multiple=TRUE),
						pickerInput("MM", "Models", choices=model_names, multiple=TRUE, selected=appstate[["MM"]] %||% settings[["MM"]]),
						pickerInput("DD", "Datasets", choices=datasets, multiple=TRUE, selected=appstate[["DD"]] %||% settings[["DD"]]),
						uiOutput(outputId="PAS_W"),
						uiOutput(outputId="AI_TO"),
						# actionButton("CC_B", "Clear Cache"),
						# actionButton("R_B", "Redraw")
						verbatimTextOutput("auth_output")
					),
					mainPanel(
						width=9,
						actionButton(
							inputId="logout_button",
							label="Log out",
							icon=shiny::icon("sign-out-alt"),
							class="btn-danger pull-right",
							style="color: white; "
						),
						tabsetPanel(
							id = "MP_CS",
							selected = appstate[["MP_CS"]] %||% settings[["MP_CS"]],
							tabPanel("Database Overview", uiOutput(outputId="DBO_T")),
							tabPanel("Dataset Analysis", uiOutput(outputId="DA_T")),
							tabPanel("Model Analysis", uiOutput(outputId="MA_T")),
							# , tabPanel("Session Info", verbatimTextOutput(outputId="SI_TO"))
						)
					)
				)
				#, includeHTML(system.file("assets/html/footer.html", package="imlui"))
			)
		} else {
			div(
				id="login_panel",
				style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
				shiny::tags$h1("Login"),
				shiny::wellPanel(
					shiny::textInput("login_user_name", shiny::tagList(shiny::icon("user"), "Username or email")),
					shiny::passwordInput("login_password", shiny::tagList(shiny::icon("unlock-alt"), "Password")),
					shiny::div(
						style = "text-align: center;",
						shiny::actionButton("login_button", "Sign in", class="btn btn-primary btn-block")
					),
					shinyjs::hidden(
						shiny::div(
							id = "login_error",
							shiny::tags$p(
								"Login failed!",
								style = "color: red; font-weight: bold; padding-top: 5px;",
								class = "text-center"
							)
						)
					)
				),
				shiny::wellPanel(
					shiny::div(
						style = "text-align: left;",
						shiny::tags$div(
							shiny::tagList(
								shiny::icon("sign-in-alt"),
								shiny::tags$b("Sign in with")
							),
							style = "text-align: left;"
						),
						shiny::actionButton(
							inputId = "login_button_auth_spang_lab",
							label = shiny::tagList(
								shiny::img(
									src="imlui/assets/png/spang-lab-logo-64x64.png",
									alt="spang-lab-logo.png",
									style="height: 1.5em;"
								),
								"Spang Auth"
							),
							class = "btn-block"
						),
						shiny::actionButton(
							inputId = "login_button_gitlab_spang_lab",
							label = shiny::tagList(
								shiny::img(
									src="imlui/assets/png/spang-lab-logo-64x64.png",
									alt="spang-lab-logo.png",
									style="height: 1.5em;"
								),
								"Spang Gitlab"
							),
							class = "btn-block"
						),
						shiny::actionButton(
							inputId = "login_button_google",
							label = shiny::tagList(
								shiny::img(
									src="imlui/assets/png/google-logo-48x48.png",
									alt="google-logo.png",
									style="height: 1.5em;"
								),
								"Google"
							),
							class = "btn-block"
						),
						shiny::actionButton(
							inputId = "login_button_github",
							label = shiny::tagList(
								shiny::img(
									src="imlui/assets/png/github-mark-32px.png",
									alt="github-mark.png",
									style="height: 1.5em;"
								),
								"GitHub"
							),
							class = "btn-block"
						),
						shiny::actionButton(
							inputId = "login_button_gitlab",
							label = shiny::tagList(
								shiny::img(
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
	})

######################################## UI TABS LAYER 1 ###############################################################

	# TODO: Database Overview - Tabset
	OO$DBO_T <- renderUI({
		glueerr("isolate(II$DBO_T_CS): '{isolate(II$DBO_T_CS)}'")
		tabsetPanel(
			id="DBO_T_CS"
			, tabPanel("Paper",     uiOutput(outputId="DBO_PA_T"))
			, tabPanel("Models",    uiOutput(outputId="DBO_MO_T"))
			, tabPanel("Datasets",  uiOutput(outputId="DBO_DS_T"))
			# , tabPanel("Samples",   uiOutput(outputId="DBO_SA_T"))
			# , tabPanel("Datatypes", uiOutput(outputId="DBO_DT_T"))
			# , tabPanel("Methods",   uiOutput(outputId="DBO_ME_T"))
			# , tabPanel("Platforms", uiOutput(outputId="DBO_PF_T"))
			, tabPanel("Settings",  uiOutput(outputId="DBO_SE_T"))
		)
	})

	# OK: Data Analysis - Tabset
	OO$DA_T <- renderUI({
		if (length(DD()) == 0) {
			div(HTML("Please choose a dataset first"))
		} else {
			glueerr("isolate(II$DA_T_CS):  '{isolate(II$DA_T_CS)}'")
			tabsetPanel(
				id="DA_T_CS", selected=isolate(II$DA_T_CS) %||% settings[["DA_T_CS"]]
				, tabPanel("Descriptions", shinycssloaders::withSpinner(verbatimTextOutput(outputId="DA_DD_TO")))
				, tabPanel("MSD-Plot", shinycssloaders::withSpinner(uiOutput(outputId="DA_MSDP_T")))
			)
		}
	})

	# OK: Model Analysis - Tabset
	OO$MA_T <- renderUI({
		if (length(MM()) == 0) {
			div(HTML("Please choose a model first"))
		} else {
			glueerr("isolate(II$MA_T_CS):  '{isolate(II$MA_T_CS)}'")
			tabsetPanel(
				id="MA_T_CS", selected=isolate(II$MA_T_CS) %||% settings[["MA_T_CS"]]
				, tabPanel("Descriptions", verbatimTextOutput(outputId="MA_MD_TO"))
				, tabPanel("Predictions", uiOutput(outputId="MA_MP_T"))
				# , tabPanel("Survival Curves", uiOutput(outputId="MA_SC_T"))
				, tabPanel("Feature Effects", uiOutput(outputId="MA_FE_T"))
				# tabPanel("Feature Contributions", uiOutput(outputId="MA_FC_T"))
			)
		}
	})

######################################## UI TABS LAYER 2 ###############################################################

	OO$DBO_PA_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_PA_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_PA_TBL2") ) ) ) }) # Paper
	OO$DBO_MO_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_MO_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_MO_TBL2") ) ) ) }) # Models
	OO$DBO_DS_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_DS_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_DS_TBL2") ) ) ) }) # Datasets
	OO$DBO_SA_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_SA_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_SA_TBL2") ) ) ) }) # Samples
	OO$DBO_DT_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_DT_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_DT_TBL2") ) ) ) }) # Datatypes
	OO$DBO_ME_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_ME_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_ME_TBL2") ) ) ) }) # Methods
	OO$DBO_PF_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_PF_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_PF_TBL2") ) ) ) }) # Platforms
	OO$DBO_SE_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_SE_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_SE_TBL2") ) ) ) }) # Settings

	# OK: Model Analysis - Survival Curves - Tab
		OO$MA_SC_T <- renderUI({
			# logsn("MA_P_T: ")
			list(
				fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="SCP", height=PAHPX2(), width=PAWPX())))),
				fluidRow(column(3, selectInput(inputId="survPlotM", label="Model", choices=models)))
			)
		})


	# TODO: Dataset Analysis - MSD-Plot - Tab
		OO$DA_MSDP_T <- renderUI({
			# Vars
			xtc <- ytc <- c("identity", "log2", "log10")
			xt <- isolate(II$MSDP_XT) %||% "identity"
			yt <- isolate(II$MSDP_YT) %||% "identity"
			mm <- MM()
			# User info
			# logsn("DA_MSDP_T: ")
			# Row 1 (plot only)
			MSDP_PO <- column(12, shinycssloaders::withSpinner(plotOutput(outputId="MSDP", inline=TRUE)))
			row1 <- fluidRow(MSDP_PO)
			# Row 2 (input widgets)
			MSDP_XT_SI <- column(2, selectInput(inputId="MSDP_XT", label="x-Transformation", choices=xtc, selected=xt))
			MSDP_YT_SI <- column(2, selectInput(inputId="MSDP_YT", label="y-Transformation", choices=ytc, selected=yt))
			if ( is.none(mm) ) {
				row2 <- fluidRow(MSDP_XT_SI, MSDP_YT_SI)
			} else {
				m <- isolate(II$MSDP_M) %||% MM()[[1]]
				MSDP_M_SI <- column(4, selectInput(inputId="MSDP_M", label="Model", choices=mm, selected=m))
				MSDBO_MFO_CI <- column(3, checkboxInput(inputId="MSDP_MFO", label="Model Features only", value=TRUE))
				row2 <- fluidRow(MSDP_XT_SI, MSDP_YT_SI, MSDP_M_SI, MSDBO_MFO_CI)
			}
			div(row1, row2)
		})


	# OK: Model Analysis - Predictions - Tab
		OO$MA_MP_T <- renderUI({
			# glueerr("MA_P_T: '{MM()}'")
			if (length(MM()) == 0) {
				div(HTML("Please choose a model first"))
			} else {
				div(fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="PHP", height=PAHPX1(), width=PAWPX()  )))),
					fluidRow(column(2, checkboxInput(inputId="PHP_R", label="Rug", value=TRUE     )),
							column(2, checkboxInput(inputId="PHP_DL", label="Density", value=TRUE)),
							column(3, textInput(inputId="PHP_BW", label="Binwidth", value="0.1"  ))))
			}
		})

	# OK: Model Analysis - Survival Curvers - Tab
		OO$MA_SC_T <- renderUI({
			# logsn("MA_P_T: ")
			list(
				fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="SCP", height=PAHPX2(), width=PAWPX())))),
				fluidRow(column(3, selectInput(inputId="survPlotM", label="Model", choices=models)))
			)
		})

	# OK: Model Analysis - Feature Effects - Tab
	OO$MA_FE_T <- renderUI({
		# logsn("MA_FE_T: ")
		if (length(MM()) == 0) {
			div(HTML("Please choose a model first"))
		} else if (length(MM()) == 1) {
			if (length(DD()) == 0) {
				div(HTML("Please choose a dataset first"))
			} else {
				list(
					fluidRow(
						column(12, shinycssloaders::withSpinner(plotOutput(outputId="FEP", height=PAHPX2(), width=PAWPX())))
					),
					fluidRow(
						column(2, checkboxInput(inputId="FEP_DBG", label="Debug")),
						column(4, sliderInput(inputId="FEP_N", label="N Samples", value=0, min=0, max=100)),
						column(4, sliderInput(inputId="FEP_T", label="Threshold", value=0.991, min=-3, max=3, step=0.01))
					),
					fluidRow(
						column(2, selectInput(inputId="FEP_D", label="Dataset", choices=DD())),
						uiOutput({outputId="FEP_S_SI"}),
						column(2, selectInput(inputId="FEP_F", label="Feature", choices=names(PP()[[1]]))),
						uiOutput({outputId="FEP_V_SI"})
					)
				)
			}
		} else {
			div(HTML("Multiple models are not yet implemented"))
		}
	})

######################################## UI INPUT WIDGETS ##############################################################

	# OK: Plot Area Size - Inputs
		OO$PAS_W <- renderUI({
			widget1 <- list(
				selectInput(inputId="PASC", label="Plot Area Size", choices=c("Auto", "Fixed"),
							selected=if (is.null(PASC())) settings[["PASC"]] else PASC() )
			)
			widget23 <- if (all(c(!is.null(PASC()), PASC() == "Fixed", !is.null(APAW()), !is.null(APAH())))) {
				list(
					sliderInput(inputId="MPAW", label="Plot Area Width",  min=240, max=1920, step=60,
								value=isolate(MPAW()) %||% APAW()),
					sliderInput(inputId="MPAH", label="Plot Area Height", min=240, max=1920, step=60,
								value=isolate(MPAH()) %||% APAH())
				)
			}
			return(c(widget1, widget23))
		})

	# OK: Feature-Effects-Plot - Sample - Selection-Input
		OO$FEP_S_SI <- renderUI({
			if (!is.null(II$FEP_D)) {
				column(2, selectInput(inputId="FEP_S", label="Sample", choices=SS()[[II$FEP_D]]))
			}
		})

	# FIXME: Feature-Effects-Plot - Value - Slider-Input
	OO$FEP_V_SI <- renderUI({
		d <- II$FEP_D # dataset
		s <- II$FEP_S # sample
		f <- II$FEP_F # feature
		if ( none( list(d,f,s), is.null )) {
			X <- XXR[[d]]()
			cn <- colnames(X)
			rn <- rownames(X)
			if ( (!is.null(cn)) && (!is.null(rn)) && (f %in% cn) && (s %in% rn) ) {
				x_ij <- X[s, f] # value of feature f, sample s
				x__j <- X[ , f] # value of feature f over all samples
				step <- ceiling10(diff(range(x__j))) / 100
				column(4, sliderInput(inputId="FEP_V", label="Value", min=min(x__j) , max=max(x__j), step=step, value=x_ij))
			}
		}
	})

######################################## TEXT OUTPUTS ##################################################################

	# OK: App Info - Text-Output
		OO$AI_TO <- renderUI({
			div( HTML( paste0(
				"<b>App Info</b>",                                    "<br>",
				"<b>Version:</b> ",        V,                         "<br>",
				"<b>Browser Window:</b> ", BW(),  " x ", BH(), " px", "<br>",
				"<b>Main Panel:</b> ",     MPW(), " x ", MPH()," px", "<br>",
				"<b>Plot Area:</b> ",      PAW(), " x ", PAH()," px", "<br>",
				"<br>"
			) ) )
		})

	# OK: Session-Info Text-Output
		OO$SI_TO <- renderPrint({
			# logsn("SI_TO")
			catstr <- function(l, e) {
				s <- deparse(substitute(e))
				cat(glue("> {s}\n\n"))
				str(force(e), max.level=l)
				cat("\n\n")
			}
			# Runtime value of simple reactives
			catstr(1, II$FEP_D); catstr(1, II$FEP_F); catstr(1, II$FEP_S);
			catstr(1, MM()); catstr(1, DD()); catstr(1, PP()); catstr(1, XX()); catstr(1, FF()); catstr(1, SS());
			catstr(1, CXX()); catstr(1, NXX()); catstr(2, YYY())
			# Runtime value of nested reactives
			catstr(2, map(PPR, ~ .x()));  catstr(2, map(XXR, ~ .x()));
			catstr(2, map(CXXR, ~ .x())); catstr(2, map(NXXR, ~ .x()));
			catstr(2, map(YYYR, function(YYR) map(YYR, ~ .x())))
			# Reactive objects
			catstr(0, MM); catstr(0, DD); catstr(0, PP); catstr(0, XX); catstr(0, FF); catstr(0, SS)
			catstr(0, CXX); catstr(0, NXX); catstr(0, YYY); catstr(0, PPR); catstr(0, XXR)
			catstr(0, CXXR); catstr(0, NXXR); catstr(1, YYYR)
			# Input and Session object
			catstr(1, input); catstr(1, session)
		})

	# OK: Dataset-Description Text-Output
		OO$DA_DD_TO <- renderPrint({
			# logsn(glue("DA_DD_TO: {collapseCS(DD())}"))
			walk(DD(), ~ cat(.x, "\n", describe_df(XXR[[.x]]()), "\n"))
			# for (i in seq_along(dfs)) {
			#     cat(names(dfs)[[i]], "\n", sep="")
			#     describe_df(dfs[[i]])
			#     cat("\n")
			# }
		})

	# TODO: Model-Description Text-Output
	OO$MA_MD_TO <- renderPrint({
		# logsn("MA_DD_TO")
		pp <- PP()
		for (i in seq_along(pp)) {
			cat(names(pp)[[i]], "\n", sep="")
			str(pp[[i]], max.level=NA)
			cat("\n")
		}
	})

######################################## TABLE OUTPUTS #################################################################

	render_dt = function(.tbl, .visible="_all", .invisible=character()) {
		colvis <- if (.visible[[1]] == "_all") (seq_along(.tbl) - 1) else (which(colnames(.tbl) %in% .visible) - 1)
		colvis <- which(colnames(.tbl)[colvis + 1] %notin%  .invisible) - 1
		DT::renderDT(
			# args: https://cran.r-project.org/web/packages/DT/DT.pdf
			# options: https://datatables.net/reference/option/
			expr={.tbl}, server=TRUE, editable='cell', selection="single", rownames=FALSE,
			options=list(
				scrollX=FALSE, scrollY=PAHPX1_05(), scrollCollapse=TRUE, paging=FALSE,
				# sDom  = '<"top">lrt<"bottom">ipf',
				columnDefs = list(
					list(className = 'dt-left', targets = "_all"),
					list(visible = TRUE, targets = colvis),
					list(visible = FALSE, targets = "_all")
				)
			)
		)
	}
	OO$DBO_PA_TBL <- render_dt(.tbl=config$Papers,   .invisible=c("ID", "Details", "Datasets", "Models", "Summary")) # Paper    , .visible=c("Year", "Author", "Title", "Summary")
	OO$DBO_MO_TBL <- render_dt(.tbl=config$Models,   .invisible=c("ID", "Symbol", "Features", "Betas", "Modeltype")) # Models   , .visible=c("Platforms", "Methods", "Modeltype", "Symbol")
	OO$DBO_DS_TBL <- render_dt(.tbl=config$Datasets, .invisible=c("Samples", "Symbol")                             ) # Datasets , .visible=c("N", "M")
	OO$DBO_SA_TBL <- render_dt(.tbl=config$Samples                                                                 ) # Samples
	OO$DBO_DT_TBL <- render_dt(.tbl=config$Datatypes                                                               ) # Datatypes
	OO$DBO_ME_TBL <- render_dt(.tbl=config$Methods                                                                 ) # Methods
	OO$DBO_PF_TBL <- render_dt(.tbl=config$Platforms                                                               ) # Platforms
	OO$DBO_SE_TBL <- render_dt(.tbl=config$Settings                                                                ) # Settings
	observeEvent(input$DBO_PA_TBL_cell_edit, {config$Papers    <<- DT::editData(config$Papers,    input$DBO_PA_TBL_cell_edit, "DBO_PA_TBL", rownames=FALSE, resetPaging=FALSE)})
	observeEvent(input$DBO_MO_TBL_cell_edit, {config$Models    <<- DT::editData(config$Models,    input$DBO_MO_TBL_cell_edit, "DBO_MO_TBL", rownames=FALSE, resetPaging=FALSE)})
	observeEvent(input$DBO_DS_TBL_cell_edit, {config$Datasets  <<- DT::editData(config$Datasets,  input$DBO_DS_TBL_cell_edit, "DBO_DS_TBL", rownames=FALSE, resetPaging=FALSE)})
	observeEvent(input$DBO_SA_TBL_cell_edit, {config$Samples   <<- DT::editData(config$Samples,   input$DBO_SA_TBL_cell_edit, "DBO_SA_TBL", rownames=FALSE, resetPaging=FALSE)})
	observeEvent(input$DBO_DT_TBL_cell_edit, {config$Datatypes <<- DT::editData(config$Datatypes, input$DBO_DT_TBL_cell_edit, "DBO_DT_TBL", rownames=FALSE, resetPaging=FALSE)})
	observeEvent(input$DBO_ME_TBL_cell_edit, {config$Methods   <<- DT::editData(config$Methods,   input$DBO_ME_TBL_cell_edit, "DBO_ME_TBL", rownames=FALSE, resetPaging=FALSE)})
	observeEvent(input$DBO_PF_TBL_cell_edit, {config$Platforms <<- DT::editData(config$Platforms, input$DBO_PF_TBL_cell_edit, "DBO_PF_TBL", rownames=FALSE, resetPaging=FALSE)})
	observeEvent(input$DBO_SE_TBL_cell_edit, {config$Settings  <<- DT::editData(config$Settings,  input$DBO_SE_TBL_cell_edit, "DBO_SE_TBL", rownames=FALSE, resetPaging=FALSE)})

	render_dt2 = function(.name, .inputID, .keep, .collapse, .maps) {
		DT::renderDT(
			expr={
				rows_sel <- input[[paste0(.inputID, "_rows_selected")]]
				if (is.null(rows_sel)) {
					NULL
				} else {
					row <- config[[.name]][rows_sel[[1]], ]
					props <- c(.keep, .collapse, .maps)
					df <- data.frame(Property=props, Value=rep(NA, length(props))); rownames(df) <- props
					df[.keep, "Value"] <- unlist(row[.keep])
					for (e in .collapse) {
						df[e, "Value"] <- collapseCS(row[[e]][[1]])
					}
					for (m in .maps) {
						map_tbl_name <- collapse_(c("Mapping", sort(c(.name, m), decreasing=TRUE))) # Mapping_Paper_Models
						map_tbl <- config[[map_tbl_name]]
						own_col <- collapse_(c(substr(.name, 1, nchar(.name) -1 ), "ID")) # Paper_ID
						map_col <- collapse_(c(substr(m, 1, nchar(m) -1 ), "ID")) # Model_ID
						map_ids <- map_tbl[[own_col]]==row$ID
						map_vals <- map_tbl[map_ids, map_col]
						df[m, "Value"] <- collapseCS(map_vals)
					}
					df
				}
			},
			server=FALSE, editable=FALSE, selection="single", rownames=FALSE,
			options=list(scrollX=FALSE, scrollY=PAHPX1_05(), scrollCollapse=TRUE, paging=FALSE)
		)
	}

	OO$DBO_PA_TBL2 <- render_dt2(
		.name="Papers",
		.inputID="DBO_PA_TBL",
		.keep=c("ID", "Year", "Author", "Summary"),
		.collapse=c("Details"),
		.maps=c("Models", "Datasets")
	)
	OO$DBO_MO_TBL2 <- render_dt2(
		.name="Models",
		.inputID="DBO_MO_TBL",
		.keep=c("ID", "Name", "Classes", "Methods", "Datatypes", "Platforms", "Modeltype", "Symbol"),
		.collapse=c("Features", "Betas"),
		.maps=c("Papers")
	)
	OO$DBO_DS_TBL2 <- render_dt2(
		.name="Datasets",
		.inputID="DBO_DS_TBL",
		.keep=c("ID", "N", "M", "Symbol"),
		.collapse=c("Samples"),
		.maps=c("Papers")
	)
	# OO$DBO_SA_TBL2 <- render_dt2(.tbl=config$Samples  ) # Samples
	# OO$DBO_DT_TBL2 <- render_dt2(.tbl=config$Datatypes) # Datatypes
	# OO$DBO_ME_TBL2 <- render_dt2(.tbl=config$Methods  ) # Methods
	# OO$DBO_PF_TBL2 <- render_dt2(.tbl=config$Platforms) # Platforms
	# OO$DBO_SE_TBL2 <- render_dt2(.tbl=config$Settings ) # Settings

######################################## PLOTS #########################################################################


	# Helper Funcs
		AL <- list() # Container for Argument Lists passed to plot funcs

		asSVG <- function(
			.pname,
			.func,
			.arglist,
			.fname=deparse(substitute(.func))
		) {
			width <- PAW()
			height <- PAH() - 240
			outfile <- paste0(file.path(getCacheDir(), .pname), ".svg")
			argListChar <- deparse(substitute(.arglist))
			if (none(.arglist, is.null)) {
				log0n("Storing result of ", .fname, "(...) in svg of size:", width, "x", height)
				# STR(.arglist, max.level=1)
				svglite(outfile, width=width/92, height=height/92)
				tryCatch(
					do.call(.func, args=.arglist),
					error = function(cond) STR(cond),
					finally = dev.off()
				)
			} else {
				if (!exists(outfile)) {
					# Create dummy file if .arglist is invalid and no file exists yet
					# logsn(glue("{.pname}: plot(1:10)\n"))
					# STR(.arglist, appstate[["DEBUG_LEVEL"]])
					svglite(outfile, width=width/92, height=height/92)
					tryCatch(
						plot(1:10, main="DUMMY"), # FIXME: display .arglist instead
						error = function(cond) STR(cond),
						finally = dev.off()
					)
				} else {
					# Use existing file until inputs are valid again
					# logsn(glue("{.pname}: using cached file {outfile}\n"))
				}
			}
			list(src=outfile, width=width, height=height, alt=.pname)
		}

		renderSVG <- function(
			.name, # .name = (str) plot name (shown in browser as alternative name of image)
			.func, # .func = (closure) plot function
			.argfunc # .argfunc = (closure) function to return arglist required for `.func` (usually a reactive)
		) {
			.fname=deparse(substitute(.func))
			renderImage(
				expr = asSVG(.pname=.name, .func=.func, .arglist=.argfunc(), .fname=.fname),
				deleteFile=FALSE
			)
		}

	# Outputs
		OO$DBO_PA_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_PA_P"); asSVG(.pname="DBO_PA_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Paper
		OO$DBO_MO_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_MO_P"); asSVG(.pname="DBO_MO_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Models
		OO$DBO_DS_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_DS_P"); asSVG(.pname="DBO_DS_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Datasets
		OO$DBO_SA_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_SA_P"); asSVG(.pname="DBO_SA_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Samples
		OO$DBO_DT_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_DT_P"); asSVG(.pname="DBO_DT_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Datatypes
		OO$DBO_ME_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_ME_P"); asSVG(.pname="DBO_ME_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Methods
		OO$DBO_PF_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_PF_P"); asSVG(.pname="DBO_PF_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Platforms
		OO$DBO_SE_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_SE_P"); asSVG(.pname="DBO_SE_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Settings


	# MSDP ( Mean Standard Deviation Plot) (old name: Dataset Features Plot (DFP))
		AL$MSDP <- reactive({list(
			dd = DD(), xx = NXX(), xt = II$MSDP_XT, yt = II$MSDP_YT,
			ff = if ( not.none(II$MSDP_M) && II$MSDP_MFO ) FFR[[II$MSDP_M]]() else ""
		)})
		OO$MSDP <- renderSVG(.name="MSDP", .func=makeMSDPlot, .argfunc=AL$MSDP)
		# OO$MSDP <- renderImage(
		#     expr = {asSVG(.pname="MSDP", .func=makeMSDPlot, .arglist=MSDP_AL())},
		#     deleteFile=FALSE
		# )

	# FEP (Feature Effects Plot)
		AL$FEP <- reactive({
			.d <- II$FEP_D
			.s <- II$FEP_S
			.f <- II$FEP_F
			.v <- II$FEP_V
			.m <- if (not.none(MM())) MM()[[1]] else NULL
			b  <- if (not.none(.m)) PPR[[.m]]() else NULL
			X  <- if (not.none(.d)) XXR[[.d]]() else NULL
			s  <- if (not.none(.s) && not.none(X) && .s %in% rownames(X)) .s else NULL
			debug <- II$FEP_DBG
			n_extreme <- II$FEP_N
			threshold <- II$FEP_T
			list(b=b, X=X, s=s, debug=debug, n_extreme=n_extreme, threshold=threshold)
		})
		OO$FEP <- renderSVG(.name="FEP", .func=plot_feature_effects, .argfunc=AL$FEP)
		# OO$FEP <- renderImage(
		#     expr = asSVG(.pname="FEP", .func=plot_feature_effects, .arglist=AL$FEP()),
		#     deleteFile=FALSE
		# )

	# PHP (Predictions Histogram Plot)
		OO$PHP <- renderImage(
			expr = {
				.arglist <- list(
					predictions=YYY()[[MM()[[1]]]],
					density_lines=II$PHP_DL,
					rug=II$PHP_R,
					binwidth=as.numeric(II$PHP_BW)
				)
				asSVG(.pname="PHP", .func=plot_predictions_histogram, .arglist=.arglist)
			},
			deleteFile=FALSE
		)

	# SCP (Survival Curves Plot)
		OO$SCP <- renderImage(
			expr = {
				.arglist <- list(x=1:10, main="dummy")
				asSVG(.pname="SCP", .func=plot, .arglist=.arglist)
			},
			deleteFile=FALSE
		)

}
