imluiServer <- function(input, output, session) {
########## INITIALIZATION ##############################################################################################
	########## Print startup message ###################################################################################
		logsne("Starting imluiServer from process ID:", Sys.getpid(), "...")
	########## Global reactive values ##################################################################################
		II <- input
		OO <- output
		RV <- reactiveValues(
			oauth2_flow_started=FALSE,
			redirected_to=NULL,
			user=shiny::reactiveValues(
				is_authenticated=FALSE,
				id=NULL,
				cookie_already_checked=FALSE
			)
		)
	########## Reread imlui_config.yml (allows for live change of DB) ##################################################
		read_imlui_config(create_from_template_if_missing = FALSE)
	########## Connect to database and fetch all tables ################################################################
		db_conn <- connect_imlui_db()
		# db <- reactiveValues()
		db <- list()
		logsne("Reading tables...")
		for (tbl in globals$TABLES) {
			db[[tbl]] <- DBI::dbReadTable(db_conn, tbl)
		}
		rownames(db$users) <- db$users$user_id
		# logsne("Creating table observers...")
		# tbl_observers <- list()
		# for (tbl in globals$TABLES) {
		# 	tbl_observers[[tbl]] <- local({
		# 		tbl <- tbl # copy in local scope (to prevent observer from using the outer `tbl` object)
		# 		observeEvent(db$Appstate, {logsne("Table", tbl, "was changed")}, ignoreInit=TRUE)
		# 	})
		# }

	########## Define database helper functions ########################################################################
		db_send <- function(...) {
			res <- DBI::dbExecute(db_conn, ...)
			# DBI::dbClearResult(res)
			return(res)
		}
		db_get_valid_cookies <- function(expiry=7) {
			# This function must return a data.frame with columns user and sessionid. Other
			# columns are also okay and will be made available to the app after log in as
			# columns in credentials()$user_auth
			# TODO: Based on shinyauthr example (cite!)
			db$mapping_users_sessions %>%
				dplyr::mutate(login_time = lubridate::ymd_hms(login_time)) %>%
				tibble::as_tibble() %>%
				dplyr::filter(login_time > lubridate::now() - lubridate::days(expiry))
		}
		db_insert_cookie <- function(user_id, session_id) {
			# This function must accept two parameters: user and sessionid. It will be
			# called whenever the user successfully logs in with a password. This function
			# saves to your database.
			# TODO: Based on shinyauthr example (cite!)
			tibble::tibble(
				user_id=user_id,
				session_id=session_id,
				login_time=as.character(now())
			) %>%
			DBI::dbWriteTable(db_conn, "mapping_users_sessions", ., append=TRUE)
		}
		set_login_cookie <- function(user_id, session_id) {
			shinyjs::js$setcookie(session_id)
			db_insert_cookie(user_id, session_id)
		}

	########## Define static url symbols ###############################################################################
		logsne("Parsing URL ...")
		url_search <- isolate(session$clientData$url_search)
		url_protocol <- isolate(session$clientData$url_protocol) # examples: "http:", "https:"
		url_hostname <- isolate(session$clientData$url_hostname) # examples: "localhost", "something.spang-lab.de"
		url_port <- isolate(session$clientData$url_port) # examples: 443, 80, 99 (https-default, http-default, custom)
		url_pathname <- isolate(session$clientData$url_pathname) # examples: "/", "/shinyserver/imlui"
		url_params <- parseQueryString(url_search)
		url <- paste0(url_protocol, "//", url_hostname, ":", url_port, url_pathname, url_search)
		logsne("\tURL:", url)
		github_redirect_url <- paste0(url_protocol, "//", url_hostname, ":", url_port, url_pathname)
		shinygithub_oauth_app <- httr::oauth_app(
			appname="shinygithub", # TODO: take from db$settings
			key="51d46f96810d1fd182a2", # TODO: take from db$settings
			secret="66eec8782825eeb61007dbef32f91afc9c3587aa", # TODO: take from db$settings
			redirect_uri=github_redirect_url
		)
	########## Check if coming back from Github Auth ###################################################################
		observeEvent(url_params, once=TRUE, { # use `observe()` so we may write reactive Values
			logsne("Checking if coming back from Github Auth...")
			reload_on_logout = TRUE
			if (is.null(url_params$code)) {
				logsne("\tNope, we don't...")
			} else {
				logsne("\tYes, we do. Fetching user info...")
				github_oauth2_access_token <- httr::oauth2.0_access_token(
					endpoint=httr::oauth_endpoints("github"),
					app=shinygithub_oauth_app,
					code=url_params$code
					# returns: list(access_token="asdf", scope="", token_type="bearer")
				)
				github_oauth2_token <- httr::oauth2.0_token(
					app=shinygithub_oauth_app,
					endpoint=httr::oauth_endpoints("github"),
					credentials=github_oauth2_access_token,
					cache=FALSE
				)
				resp <- httr::GET("https://api.github.com/user", httr::config(token=github_oauth2_token))
				body <- httr::content(resp)
				# List of: 32
				# Colnames: login, id, node_id, avatar_url, gravatar_id, url, html_url, followers_url, following_url,
				#           gists_url, starred_url, subscriptions_url, organizations_url, repos_url, events_url,
				#           received_events_url, type, site_admin, name, company, blog, location, email, hireable, bio,
				#           twitter_username, public_repos, public_gists, followers, following, created_at, updated_at
				# Important: login:      "toscm"
				#            id:         "12760468"
				#			 node_id:    "MDQ6VXNlcjEyNzYwNDY4"
				#			 login:      "toscm"
				#            avatar_url: "https://avatars.githubusercontent.com/u/12760468?v=4"
				#            name:       "Tobias Schmidt"
				#            email:      NULL
				if (httr::status_code(resp) == 200) {
					logsne("Github login successful:")
					logsne("\tlogin", body$login %||% "NULL")
					logsne("\tid:", body$id %||% "NULL")
					logsne("\tnode_id:", body$node_id %||% "NULL")
					logsne("\tlogin:", body$login %||% "NULL")
					logsne("\tavatar_url:", body$avatar_url %||% "NULL")
					logsne("\tname:", body$name %||% "NULL")
					logsne("\temail:", body$email %||% "NULL")
					if (!(body$id %in% db$users$github_id)) { # new user -> create new entry in DB
						user <- list(
							user_id = paste0("github_user_", body$id),
							group_ids = "standard",
							display_name = body$name,
							github_id = body$id,
							avatar_url = body$avatar_url
						)
						db_send(
							"INSERT INTO users
							(user_id, group_ids, display_name, github_id, avatar_url)
							VALUES ($1, $2, $3, $4, $5)",
							params=unname(user)
						)
						db$users[user$user_id, names(user)] <- user
					}
					RV$user$id <- db$users$user_id[which(db$users$github_id == body$id)] # use which to get rid of NAs
					RV$user$is_authenticated <- TRUE # TODO: remove this. If user$id is NULL, we're not authenticated...
					x <- randomString()
					set_login_cookie(user_id=RV$user$id, session_id=x)
				} else {
					shinyjs::show(id="login_error")
				}
			}
		})
	########## Read init values from database ##########################################################################
		logsne("Generating init values from database...")
		settings <- isolate({x <- db$Settings$Value; names(x) <- db$Settings$ID; x})
	########## Configure Bookmark excludes #############################################################################
		models <- dplyr::filter(db$Models, Symbol != "")$Symbol
		model_names <- dplyr::filter(db$Models, Symbol != "")$Name
		dataset_names <- dplyr::filter(db$Datasets, Symbol != "")$ID
		logsne("Configuring Bookmark excludes...")
		setBookmarkExclude(c(
			"dim",
			"login_button",
			"login_button_github",
			"login_button_google",
			"login_button_auth_spang_lab",
			"login_button_gitlab_spang_lab",
			"login_button_gitlab",
			"login_user_name",
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
			"login_jscookie",
			"DBO_SE_TBL_state",
			"logout_button"
		))


	########## Define Reactives ########################################################################################

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

		MM   <- r({ if (!is.null(II$MM)) db$Models$Symbol[match(II$MM, db$Models$Name)] else list() })  # MM = Models = list(char)

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




########## OBSERVERS ###################################################################################################
	########## On logout_button click ##################################################################################
		# remove cookie and reload session
		shiny::observeEvent(
			input$logout_button,
			{
				shinyjs::js$rmcookie()
				session$reload()
			}
		)

	########## On change of authentication state: restore previuous appstate ###########################################
		# show/hide login panel
		shiny::observe({ if (RV$user$is_authenticated) { # Update query string
			x <- db$Appstate
			last_url <- x[x$user_id == RV$user$id & x$resource_id == "url", "resource_value"]
			logsne("Event `RV$user$is_authenticated` triggered ...")
			logsne("\tconnected user:", dput2(RV$user$id))
			logsne("\tlast url:", dput2(last_url))
			logsne("\tcurrent url:", dput2(url)) # this value is changed through the bookmarking stuff!
			logsne("\tsession start url params:", dput2(url_params)) # this is the true URL obtained from the browser
			logsne("\tcurrent url search:", dput2(session$clientData$url_search)) # true URL obtained from browser
			if (grepl("&redirect=false", url_search)) {
				logsne("\tURL parameter `redirect=false` is present, so do no more redirections")
			} else if (length(last_url) == 0) {
				logsne("\tNo last URL stored for user", RV$user$id, "so nothing to restore ...")
			} else {
				if (length(last_url) > 1) {
					logsne("\tWarning: multiple URLs found. Using the last one...")
				}
				new_url <- paste0(last_url[length(last_url)], "&redirect=false")
				logsne("\tRestoring previous appstate by redirecting to url:", new_url)
				OO$web_app <- redirection_to(new_url)
			}
		} })

	########## When javascript is ready ################################################################################
		# retrieve cookie
		shiny::observeEvent(
			shiny::isTruthy(shinyjs::js$getcookie()), {
				logsne("Javascript is ready. Calling `shinyjs::js$getcookie()` to set `input$login_jscookie` ...")
				shinyjs::js$getcookie()
			}
		)

	########## When valid cookie is present ############################################################################
		# update expiry date
		shiny::observeEvent(
			input$login_jscookie, {
				logsne("Variable `Input$login_jscookie` is available, starting authentication...")
				if (RV$user$is_authenticated) {
					logsne("\tNothing to do. Already authenticated.")
				} else if (is.null(input$login_jscookie) || nchar(input$login_jscookie) == 0) {
					logsne("\tValue of `input$login_jscookie` is NULL or '', stopping authentication.")
				} else {
					db_cookie_data <- {x <- db_get_valid_cookies(); x[x$session_id == input$login_jscookie, ]}
					if (nrow(db_cookie_data) != 1) {
						# Invalid valid cookie found (or multiple, which should never occur), so remove it
						logsne("\tCookie invalid, removing it...")
						shinyjs::js$rmcookie()
						logsne("\tSome more logging info about the cookie:")
						logsne("\t\tinput$login_jscookie:", input$login_jscookie)
						logsne("\t\tRV$user$is_authenticated:", RV$user$is_authenticated)
						logsne("\t\t!is.null(input$login_jscookie):", !is.null(input$login_jscookie))
						logsne("\t\tnchar(input$login_jscookie) > 0:", nchar(input$login_jscookie) > 0)
						logsne("\t\tdput(db_cookie_data):", . <- dput2(db_cookie_data))
					} else {
						# Valid cookie found, so get corresponding user_id, set reactives values user$id and
						# user$is_authenticated and then regenerate cookie to update expiry date
						RV$user$id <- db_cookie_data$user_id
						RV$user$is_authenticated <- TRUE
						set_login_cookie(user_id = RV$user$id, session_id = randomString())
						logsne("\tCookie valid, authenticated successfully as", RV$user$id, "...")
					}
				}
			}
		)

	########## When login button is clicked ############################################################################
		# check user/pass and set cookie
		shiny::observeEvent(
			input$login_button, {
				logsne("login button clicked:", input$login_button)
				users_idx <- which(db$users$user_id==input$login_user_name)
				if (input$login_user_name %in% db$users$user_id) {
					user_exists <- TRUE
					user_password <- db$users$password[db$users$user_id==input$login_user_name]
					if (globals$SODIUM_HASHED) {
						password_matches <- sodium::password_verify(user_password, input$login_password)
					} else {
						password_matches <- identical(user_password, input$login_password)
					}
				} else {
					user_exists <- FALSE
				}
				if (user_exists && password_matches) {
					RV$user$is_authenticated <- TRUE
					RV$user$id <- input$login_user_name
					set_login_cookie(user_id=RV$user$id, session_id = randomString())
					logsne("Authenticated successfully as", RV$user$id)
				} else {
					# if not valid temporarily show error logsne to user
					shinyjs::toggle(id = "login_error", anim = TRUE, time = 1, animType = "fade")
					shinyjs::delay(5000, shinyjs::toggle(id = "login_error", anim = TRUE, time = 1, animType = "fade"))
				}
			}
		)

	########## When login_github button is clicked #####################################################################
		# start OAuth2.0 authentication, i.e. redirect to
		# <https://github.com/login/oauth/{authorize/access_token}>
		shiny::observeEvent(
			eventExpr = input$login_button_github,
			handlerExpr = {
				logsne("input$login_button_github clicked:", input$login_button_github)
				url <- httr::oauth2.0_authorize_url(
					endpoint = httr::oauth_endpoints("github"),
					app = shinygithub_oauth_app,
					scope = ""
				)
				OO$web_app <- redirection_to(url)
				RV$oauth2_flow_started <- TRUE # TODO: remove if not used
				RV$redirected_to <- "Github" # TODO: remove if not used
			}
		)

	########## On change of url: print url parts #######################################################################
		# print URL details for logging
		observeEvent(session$clientData, {
			logsne("session$clientData changed:")
			logsne("url_protocol:", session$clientData$url_protocol)
			logsne("url_hostname:", session$clientData$url_hostname)
			logsne("url_port:", session$clientData$url_port)
			logsne("url_pathname:", session$clientData$url_pathname)
			logsne("url_search:", session$clientData$url_search)
			logsne("url_hash_initial:", session$clientData$url_hash_initial)
			logsne("url_hash:", session$clientData$url_hash)
			logsne("output_web_app_width:", session$clientData$output_web_app_width)
			logsne("output_web_app_height:", session$clientData$output_web_app_height)
		}, ignoreInit=TRUE)

	########## On any input change #####################################################################################
		# update the query string
		# (based on <https://mastering-shiny.org/action-bookmark.html#action-bookmark>)
		observe({
			reactiveValuesToList(input) #1
			#1 Not quite sure why this is necessary. Maybe to trigger this on any input change, however if this is the
			# case, converting to all reactive values to a list seems a bit costly in terms of runtime. Maybe improve
			# later.
			session$doBookmark() # triggers onBookmark and onBookmarked callback functions
		})
		onBookmarked(function(url) {
			# logsne("Bookmarking url:", url)
			updateQueryString(queryString=url)
			url <<- url
		}) # Update the query string

########## UI RENDERES #################################################################################################
	########## Webapp ##################################################################################################
		OO$web_app <- renderUI({
			if (RV$user$is_authenticated) {
				div(
					sidebarLayout(
						sidebarPanel(
							width=3,
							style="-ms-flex: 0 0 230px; flex: 0 0 230px; background-color: greenyellow;",
							# pickerInput("Paper", "Paper", choices=names(db$Papers), multiple=TRUE),
							# pickerInput("Method", "Method", choices=model_names, multiple=TRUE),
							pickerInput("MM", "Models", choices=model_names, multiple=TRUE),
							pickerInput("DD", "Datasets", choices=datasets, multiple=TRUE),
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
								selected = "Model Analysis",
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
	########## Main Panel (layer 1 tabs) ###############################################################################
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
	########## DBO Panel (layer 2 tabs) ################################################################################
		OO$DBO_PA_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_PA_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_PA_TBL2") ) ) ) }) # Paper
		OO$DBO_MO_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_MO_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_MO_TBL2") ) ) ) }) # Models
		OO$DBO_DS_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_DS_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_DS_TBL2") ) ) ) }) # Datasets
		OO$DBO_SA_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_SA_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_SA_TBL2") ) ) ) }) # Samples
		OO$DBO_DT_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_DT_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_DT_TBL2") ) ) ) }) # Datatypes
		OO$DBO_ME_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_ME_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_ME_TBL2") ) ) ) }) # Methods
		OO$DBO_PF_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_PF_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_PF_TBL2") ) ) ) }) # Platforms
		OO$DBO_SE_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("DBO_SE_TBL") ) ), fluidRow( column( 12, DT::DTOutput("DBO_SE_TBL2") ) ) ) }) # Settings
	########## MA Panel (layer 2 tabs) #################################################################################
		# OK: Model Analysis - Survival Curves - Tab
		OO$MA_SC_T <- renderUI({
			# logsne("MA_P_T: ")
			list(
				fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="SCP", height=PAHPX2(), width=PAWPX())))),
				fluidRow(column(3, selectInput(inputId="survPlotM", label="Model", choices=models)))
			)
		})
		# OK: Model Analysis - Predictions - Tab
		OO$MA_MP_T <- renderUI({
			# glueerr("MA_P_T: '{MM()}'")
			if (length(MM()) == 0) {
				div(HTML("Please choose a dataset and model first"))
			} else if (length(DD()) == 0) {
				div(HTML("Please choose a dataset and model first"))
			} else {
				div(fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="PHP", height=PAHPX1(), width=PAWPX()  )))),
					fluidRow(column(2, checkboxInput(inputId="PHP_R", label="Rug", value=TRUE     )),
							column(2, checkboxInput(inputId="PHP_DL", label="Density", value=TRUE)),
							column(3, textInput(inputId="PHP_BW", label="Binwidth", value="0.1"  ))))
			}
		})
		# OK: Model Analysis - Survival Curvers - Tab
		OO$MA_SC_T <- renderUI({
			# logsne("MA_P_T: ")
			list(
				fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="SCP", height=PAHPX2(), width=PAWPX())))),
				fluidRow(column(3, selectInput(inputId="survPlotM", label="Model", choices=models)))
			)
		})
		# OK: Model Analysis - Feature Effects - Tab
		OO$MA_FE_T <- renderUI({
			# logsne("MA_FE_T: ")
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
	########## DA Panel (layer 2 tabs) #################################################################################
		# TODO: Dataset Analysis - MSD-Plot - Tab
		OO$DA_MSDP_T <- renderUI({
			# Vars
			xtc <- ytc <- c("identity", "log2", "log10")
			xt <- isolate(II$MSDP_XT) %||% "identity"
			yt <- isolate(II$MSDP_YT) %||% "identity"
			mm <- MM()
			# User info
			# logsne("DA_MSDP_T: ")
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

########## INPUT WIDGETS ###############################################################################################
	########## OK: Plot Area Size - Inputs #############################################################################
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

	########## OK: Feature-Effects-Plot - Sample - Selection-Input #####################################################
		OO$FEP_S_SI <- renderUI({
			if (!is.null(II$FEP_D)) {
				column(2, selectInput(inputId="FEP_S", label="Sample", choices=SS()[[II$FEP_D]]))
			}
		})

	########## FIXME: Feature-Effects-Plot - Value - Slider-Input ######################################################
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

########## TEXT OUTPUTS ################################################################################################
	########## OK: App Info - Text-Output ##############################################################################
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

	########## OK: Session-Info Text-Output ############################################################################
		OO$SI_TO <- renderPrint({
			# logsne("SI_TO")
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

	########## OK: Dataset-Description Text-Output #####################################################################
		OO$DA_DD_TO <- renderPrint({
			# logsne(glue("DA_DD_TO: {collapseCS(DD())}"))
			walk(DD(), ~ cat(.x, "\n", describe_df(XXR[[.x]]()), "\n"))
			# for (i in seq_along(dfs)) {
			#     cat(names(dfs)[[i]], "\n", sep="")
			#     describe_df(dfs[[i]])
			#     cat("\n")
			# }
		})

	########## TODO: Model-Description Text-Output #####################################################################
		OO$MA_MD_TO <- renderPrint({
			# logsne("MA_DD_TO")
			pp <- PP()
			for (i in seq_along(pp)) {
				cat(names(pp)[[i]], "\n", sep="")
				str(pp[[i]], max.level=NA)
				cat("\n")
			}
		})
########## TABLE OUTPUTS ###############################################################################################
	########## Helpers #################################################################################################
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
		render_dt2 = function(.name, .inputID, .keep, .collapse, .maps) {
			DT::renderDT(
				expr={
					rows_sel <- input[[paste0(.inputID, "_rows_selected")]]
					if (is.null(rows_sel)) {
						NULL
					} else {
						row <- db[[.name]][rows_sel[[1]], ]
						props <- c(.keep, .collapse, .maps)
						df <- data.frame(Property=props, Value=rep(NA, length(props))); rownames(df) <- props
						df[.keep, "Value"] <- unlist(row[.keep])
						for (e in .collapse) {
							df[e, "Value"] <- collapseCS(row[[e]][[1]])
						}
						for (m in .maps) {
							map_tbl_name <- collapse_(c("Mapping", sort(c(.name, m), decreasing=TRUE))) # Mapping_Paper_Models
							map_tbl <- db[[map_tbl_name]]
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
	########## Overview Tables (Top) ###################################################################################
		OO$DBO_PA_TBL <- render_dt(.tbl=db$Papers,   .invisible=c("ID", "Details", "Datasets", "Models", "Summary")) # Paper    , .visible=c("Year", "Author", "Title", "Summary")
		OO$DBO_MO_TBL <- render_dt(.tbl=db$Models,   .invisible=c("ID", "Symbol", "Features", "Betas", "Modeltype")) # Models   , .visible=c("Platforms", "Methods", "Modeltype", "Symbol")
		OO$DBO_DS_TBL <- render_dt(.tbl=db$Datasets, .invisible=c("Samples", "Symbol")                             ) # Datasets , .visible=c("N", "M")
		OO$DBO_SA_TBL <- render_dt(.tbl=db$Samples                                                                 ) # Samples
		OO$DBO_DT_TBL <- render_dt(.tbl=db$Datatypes                                                               ) # Datatypes
		OO$DBO_ME_TBL <- render_dt(.tbl=db$Methods                                                                 ) # Methods
		OO$DBO_PF_TBL <- render_dt(.tbl=db$Platforms                                                               ) # Platforms
		OO$DBO_SE_TBL <- render_dt(.tbl=db$Settings                                                                ) # Settings
	########## Detail Tables (Top) #####################################################################################
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
		# OO$DBO_SA_TBL2 <- render_dt2(.tbl=db$Samples  ) # Samples
		# OO$DBO_DT_TBL2 <- render_dt2(.tbl=db$Datatypes) # Datatypes
		# OO$DBO_ME_TBL2 <- render_dt2(.tbl=db$Methods  ) # Methods
		# OO$DBO_PF_TBL2 <- render_dt2(.tbl=db$Platforms) # Platforms
		# OO$DBO_SE_TBL2 <- render_dt2(.tbl=db$Settings ) # Settings

	########## Editing Observers #######################################################################################
		observeEvent(input$DBO_PA_TBL_cell_edit, {db$Papers    <<- DT::editData(db$Papers,    input$DBO_PA_TBL_cell_edit, "DBO_PA_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$DBO_MO_TBL_cell_edit, {db$Models    <<- DT::editData(db$Models,    input$DBO_MO_TBL_cell_edit, "DBO_MO_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$DBO_DS_TBL_cell_edit, {db$Datasets  <<- DT::editData(db$Datasets,  input$DBO_DS_TBL_cell_edit, "DBO_DS_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$DBO_SA_TBL_cell_edit, {db$Samples   <<- DT::editData(db$Samples,   input$DBO_SA_TBL_cell_edit, "DBO_SA_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$DBO_DT_TBL_cell_edit, {db$Datatypes <<- DT::editData(db$Datatypes, input$DBO_DT_TBL_cell_edit, "DBO_DT_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$DBO_ME_TBL_cell_edit, {db$Methods   <<- DT::editData(db$Methods,   input$DBO_ME_TBL_cell_edit, "DBO_ME_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$DBO_PF_TBL_cell_edit, {db$Platforms <<- DT::editData(db$Platforms, input$DBO_PF_TBL_cell_edit, "DBO_PF_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$DBO_SE_TBL_cell_edit, {db$Settings  <<- DT::editData(db$Settings,  input$DBO_SE_TBL_cell_edit, "DBO_SE_TBL", rownames=FALSE, resetPaging=FALSE)})





########## PLOT OUTPUTS ################################################################################################
	########## Helper Funcs ############################################################################################
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
				log0ne("Storing result of ", .fname, "(...) in svg of size:", width, "x", height)
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
					# logsne(glue("{.pname}: plot(1:10)\n"))
					# STR(.arglist, appstate[["DEBUG_LEVEL"]])
					svglite(outfile, width=width/92, height=height/92)
					tryCatch(
						plot(1:10, main="DUMMY"), # FIXME: display .arglist instead
						error = function(cond) STR(cond),
						finally = dev.off()
					)
				} else {
					# Use existing file until inputs are valid again
					# logsne(glue("{.pname}: using cached file {outfile}\n"))
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

	########## Outputs #################################################################################################
		OO$DBO_PA_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_PA_P"); asSVG(.pname="DBO_PA_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Paper
		OO$DBO_MO_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_MO_P"); asSVG(.pname="DBO_MO_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Models
		OO$DBO_DS_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_DS_P"); asSVG(.pname="DBO_DS_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Datasets
		OO$DBO_SA_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_SA_P"); asSVG(.pname="DBO_SA_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Samples
		OO$DBO_DT_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_DT_P"); asSVG(.pname="DBO_DT_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Datatypes
		OO$DBO_ME_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_ME_P"); asSVG(.pname="DBO_ME_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Methods
		OO$DBO_PF_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_PF_P"); asSVG(.pname="DBO_PF_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Platforms
		OO$DBO_SE_P <- renderImage(expr={.arglist <- list(x=1:10, main="DBO_SE_P"); asSVG(.pname="DBO_SE_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Settings


	########## MSDP ( Mean Standard Deviation Plot) (old name: Dataset Features Plot (DFP)) ############################
		AL$MSDP <- reactive({list(
			dd = DD(), xx = NXX(), xt = II$MSDP_XT, yt = II$MSDP_YT,
			ff = if ( not.none(II$MSDP_M) && II$MSDP_MFO ) FFR[[II$MSDP_M]]() else ""
		)})
		OO$MSDP <- renderSVG(.name="MSDP", .func=makeMSDPlot, .argfunc=AL$MSDP)
		# OO$MSDP <- renderImage(
		#     expr = {asSVG(.pname="MSDP", .func=makeMSDPlot, .arglist=MSDP_AL())},
		#     deleteFile=FALSE
		# )

	########## FEP (Feature Effects Plot) ##############################################################################
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

	########## PHP (Predictions Histogram Plot) ########################################################################
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

	########## SCP (Survival Curves Plot) ##############################################################################
		OO$SCP <- renderImage(
			expr = {
				.arglist <- list(x=1:10, main="dummy")
				asSVG(.pname="SCP", .func=plot, .arglist=.arglist)
			},
			deleteFile=FALSE
		)

########## ON FLUSH OBSERVERS ##########################################################################################
	# onFlush: do nothing
		onFlush(
			function() {
				# logsne("Event onFlush triggered: no callback registered")
			},
			once = FALSE,
			session = getDefaultReactiveDomain()
		)
	# onFlushed: do nothing
		onFlushed( # Do nothing
			function() {
				# logsne("Event onFlushed() triggered: no callback registered")
			},
			once = FALSE,
			session = getDefaultReactiveDomain()
		)
	
########## ON STOP OBSERVERS ###########################################################################################
	########## onStop: Store appstate in form of URL (called before onSessionEnded) ####################################
		onStop(
			function() {
				logsne("Event onStop triggered in server: storing appstate as URL in DB...")
				user_id <- isolate(RV$user$id)
				if (!is.null(user_id)) {
					logsne("\tuser_id:", user_id)
					logsne("\tresource_id:", "url")
					logsne("\tresource_value:", url)
					idx <- which(db$Appstate$user_id == user_id && db$Appstate$resource_id == "url")
					if (length(idx) == 0) {
						db_send("INSERT INTO Appstate
								(user_id, resource_id, resource_value)
								VALUES (?, ?, ?)",
								params=list(user_id, "url", url)
						)
					} else {
						db_send("UPDATE Appstate
								SET resource_value = ?
								WHERE user_id = ? AND resource_id = ?",
								params=list(url, user_id, "url"))
					}
				} else {
					logsne("\tSkipped, no user logged in.")
				}
			},
			session = getDefaultReactiveDomain()
		)
	########## onSessionEnded: Close DB connection #####################################################################
		onSessionEnded(
			function() {
				logsne("Event onSessionEnded() triggered: disconnecting DB")
				DBI::dbDisconnect(db_conn)
			},
			session = getDefaultReactiveDomain()
		)
########## FUNCTION END ################################################################################################
}
