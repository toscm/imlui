server__init_data <- function(input, output, session) {
	data <- rlang::env(
		input = input,
		output = output,
		session = session,
		config = util__read_imlui_config(),
		database = db__database(config),
		appstate = server__init_appstate(),
		reactives = server__init_reactives(input),
		statics = server__init_statics(session)
	)
	return(data)
}

server__init_appstate <- function() {
	reactiveValues(
		oauth2_flow_started=FALSE,
		redirected_to=NULL,
		user=shiny::reactiveValues(
			is_authenticated=FALSE,
			id=NULL,
			cookie_already_checked=FALSE
		)
	)
}

server__init_reactives <- function(data) {
	# Helpers
	# Duplicated letters like MM indicate lists, so you can do `for (M in MM)`
	# Tripled letters like YYY indicate list of lists `for (M in MM) {for (D in DD) {YYY[[M]][[D]]}}`
	r  <- reactive
	l  <- clapply
	database <- data$database

	# User Data
	logsne("Initializing User Data Reactives")
	USER <- r({ database$users[rv$user$id, ] }) # 1x9 or 0x9 data.frame
	USER_ID <- r({ USER()$user_id }) # character vector of length 0 or 1
	GROUP_IDS <- r({ USER()$group_ids }) # character vector of length 0 or 1
	DISPLAY_NAME <- r({ USER()$group_ids }) # character vector of length 0 or 1
	GITHUB_ID <- r({ USER()$user_id }) # character vector of length 0 or 1
	AVATAT_URL <- r({ USER()$avatat_url }) # character vector of length 0 or 1
	PASSWORD <- r({ USER()$password }) # character vector of length 0 or 1
	GITLAB_ID <- r({ USER()$gitlab_id }) # character vector of length 0 or 1
	GOOGLE_ID <- r({ USER()$google_id }) # character vector of length 0 or 1
	SPANG_LAB_GITLAB_ID <- r({ USER()$spang_lab_gitlab_id }) # character vector of length 0 or 1
	SPANG_LAB_AUTH_ID <- r({ USER()$spang_lab_auth_id }) # character vector of length 0 or 1

	# Model Metadata
	logsne("Initializing Model Metadata Reactives")
	MODEL_IDS <- r({
		if (length(GROUP_IDS()) != 1) {
			ids <- c()
		} else if (grepl("admin", GROUP_IDS())) {
			ids <- database$Models$ID[(database$Models$Symbol != "") & (!is.na(database$Models$Symbol))]
		} else {
			mgm <- database$mapping_groups_models
			ids1 <- mgm$model_id[stringr::str_detect(GROUP_IDS(), pattern=mgm$group_id)]
			mum <- database$mapping_users_models
			ids2 <- mum$model_id[mum$user_id == USER_ID()]
			ids <- unique(c(ids1, ids2))
		}
		logsne("Reactive `MODEL_IDS` requested:", dput2(ids))
		ids
	})
	MODEL_SYMBOLS <- r({ database$Models[MODEL_IDS(), "Symbol"] })
	MODEL_NAMES <- r({ x <- database$Models[MODEL_IDS(), "Name"]; names(x) <- MODEL_SYMBOLS(); x})
	MODEL_PKGS <- r({ x <- database$Models[MODEL_IDS(), "Package"]; names(x) <- MODEL_SYMBOLS(); x})

	# Dataset Metadata
	logsne("Initializing Dataset Metadata Reactives")
	DATASET_IDS <- r({
		if (length(GROUP_IDS()) != 1) {
			ids <- c()
		} else if (grepl("admin", GROUP_IDS())) {
			ids <- database$Datasets$ID[database$Datasets$Symbol != ""]
		} else {
			mgd <- database$mapping_groups_datasets
			ids1 <- mgd$dataset_id[stringr::str_detect(GROUP_IDS(), pattern=mgd$group_id)]
			mud <- database$mapping_users_datasets
			ids2 <- mud$dataset_id[mud$user_id == USER_ID()]
			ids <- unique(c(ids1, ids2))
		}
		logsne("Reactive `DATASET_IDS` requested:", dput2(ids))
		ids
	})
	DATASET_SYMBOLS <- r({ database$Datasets[DATASET_IDS(), "Symbol"] })
	DATASET_NAMES <- r({ x <- database$Datasets[DATASET_IDS(), "Name"]; names(x) <- DATASET_SYMBOLS(); x })
	DATASET_PKGS <- r({ x <- database$Datasets[DATASET_IDS(), "Package"]; names(x) <- DATASET_SYMBOLS(); x })
	DATASET_TRANSPOSE <- r({
		x <- as.logical(database$Datasets[DATASET_IDS(), "Transpose"]); names(x) <- DATASET_SYMBOLS(); x
	})

	# Browser / Plot Dimensions (alphabetically)
	logsne("Initializing Browser / Plot Dimensions reactives ...")
	APAH      <- r({ PAHS[max(which(PAHS < (MPH() - 120)), 1)] }) # APAH = Automatic Plot Area Height = num
	APAW      <- r({ PAWS[max(which(PAWS < (MPW() -   0)), 1)] }) # APAW = Automatic Plot Area Width = num
	BH        <- r({ if (is.null(input$dim[2])) 480 else input$dim[2] })  # BH = Browser Height = num
	BW        <- r({ if (is.null(input$dim[1])) 640 else input$dim[1] })  # BW = Browser Width = num
	MPAH      <- r({ input$MPAH })  # MPAH = Manual Plot Area Height = num
	MPAW      <- r({ input$MPAW })  # MPAW = Manual Plot Area Width = num
	MPH       <- r({ BH() -  42.00 }) # MPH = Main Panel Height = num
	MPW       <- r({ BW() *   0.75 }) # MPW = Main Panel Width = num
	PAH       <- r({ if ( PASC() == "Fixed" && !(is.null(MPAH())) ) MPAH() else APAH() }) # PAH = PAHeight = num
	PAHPX0    <- r({ paste0(PAH() -   0, "px") }) # PAH = Plot Area Height in Pixels = num
	PAHPX1    <- r({ paste0(PAH() - 120, "px") }) # PAH = Plot Area Height in Pixels = num
	PAHPX1_05 <- r({ paste0((PAH() - 120) * 0.5, "px") }) # PAH = Plot Area Height in Pixels = num
	PAHPX2    <- r({ paste0(PAH() - 240, "px") }) # PAH = Plot Area Height in Pixels = num
	PAHPX3    <- r({ paste0(PAH() - 360, "px") }) # PAH = Plot Area Height in Pixels = num
	PASC      <- r({ input$PASC })  # PASC = Plot Area Size Calculation = char
	PAW       <- r({ if ( PASC() == "Fixed" && !(is.null(MPAW())) ) MPAW() else APAW() }) # PAW = PAWidth = num
	PAWPX     <- r({ paste0(PAW(), "px") }) # PAW = Plot Area Width in Pixels = num

	# Model Reactives (one reactive per model)
	logsne("Initializing Model Reactives (one reactive per model) ...")
	# FFR: Features[model] = list(r(vector(char)))
	# PPR: Parameters[model] = list(r(num/obj))
	FFR <- l( database$Models$Symbol, function(s) { r({ names(PPR[[s]]()) }) } )
	PPR <- l( database$Models$Symbol, function(s) { r({ getdata(sym=s, typ="model", pkg=MODEL_PKGS()[s]) }) } )

	# Dataset Reactives (one reactive per dataset)
	logsne("Initializing Dataset Reactives (one reactive per dataset) ...")
	# XXR.: Covariate_Dataframe[dataset] = list(r(data.frame))
	# XXR : Covariate_Dataframe[dataset] = list(r(data.frame))
	# CXXR: Categorical_Covariate_Dataframe[dataset] = list(r(data.frame))
	# NXXR: Numerical_Covariate_Dataframe[dataset] = list(r(data.frame))
	XXR. <- l( database$Datasets$Symbol, function(s) {
		r({ getdata(sym=s, typ="dataset", pkg=DATASET_PKGS()[s], transpose=DATASET_TRANSPOSE()[s]) })
	} )
	XXR  <- l( database$Datasets$Symbol, function(s) {
		r({ do.call(dplyr::rename, list(XXR.[[s]](), globals$FEATURE_MAPPINGS[[s]]) ) })
	} )
	CXXR <- l( database$Datasets$Symbol, function(s) {
		r({ select(XXR[[s]](),  where(is.factor)) })
	} )
	NXXR <- l( database$Datasets$Symbol, function(s) {
		r({ select(XXR[[s]](), !where(is.factor)) })
	} )

	# Prediction Reactives (one reactive per prediction, i.e. model x dataset)
	logsne("Initializing Prediction Reactives (one reactive per prediction, i.e. model x dataset) ...")
	# YYYR = Outcomes[model][dataset] = list(list(reactive(num)))
	YYYR <- {
		l( database$Models$Symbol, function(m) {
			l(database$Datasets$Symbol, function(d) {
				r({
					purrr::possibly(.f=predict, otherwise=NULL)(
						# possibly: wrapped function uses a default value (otherwise) whenever an error occurs
						PPR[[m]](), XXR[[d]]()
					)
				})
			})
		})
	}

	# Model Reactives (one reactive containing a list of all selected models)
	logsne("Initializing Model Reactives (one reactive containing a list of all selected models) ...")
	# MM = Models = list(char)
	# PP = Parameters[model] = list(num/obj)
	# FF = Features[model] = list(vector(char))
	MM <- r({ if (!is.null(input$MM)) database$Models$Symbol[match(input$MM, database$Models$Name)] else list() })
	PP <- r({ l(MM(), function(m) PPR[[m]]()) })
	FF <- r({ l(PP(), names) })

	# Dataset Reactives  (one reactive containing a list of all selected datasets)
	logsne("Initializing Dataset Reactives  (one reactive containing a list of all selected datasets) ...")
	# DD: Datasets = list(char)
	# XX: covariate dataframe[dataset] = list(data.frame)
	# SS: Samples[dataset] = list(vector(char))
	# CXX: Categorical covariate dataframe[dataset] = list(data.frame)
	# NXX: Numerical covariate dataframe[dataset] = list(data.frame)
	DD  <- r({ if (!is.null(input$DD)) input$DD else list() })
	XX  <- r({ l(DD(), function(d) XXR[[d]]() ) })
	SS  <- r({ l(XX(), rownames) })
	CXX <- r({ l(XX(), function(X) select(X, where(is.factor))) })
	NXX <- r({ l(XX(), function(X) select(X, !where(is.factor))) })

	# Prediction Reactive (one reactive containing a list of all prediction, i.e. models x datasets)
	logsne("Initializing Prediction Reactive (one reactive containing a list of all prediction, i.e. models x datasets) ...")
	# YYY: Outcomes[model][dataset] = r(list(list(num)))
	YYY <- r({ l(MM(), function(M) { l(DD(), function(D) { YYYR[[M]][[D]]() }) }) })
}

server__init_statics <- function() {
	logsne("Parsing URL ...")
	statics <- list()
	statics$url_search <- isolate(session$clientData$url_search)
	statics$url_protocol <- isolate(session$clientData$url_protocol) # examples: "http:", "https:"
	statics$url_hostname <- isolate(session$clientData$url_hostname) # examples: "localhost", "something.spang-lab.de"
	statics$url_port <- isolate(session$clientData$url_port) # examples: 443, 80, 99 (https-default, http-default, custom)
	statics$url_pathname <- isolate(session$clientData$url_pathname) # examples: "/", "/shinyserver/imlui"
	statics$url_params <- parseQueryString(url_search)
	statics$url <- paste0(url_protocol, "//", url_hostname, ":", url_port, url_pathname, url_search)
	statics$logsne("\tURL:", url)
	statics$github_redirect_url <- paste0(url_protocol, "//", url_hostname, ":", url_port, url_pathname)
	statics$shinygithub_oauth_app <- httr::oauth_app(
		appname="shinygithub", # TODO: take from database$settings
		key="51d46f96810d1fd182a2", # TODO: take from database$settings
		secret="66eec8782825eeb61007dbef32f91afc9c3587aa", # TODO: take from database$settings
		redirect_uri=github_redirect_url
	)
	logsne("Generating init values from database...")
	statics$settings <- isolate({x <- database$Settings$Value; names(x) <- database$Settings$ID; x})
}
