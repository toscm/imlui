# Database <- R6::R6Class("Database",
#   public = list(
#     name = NULL,
#     hair = NULL,
#     initialize = function(name = NA, hair = NA) {
#       self$name <- name
#       self$hair <- hair
#       self$greet()
#     },
#     set_hair = function(val) {
#       self$hair <- val
#     },
#     greet = function() {
#       cat(paste0("Hello, my name is ", self$name, ".\n"))
#     }
#   )
# )

# db__database <- function(...) {
# 	Database$new(...)
# }

# read_db <- function(db_conn) {
# 	db <- list()
# 	logsne("Reading tables...")
# 	for (tbl in globals$TABLES) {
# 		db[[tbl]] <- DBI::dbReadTable(db_conn, tbl)
# 	}
# 	rownames(db$users) <- db$users$user_id
# 	rownames(db$Models) <- db$Models$ID
# 	rownames(db$Datasets) <- db$Datasets$ID
# 	db
# }

# connect_imlui_db <- function(init_if_empty=FALSE) {
# 	# Define Variables
# 		dbms.type <- options("imlui.config.dbms.type")[[1]]
# 		dbms.filepath <- options("imlui.config.dbms.filepath")[[1]]
# 		dbms.hostname <- options("imlui.config.dbms.hostname")[[1]]
# 		dbms.port <- options("imlui.config.dbms.port")[[1]]
# 		dbms.database <- options("imlui.config.dbms.database")[[1]]
# 		dbms.username <- options("imlui.config.dbms.username")[[1]]
# 		dbms.password <- options("imlui.config.dbms.password")[[1]]
# 		default_imlui_db.sqlite <- system.file("assets/sqlite/imlui_db.sqlite", package="imlui")
# 		creation_of_default_users_required <- FALSE
# 	# Check Variables
# 		if (is.null(dbms.type)) {
# 			stop("Make sure to call `util__read_imlui_config()` before calling `connect_imlui_db()`")
# 		} else if (!(dbms.type %in% c("sqlite", "postgres"))) {
# 			stop("imlui_config.yml/dbms/type must be either 'sqlite' or 'postgres', not", dbms.type)
# 		}
# 	# SQLite case
# 		if (dbms.type == "sqlite") {
# 			if (init_if_empty) {
# 				logsne("Checking database", dbms.filepath, "...")
# 				if (!file.exists(dbms.filepath)) {
# 					catsne("Database does not exist yet:")
# 					catsne("Copying", default_imlui_db.sqlite, "to", dbms.filepath, "...")
# 					file.copy(default_imlui_db.sqlite, dbms.filepath)
# 					creation_of_default_users_required <- TRUE
# 				}
# 			}
# 			imlui_db <- DBI::dbConnect(RSQLite::SQLite(), dbms.filepath)
# 	# Postgres case
# 		} else if (dbms.type == "postgres") {
# 			imlui_db <- DBI::dbConnect(
# 				drv = RPostgres::Postgres(), host = dbms.hostname, port = dbms.port,
# 				dbname = dbms.database, user = dbms.username, password = dbms.password
# 			)
# 			if (init_if_empty) {
# 				log0ne("Checking database ", dbms.hostname, ":", dbms.port, "/", dbms.database, "...")
# 				if (length(DBI::dbListTables(imlui_db)) == 0) {
# 					log0ne("Found no tables in `", dbms.hostname, ":", dbms.port, "/", dbms.database, "`:")
# 					log0ne("Initializing with default values...")
# 					local({
# 						default_imlui_db <- DBI::dbConnect(RSQLite::SQLite(), default_imlui_db.sqlite)
# 						on.exit(DBI::dbDisconnect(default_imlui_db))
# 						for (tbl in DBI::dbListTables(default_imlui_db)) {
# 							DBI::dbWriteTable(imlui_db, tbl, value=DBI::dbReadTable(default_imlui_db, tbl))
# 						}
# 					})
# 					creation_of_default_users_required <- TRUE
# 				}
# 			}
# 		}
# 	# Create initial users if required
# 		if (creation_of_default_users_required) {
# 			if (FALSE) { # TODO: Temporarily disabled, for now (development phase) keep pass1 and pass2 as fixed pws
# 				logsne("Generating initial users (please store them in a secure place) ...")
# 				initial_users_df <- data.frame(
# 					user_id = c("admin", "testuser"),
# 					password = c("pass1", "pass2"),
# 					group_ids = c("admin", "standard"),
# 					display_name = c("Admin", "Test User")
# 				)
# 				PRINT(initial_users_df)
# 				DBI::dbWriteTable(conn=imlui_db, name="users", value=initial_users_df, overwrite=TRUE)
# 			}
# 		}
# 	return(imlui_db)
# }

# 	########## Define database helper functions ########################################################################
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
			login_time <- NULL # TODO: remove tidyverse NSE stuff
			database$mapping_users_sessions %>%
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
				login_time=as.character(lubridate::now())
			) %>%
			DBI::dbWriteTable(db_conn, "mapping_users_sessions", ., append=TRUE)
		}
		set_login_cookie <- function(user_id, session_id) {
			shinyjs::js$setcookie(session_id)
			db_insert_cookie(user_id, session_id)
		}

