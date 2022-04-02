# Symbol
# |                   Exported
# |                   | Description
# |                   | |
# %d%                 - Like purrrs %||% but also checks for empty lists and empty strings (named %d% like default)
# %notin%             - Like %in%, but negated (`x %notin% c(1,2,3)` is better readable than `!(x %in% c(1,2,3)`)
# caterr              - Like cat, but to stderr
# ceiling10           - Round up to nearest power of 10 (18-->100, 2345-->10000 etc.)
# clapply             - Like normal lapply, but sets names of returned list to X if X is a named character vector
# collapse            - Collapse a n*1 character vector `x` into a 1*1 character vector seperated by `s`
# collapseCS           - Collapse a n*1 character vector `x` into a 1*1 character vector seperated by ", "
# corn                - Like head and tail, but returns `n` rows/cols from each side of `x` (i.e. the corners of `x`)
# describe_df         - Describes first `ncol` columns of dataframe `df` (details below)
# exit                - Like pythons `sys.exit`
# getCacheDir         - Return normed path to cache directory (<home>/.imlui/cache)
# getdata             - Like normal get but additionally calls `data` if the object does not exist yet.
# getfd               - Return normed path to current file directory
# getHomeDir          - Return normed path to home directory (i.e. %USERPROFILE% or %HOME%)
# getSysCnf           - Return normed path to system config file (e.g. .../imlui/inst/config.json)
# getUserCnf          - Return normed path to user config file (e.g. <home>/.imlui/config.json)
# glueerr             - Like glue but to stderr
# is.categorical      - Guesses if a vector contains categorical values or not
# is.none             - TRUE for NULL, empty lists and empty string
# is.non.empty.string - TRUE for non empty strings
# libraries           - Import multiple libs at once
# logsne               - Like `cat(..., file=stderr); cat("\n", file=stderr)`
# nl_nv_2_df          @ Convert named list of named vectors `nlnv` to dataframe `df` with
# normPath            - Replace backslashes with forward slashes
# not.none            - Equal  to `!is.none` but better readable
# null2na             - Return input `e` unchanged or NA if is.null(e)
# PAHS                - Numerical vector describing plot area height steps, e.g. seq(240, 1920, 60)
# PAWS                - Numerical vector describing plot area width steps, e.g. seq(240, 1920, 60)
# predict.numeric     @ Predict method for linear models provided as plain named vectors
# PRINT               - Like `print` but to stderr
# readSysCnf          - Return System Config as environment
# read_imlui_config         - Return User Config as environment
# STR                 - Like `str` but to stderr
# V                   - String containing current package version, e.g. '0.0.0.9010'

# Like purrrs %||% but also checks for empty lists and empty strings (%d% for default)
`%d%` <- function (x, y) { if (is.none(x)) y else x }

# Like %in%, but negated (`x %notin% c(1,2,3)` is better readable than `!(x %in% c(1,2,3)`)
`%notin%` <- function(x, table) match(x, table, nomatch = 0) == 0

# Like cat, but to stderr
caterr <- function(..., end="\n") { cat(..., end, file=stderr()) }
cat2e <- function(...) toscutil::cat2(..., file=stderr())
cat0e <- function(...) toscutil::cat0(..., file=stderr())
catfe <- function(...) toscutil::catf(..., file=stderr())
catne <- function(...) toscutil::catn(..., file=stderr())
cat00e <- function(...) toscutil::cat0(..., file=stderr())
cat0ne <- function(...) toscutil::cat0n(..., file=stderr())
catfne <- function(...) toscutil::catfn(..., file=stderr())
catnne <- function(...) toscutil::catnn(..., file=stderr())
catsne <- function(...) toscutil::catsn(..., file=stderr())
catsse <- function(..., sep=" ", end=" ") toscutil::cat2(..., sep=sep, end=end, file=stderr())
cat0se <- function(..., sep="", end=" ") toscutil::cat2(..., sep=sep, end=end, file=stderr())

logerr <- function(...) { cat0se(toscutil::now(), ":"); caterr(...)}
log2e <- function(...) { cat0se(toscutil::now(), ":"); cat2e(...)}
log0e <- function(...) { cat0se(toscutil::now(), ":"); cat0e(...)}
logfe <- function(...) { cat0se(toscutil::now(), ":"); catfe(...)}
logne <- function(...) { cat0se(toscutil::now(), ":"); catne(...)}
log00e <- function(...) { cat0se(toscutil::now(), ":"); cat00e(...)}
log0ne <- function(...) { cat0se(toscutil::now(), ":"); cat0ne(...)}
logfne <- function(...) { cat0se(toscutil::now(), ":"); catfne(...)}
lognne <- function(...) { cat0se(toscutil::now(), ":"); catnne(...)}
logsne <- function(...) { cat0se(toscutil::now(), ":"); catsne(...)}
logsse <- function(...) { cat0se(toscutil::now(), ":"); catsse(...)}
log0se <- function(...) { cat0se(toscutil::now(), ":"); cat0se(...)}

# Round up to nearest power of 10 (18-->100, 2345-->10000 etc.)
ceiling10 <- function(x) { 10^ceiling(log10(x)) }

# Like normal lapply, but sets names of returned list to X if X is a named character vector
clapply <- function(X, FUN, ...) {
	r <- lapply(X, FUN, ...)
	if (is.null(names(X)) && is.character((X)))
		names(r) <- X
	r
}

# Collapse a n*1 character vector `x` into a 1*1 character vector seperated by `s`
collapse <- function(x, s) paste(x, collapse=s)
collapse_ <- function(x)   paste(x, collapse="_")
collapseCS <- function(x)  paste(x, collapse=", ")  # TODO: remove `s` argument!
collapseNL <- function(x)  paste(x, collapse="\n")

connect_imlui_db <- function(init_if_empty=FALSE) {
	# Define Variables
		dbms.type <- options("imlui.config.dbms.type")[[1]]
		dbms.filepath <- options("imlui.config.dbms.filepath")[[1]]
		dbms.hostname <- options("imlui.config.dbms.hostname")[[1]]
		dbms.port <- options("imlui.config.dbms.port")[[1]]
		dbms.database <- options("imlui.config.dbms.database")[[1]]
		dbms.username <- options("imlui.config.dbms.username")[[1]]
		dbms.password <- options("imlui.config.dbms.password")[[1]]
		default_imlui_db.sqlite <- system.file("assets/sqlite/imlui_db.sqlite", package="imlui")
		creation_of_default_users_required <- FALSE
	# Check Variables
		if (is.null(dbms.type)) {
			stop("Make sure to call `read_imlui_config()` before calling `connect_imlui_db()`")
		} else if (!(dbms.type %in% c("sqlite", "postgres"))) {
			stop("imlui_config.yml/dbms/type must be either 'sqlite' or 'postgres', not", dbms.type)
		}
	# SQLite case
		if (dbms.type == "sqlite") {
			if (init_if_empty) {
				logsne("Checking database", dbms.filepath, "...")
				if (!file.exists(dbms.filepath)) {
					catsn("Database does not exist yet:")
					catsn("Copying", default_imlui_db.sqlite, "to", dbms.filepath, "...")
					file.copy(default_imlui_db.sqlite, dbms.filepath)
					creation_of_default_users_required <- TRUE
				}
			}
			imlui_db <- DBI::dbConnect(RSQLite::SQLite(), dbms.filepath)
	# Postgres case
		} else if (dbms.type == "postgres") {
			imlui_db <- DBI::dbConnect(
				drv = RPostgres::Postgres(), host = dbms.hostname, port = dbms.port,
				dbname = dbms.database, user = dbms.username, password = dbms.password
			)
			if (init_if_empty) {
				log0ne("Checking database ", dbms.hostname, ":", dbms.port, "/", dbms.database, "...")
				if (length(DBI::dbListTables(imlui_db)) == 0) {
					log0ne("Found no tables in `", dbms.hostname, ":", dbms.port, "/", dbms.database, "`:")
					log0ne("Initializing with default values...")
					local({
						default_imlui_db <- DBI::dbConnect(RSQLite::SQLite(), default_imlui_db.sqlite)
						on.exit(DBI::dbDisconnect(default_imlui_db))
						for (tbl in DBI::dbListTables(default_imlui_db)) {
							DBI::dbWriteTable(imlui_db, tbl, value=DBI::dbReadTable(default_imlui_db, tbl))
						}
					})
					creation_of_default_users_required <- TRUE
				}
			}
		}
	# Create initial users if required
		if (creation_of_default_users_required) {
			if (FALSE) { # TODO: Temporarily disabled, for now (development phase) keep pass1 and pass2 as fixed pws
				logsne("Generating initial users (please store them in a secure place) ...")
				initial_users_df <- data.frame(
					user_id = c("admin", "testuser"),
					password = c("pass1", "pass2"),
					group_ids = c("admin", "standard"),
					display_name = c("Admin", "Test User")
				)
				PRINT(initial_users_df)
				DBI::dbWriteTable(conn=imlui_db, name="users", value=initial_users_df, overwrite=TRUE)
			}
		}
	return(imlui_db)
}

# Like head and tail, but returns `n` rows/cols from each side of `x` (i.e. the corners of `x`)
corn <- function(x, n=2L) {
	if(is.vector(x)) return(x)
	stopifnot("matrix" %in% class(x) || "data.frame" %in% class(x))
	rs <- nrow(x)
	cs <- ncol(x)
	if (all(rs > 4, cs > 4))
		x[c(1:n, (rs-n+1):rs), c(1:n, (cs-n+1):cs)]
	else
		x
}

# Describes first `ncol` columns of dataframe `df`
#
# Each column is described through its column name, column index and range. If
# a column has less unique values than `max_values`, the unique values are
# listed instead of showing the range. All numerical values are rounded to
# `digit` digits.
describe_df <- function(df, ncol=25, max_levels=8, digits=2) {
	options(digits=2)
	for (i in 1:ncol) {
		if (is.numeric(df[, i])) {
			df[, i] <- round(df[, i], digits)
		}
		levels_i <- sort(unique(df[, i]))
		colname_i <- colnames(df)[i]
		if (length(levels_i) < max_levels) {
			details <- paste0(
				"[Levels: ",
				paste0(levels_i, collapse=", "),
				"]"
			)
		} else if(is.numeric(levels_i)) {
			range_i <- round(range(levels_i), digits)
			details <- paste0("[Range: ", range_i[1], "-", range_i[2], "]")
		} else {
			n <- length(levels_i)
			k <- (max_levels %/% 2)
			details <- paste0(
				"[Levels: ",
				paste0(levels_i[1:k], collapse=", "),
				", ..., ",
				paste0(levels_i[(n-k):n], collapse=", "),
				"]"
			)
		}
		cat(i, ". ", colname_i, " ", details, "\n", sep = "")
	}
}

dput2 <- function(...) {
	capture.output(dput(...))
}

# Like pythons `sys.exit`
exit <- function(exitcode) { if (interactive()) stop(exitcode, call.=FALSE) else quit(status=exitcode) }


# Return normed path to cache directory (<home>/.imlui/cache)
getCacheDir <- function() {
	cacheDir <- file.path(getHomeDir(), ".imlui", "cache")
	if (!file.exists(cacheDir)) { dir.create(cacheDir, recursive=TRUE) }
	cacheDir
}

# Like normal get but additionally calls `data` if the object does not exist yet.
#
# Also does some initial transformation to make the data compliant to the format expected by imlui functions. Where to
# look for an object is specified in imlui variable PKG and whether the object should be transposed after initialisation
# is specified in TRANSPOSE_REQUIRED.
#
# Args:
#   s: string, object to get
#   t: string, type of object (either "d" for dataset, or "m" for model)
# Returns:
#   o: get(s)
getdata <- function(s, t="d", p="") {
	if (s %in% datasets) {
		if (!is.null(s) && !exists(s)) { data(list=s, package=PKG[s]) }
		o <- get(s)
		if (any(c("RccSet", "ExpressionSet") %in% class(o))) { o <- Biobase::exprs(o) }
		if (TRANSPOSE_REQUIRED[s]) { o <- t(o) }
		o <- as.data.frame(o, stringsAsFactors=TRUE)
		n <- colnames(o)
		o[,] <- lapply(seq_along(o), function(i) {
			if (is.categorical(v=o[,i], n=n[i])) as.factor(o[,i]) else as.numeric(o[,i])
		})
		o
	} else if (t == "m") {
		if (!is.null(s) && !exists(s)) {
			data(list=s, package=PKG[s])
		}
		o <- get(s)
		o
	} else {
		stop(glue("Trying to call `getdata(s)`, but `s` ({'s'}) is neither in `datasets` nor `models`."))
	}
}

# Return normed path to current file directory
getfd <- function() {
	if (interactive() && ((sys.nframe() > 0) && ("ofile" %in% ls(sys.frame(0))))) {
		# probably started through `source` from interactive R session
		f <- sys.frame(1)$ofile
	} else {
		# probably started through `Rscript` from external shell
		args <- commandArgs()
		filearg <- args[grepl("^--file=", args)][[1]]
		f <- strsplit(filearg, "--file=")[[1]][2]
	}
	return(normpath(dirname(f)))
}

# Return normed path to home directory (i.e. %USERPROFILE% or %HOME%)
getHomeDir <- function() { normPath(Sys.getenv("USERPROFILE") %d% Sys.getenv("HOME")) }

# getSysCnf - Return normed path to system config file (e.g. .../imlui/inst/config.json)
getSysCnf <- function() { system.file("config.json", package="imlui") }

# Return normed path to user config dir (e.g. <home>/.imlui)
getUserCnfDir <- function() { file.path(getHomeDir(), ".imlui") }

# Return normed path to user config file (e.g. <home>/.imlui/config.json)
# getUserCnf <- function() { file.path(getHomeDir(), ".imlui", "config.json") }

# Return User Config as environment
getUserCnf <- function(create_from_template_if_missing) {
	norm <- function(...) { toscutil::norm_path(..., sep=sep) }
	file_cl_arg <- {x <- commandArgs(); x[grep("--config-file", x) + 1]}
	dir_cl_arg <- {x <- commandArgs(); x[grep("--config-dir", x) + 1]}
	file_env_var <- Sys.getenv("IMLUI_CONFIG_FILE")
	dir_env_var <- Sys.getenv("IMLUI_CONFIG_DIR")
	sep = "/"
	# 1. Check <config-file>
	if (is.non.empty.string(file_cl_arg)) {
		if (!file.exists(file_cl_arg)) {
			stop(file_cl_arg, " does not exist")
		}
		return(norm(file_cl_arg))
	}
	# 2. Check $IMLUI_CONFIG_FILE
	if (is.non.empty.string(file_env_var)) {
		if (!file.exists(file_env_var)) {
			stop(file_env_var, " does not exist")
		}
		return(norm(file_env_var))
	}
	# 3. Check <config-dir>/imlui_config.yml
	if (is.non.empty.string(dir_cl_arg)) {
		x <- norm(dir_cl_arg, "imlui_config.yml")
		if (file.exists(x))
			return(x)
	}
	# 4. Check $IMLUI_CONFIG_DIR/imlui_config.yml
	if (is.non.empty.string(dir_env_var)) {
		x <- norm(dir_env_arg, "imlui_config.yml")
		if (file.exists(x))
			return(x)
	}
	# 5. Check ${PWD}/<file-name>
	x <- norm(getwd(), "imlui_config.yml")
	if (file.exists(x)) {
		return(x)
	}
	# 6. Check $XDG_CONFIG_HOME/imlui/imlui_config.yml
	x <- norm(Sys.getenv("XDG_CONFIG_HOME"), "imlui/imlui_config.yml")
	if (Sys.getenv("XDG_CONFIG_HOME") != "" && file.exists(x)) {
		return(x)
	}
	# 7. Check $HOME/.config/imlui/imlui_config.yml
	x <- norm(Sys.getenv("HOME"), ".config/imlui/imlui_config.yml")
	if (Sys.getenv("HOME") != "" && file.exists(x)) {
		return(x)
	}
	# 8. Check $USERPROFILE/.config/imlui/imlui_config.yml
	x <- norm(Sys.getenv("USERPROFILE"), ".config/imlui/imlui_config.yml")
	if (Sys.getenv("USERPROFILE") != "" && file.exists(x)) {
		return(x)
	}
	# 9. No config exists, copy <IMLUI_PACKAGE_DIR>/assets/yml/imlui_config.yml to <IMLUI_CONFIG_DIR> and return
	# <IMLUI_CONFIG_DIR>/imlui_config.yml
	s <- system.file("assets/yml/imlui_config.yml", package="imlui")
	d <- toscutil::config_dir("imlui", cl_arg=dir_cl_arg, env_var=dir_env_var, create=TRUE, sep="/")
	if (create_from_template_if_missing) {
		file.copy(s, d)
	}
	return(norm(d, "imlui_config.yml"))
}

# Like glue but to stderr
glueerr <- function(...) { message(glue(..., .envir=parent.frame(1)))}

# TODO
help_console <- function(topic, format=c("text", "html", "latex", "Rd"),
						 lines=NULL, before=NULL, after=NULL) {
  format=match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  helpfile = utils:::.getHelpFile(help(topic))
  hs <- capture.output(switch(format,
							  text=tools:::Rd2txt(helpfile),
							  html=tools:::Rd2HTML(helpfile),
							  latex=tools:::Rd2latex(helpfile),
							  Rd=tools:::prepare_Rd(helpfile)
							  )
					  )
  if(!is.null(lines)) hs <- hs[lines]
  hs <- c(before, hs, after)
  cat(hs, sep="\n")
  invisible(hs)
}

# Guesses if a vector contains categorical values or not
#
# c(1,    2,    2,    2,    1,    2   ) --> TRUE (categorical)
# c(0,    0,    0.50, 0.50, 1,    1   ) --> TRUE (categorical)
# c(1.23, 7.10, 4.51, 2.22, 4.56, 5.12) --> FALSE (continous)
is.categorical <- function(v, n) {
	levels <- sort(unique(v))
	threshold <- floor(length(v) * 0.33)
	if ( (is.numeric(v)) && (length(levels) > threshold) && (!(n %in% CATEGORICALS)) )  {
		FALSE
	} else {
		TRUE
	}
}

# TRUE for NULL, empty lists and empty string
is.none <- function(x) {
	if (is.null(x)
	|| ((typeof(x) == "character") && (x == ""))
	|| ((typeof(x) == "list") && (length(x) == 0)) ) TRUE else FALSE
}

is.non.empty.string <- function(x) {
	if (!is.null(x) && length(x) == 1 && is.character(x) && nchar(x) != 0) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

# Import multiple libs at once
#
# libs: charcter vector of packages
# log: where to store messages generated by call to `library` (only printed in case of exit)
libraries <- function(libs, log=NULL) {
	# Import <libs> and redirect output to <log> if specified
	if (!is.null(log)) {
		con <- file(log, open="a")
		sink(file=con)
		sink(file=con, type="message")
	}
	failed <- logical(length(libs))
	for (i in 1:length(libs)) {
		failed[i] <- tryCatch(
			expr={library(libs[i], character.only=TRUE); FALSE},
			error=function(e) {TRUE}
		)
	}
	if (!is.null(log)) {
		sink(type="message")
		sink()
		close(con)
	}
	if (any(failed)) {
		cat("Import of following packages failed:\n")
		cat(libs[failed])
		cat("\nSee", log, "for details.\n")
		exit(1)
	}
}

#' Convert named list of named vectors `nlnv` to dataframe `df` with:
#' ```
#' df[, 1] == names(nlnv)
#' df[, 2] == names(nlnv[i])
#' df[, 3] == nlnv[[i]][j]
#'
#' @param x Numeric value
#'
#' @return
#' @export
#'
#' @examples
#' # nl_nv = List of 3:
#' # $ lamis_train: Named num [1:233] GSM275076 = 0.6801, GSM275077 = 1.0086 ...
#' # $ lamis_test1: Named num [1:181] GSM274895 = 0.505, GSM274896 = 1.541 ...
#' # $ lamis_test2: Named num [1:466] Ricover60.S99 = 0.822, Ricover60.S101 = 1.203 ...
#'
#' head(nl_nv_2_df(nl_nv))
#' #       dataset   samples           y
#' # 1 lamis_train GSM275076 -0.68014232
#' # 2 lamis_train GSM275077 -1.00861666
#' # 3 lamis_train GSM275078 -1.37423898
#' # 4 lamis_train GSM275079 -0.79874698
#' # 5 lamis_train GSM275080  0.03045401
#' # 6 lamis_train GSM275081 -0.61785528
#'
#' str(nl_nv_2_df(nl_nv))
#' # 'data.frame':   880 obs. of  3 variables:
#' #  $ dataset: chr  "lamis_train" "lamis_train" "lamis_train" "lamis_train" ...
#' #  $ samples: chr  "GSM275076" "GSM275077" "GSM275078" "GSM275079" ...
#' #  $ y      : num  -0.6801 -1.0086 -1.3742 -0.7987 0.0305 ...
nl_nv_2_df <- function(nl_nv, col1="dataset", col2="sample", col3="y") {
	df <- plyr::rbind.fill(lapply(1:length(nl_nv), function(i) {
		y <- nl_nv[[i]]
		data.frame(rep(names(nl_nv)[[i]], length(y)), names(y), y)
	}))
	colnames(df) <- c(col1, col2, col3)
	df
}

#' Convert named list of named lists `nl` to tibble:
nl2t <- function(nl) {
	rnams <- names(nl)
	expr = reduce(map(nl, names), union)
	cols <- clapply(cnams, function(j) sapply(rnams, function(i) null2na(nl[[i]][[j]])))
	df <- as_tibble(cols)
	rownames(df) <- rnams
	df
}

# Replace backslashes with forward slashes
normPath <- function(p) str_replace_all(p, "\\\\", "/")

# Equal  to `!is.none` but better readable
not.none <- function(x) !is.none(x)

# Return input `e` unchanged or NA if is.null(e)
null2na <- function(e) {if (is.null(e)) {NA} else {e}}

# Numerical vector describing plot area height steps, e.g. seq(240, 1920, 60)
PAHS <- seq(240, 1920, 60)

# Numerical vector describing plot area width steps, e.g. seq(240, 1920, 60)
PAWS <- seq(240, 1920, 60)

#' Predict method for linear models provided as plain named vectors
#'
#' @param b  Named numeric vector
#' @param X  Numeric matrix with `names(b) %in% c("Intercept", colnames(X))`
#'
#' @return
#' @export
#'
#' @examples
#' TODO: remove this method and make `lamis_signature` an object with a own predict method
predict.numeric <- function(b, X, fm=NULL) {
	# browser()
	if (class(b) == "matrix" || class(b) == "array") {
		if (ncol(b) == 1) {
			b2 <- b[,1] # preserved colnames as names
		} else if (nrow(b) == 1) {
			b2 <- b[1, ] # preserved rownames as names
		} else {
			stop(glue("`m` should be 1D but has dim {dim(m)}"))
		}
	} else {
		b2 <- b
	}
	if ("Intercept" %in% names(b2)) {
		b3 <- b2[names(b2) != "Intercept"]
		intercept <- b2["Intercept"]
	} else {
		b3 <- b2
		intercept <- 0
	}
	if (!is.numeric(b3)) stop(glue("b3 must be numeric but is {typeof(b3)}"))
	tryCatch(
		expr = { scores <- as.numeric(as.matrix(X[, names(b3)]) %*% b3) + intercept },
		error = function(cond) {
			stop(glue("names(b3) must be in colnames(X), but the following are not: ",
					  "{names(b3)[names(b3) %notin% colnames(X)]}"))
		}
	)
	names(scores) <- rownames(X)
	return(scores)
}

# Like `print` but to stderr
PRINT <- function(...) { sink(stderr()); print(...); sink() }

# Taken from https://github.com/PaulC91/shinyauthr/blob/master/R/internal.R
randomString <- function(n = 64) {
  paste(
	sample(x = c(letters, LETTERS, 0:9), size = n, replace = TRUE),
	collapse = ""
  )
}

# Return System Config as environment
readSysCnf <- function() {
	# new_environment(data = read.config(file=getSysCnf(), file.type = "json"))
	read.config(file=getSysCnf(), file.type = "json")
}

read_imlui_config <- function(create_from_template_if_missing=TRUE) {
	IMLUI_CONFIG_DIR <- toscutil::config_dir("imlui", env_var=Sys.getenv("IMLUI_CONFIG_DIR"), create=TRUE, sep="/")
	imlui_config.yml <- getUserCnf(create_from_template_if_missing=create_from_template_if_missing)
	logsne("Reading ", imlui_config.yml, "...")
	yml <- as.list(unlist(yaml::read_yaml(imlui_config.yml)))
	for (i in seq_along(yml)) {
		yml[[i]] <- gsub("${IMLUI_CONFIG_DIR}", IMLUI_CONFIG_DIR, yml[[i]], fixed=TRUE)
	}
	# yml == list(dbms=list(type=sqlite, filepath=C:/Users/tobi/.config/imlui/imlui_db.sqlite)) or
	# yml == list(dbms=list(type=postgres, hostname=localhost, port=5432, database=imluidb, username=ab, password=cd))
	names(yml) <- paste0("imlui.config.", names(yml))
	do.call(options, yml) # dbms.ype|hostname|port|database|username|password}")
	yml
}

redirection_to <- function(url) {
	sprintf('location.replace("%s");', url) %>%
		shiny::HTML() %>%
		shiny::tags$script() %>%
		shiny::renderUI()
}

storeUserCnf <- function(cnf) {
	jsonlite::write_json(cnf, path=getUserCnf(), pretty=TRUE, digits=NA, null='list', na="null")
	# write.config(config.dat = cnf, file.path = getUserCnf(), write.type = "json", auto_unbox=TRUE)
	# write.config args:
	#   config.dat: a list of config (eg. generated by read.config)
	#   file.path: File path of configuration to write. Defaults to the value of the 'R_CONFIGFILE_ACTIVE' environment variable ('config.cfg' if the variable does not exist)
	#   write.type: json/ini/yaml
	#   sections: Sections that need be write in file, default is NULL and use all of sections
	#   ...: Arguments for write.ini, prettify, toJSON, as.yaml and cat encoding if not specifield
	# toJSON args:
	#   x: the object to be encoded
	#   dataframe: how to encode data.frame objects: must be one of 'rows', 'columns' or 'values'
	#   matrix: how to encode matrices and higher dimensional arrays: must be one of 'rowmajor' or 'columnmajor'.
	#   Date: how to encode Date objects: must be one of 'ISO8601' or 'epoch'
	#   POSIXt: how to encode POSIXt (datetime) objects: must be one of 'string', 'ISO8601', 'epoch' or 'mongo'
	#   factor: how to encode factor objects: must be one of 'string' or 'integer'
	#   complex: how to encode complex numbers: must be one of 'string' or 'list'
	#   raw: how to encode raw objects: must be one of 'base64', 'hex' or 'mongo'
	#   null: how to encode NULL values within a list: must be one of 'null' or 'list'
	#   na: how to print NA values: must be one of 'null' or 'string'. Defaults are class specific
	#   auto_unbox: automatically unbox all atomic vectors of length 1. It is usually safer to avoid this and instead use the unbox function to unbox individual elements. An exception is that objects of class AsIs (i.e. wrapped in I()) are not automatically unboxed. This is a way to mark single values as length-1 arrays.
	#   digits: max number of decimal digits to print for numeric values. Use I() to specify significant digits. Use NA for max precision.
	#   pretty: adds indentation whitespace to JSON output. Can be TRUE/FALSE or a number specifying the number of spaces to indent. See prettify
	#   force: unclass/skip objects of classes with no defined JSON mapping
}

# Like `str` but to stderr
STR <- function(...) { sink(stderr()); str(...); sink() }

# String containing current package version, e.g. '0.0.0.9010'
V <- packageVersion("imlui")
