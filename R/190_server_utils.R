print_startup_message <- function() {
	infomsg("Starting server from process ID:", Sys.getpid(), "...")
}

configure_bookmarking <- function() {
	infomsg("Configuring Bookmark excludes...")
	setBookmarkExclude(c(
		"dim",
		"login_button",
		"login_button_github",
		"login_button_google",
		"login_button_auth_spang_lab",
		"login_button_gitlab_spang_lab",
		"login_button_gitlab",
		"login_user_name",
		"S_DS_TBL_cell_clicked",
		"S_DS_TBL_cells_selected",
		"S_DS_TBL_columns_selected",
		"S_DS_TBL_rows_all",
		"S_DS_TBL_rows_current",
		"S_DS_TBL_rows_selected",
		"S_DS_TBL_search",
		"S_DS_TBL_state",
		"S_MO_TBL_cell_clicked",
		"S_MO_TBL_cells_selected",
		"S_MO_TBL_columns_selected",
		"S_MO_TBL_rows_all",
		"S_MO_TBL_rows_current",
		"S_MO_TBL_rows_selected",
		"S_MO_TBL_search",
		"S_MO_TBL_state",
		"S_PA_TBL_cell_clicked",
		"S_PA_TBL_cells_selected",
		"S_PA_TBL_columns_selected",
		"S_PA_TBL_rows_all",
		"S_PA_TBL_rows_current",
		"S_PA_TBL_rows_selected",
		"S_PA_TBL_search",
		"S_PA_TBL_state",
		"S_SE_TBL_cell_clicked",
		"S_SE_TBL_cells_selected",
		"S_SE_TBL_columns_selected",
		"S_SE_TBL_rows_all",
		"S_SE_TBL_rows_current",
		"S_SE_TBL_rows_selected",
		"S_SE_TBL_search",
		"login_jscookie",
		"S_SE_TBL_state",
		"logout_button"
	))
}

asSVG <- function(
	.pname,
	.func,
	.arglist,
	.fname=deparse(substitute(.func))
) {
	width <- PAW()
	height <- PAH() - 240
	outfile <- paste0(file.path(get_imlui_cache_dir(), .pname), ".svg")
	argListChar <- deparse(substitute(.arglist))
	if (none(.arglist, is.null)) {
		infomsg(
			"Storing result of", .fname, "(...) in svg of size:", width, "x", height
		)
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
			# infomsg(glue("{.pname}: plot(1:10)\n"))
			# STR(.arglist, appstate[["DEBUG_LEVEL"]])
			svglite(outfile, width=width/92, height=height/92)
			tryCatch(
				plot(1:10, main="DUMMY"), # FIXME: display .arglist instead
				error = function(cond) STR(cond),
				finally = dev.off()
			)
		} else {
			# Use existing file until inputs are valid again
			# infomsg(glue("{.pname}: using cached file {outfile}\n"))
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

set_login_cookie <- function(db, user_id, session_id) {
  shinyjs::js$setcookie(session_id)
  db$insert_cookie(user_id, session_id)
}
