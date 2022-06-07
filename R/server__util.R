server__print_startup_message <- function() {
	logsne("Starting server from process ID:", Sys.getpid(), "...")
}

server__configure_bookmarking <- function() {
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
