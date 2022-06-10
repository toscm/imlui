server__web_app_ui_server <- function(data) {
  # TEXT OUTPUTS
	## OK: App Info - Text-Output
	data$output$AI_TO <- renderUI({
		div( HTML( paste0(
			"<b>UserID:</b> ",         USER_ID(),                 "<br>",
			"<b>Version:</b> ",        V,                         "<br>",
			"<b>Browser Window:</b> ", BW(),  " x ", BH(), " px", "<br>",
			"<b>Main Panel:</b> ",     MPW(), " x ", MPH()," px", "<br>",
			"<b>Plot Area:</b> ",      PAW(), " x ", PAH()," px", "<br>",
			"<br>"
		) ) )
	})

	## OK: Dataset-Description Text-Output
	data$output$DA_D_T <- renderPrint({
		walk(DD(), ~ cat(.x, "\n", describe_df(XXR[[.x]]()), "\n"))
	})

	## TODO: Model-Description Text-Output
	data$output$MA_MD_TO <- renderPrint({
		# logsne("MA_DD_TO")
		pp <- PP()
		for (i in seq_along(pp)) {
			cat(names(pp)[[i]], "\n", sep="")
			str(pp[[i]], max.level=NA)
			cat("\n")
		}
	})
}


# Table Output Helpers
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
        row <- database[[.name]][rows_sel[[1]], ]
        props <- c(.keep, .collapse, .maps)
        df <- data.frame(Property=props, Value=rep(NA, length(props))); rownames(df) <- props
        df[.keep, "Value"] <- unlist(row[.keep])
        for (e in .collapse) {
          df[e, "Value"] <- collapseCS(row[[e]][[1]])
        }
        for (m in .maps) {
          map_tbl_name <- collapse_(c("Mapping", sort(c(.name, m), decreasing=TRUE))) # Mapping_Paper_Models
          map_tbl <- database[[map_tbl_name]]
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