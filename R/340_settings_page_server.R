settings_tab_server <- function(data) {
  # Overview Tables
  ses$output$S_PA_TBL <- render_dt(
    .tbl = database$Papers,
    .invisible = c("ID", "Details", "Datasets", "Models", "Summary")
  ) # .visible=c("Year", "Author", "Title", "Summary")
  ses$output$S_MO_TBL <- render_dt(
    .tbl = database$Models,
    .invisible = c("ID", "Symbol", "Features", "Betas", "Modeltype")
  ) # .visible=c("Platforms", "Methods", "Modeltype", "Symbol")
  ses$output$S_DS_TBL <- render_dt(
    .tbl = database$Datasets,
    .invisible = c("Samples", "Symbol")
  ) # .visible=c("N", "M")
  ses$output$S_SA_TBL <- render_dt(.tbl = database$Samples)
  ses$output$S_DT_TBL <- render_dt(.tbl = database$Datatypes)
  ses$output$S_ME_TBL <- render_dt(.tbl = database$Methods)
  ses$output$S_PF_TBL <- render_dt(.tbl = database$Platforms)
  ses$output$S_SE_TBL <- render_dt(.tbl = database$Settings)

  # Detail Tables
  output$S_PA_TBL2 <- render_dt2(
    .name = "Papers",
    .inputID = "S_PA_TBL",
    .keep = c("ID", "Year", "Author", "Summary"),
    .collapse = c("Details"),
    .maps = c("Models", "Datasets")
  )
  output$S_MO_TBL2 <- render_dt2(
    .name = "Models",
    .inputID = "S_MO_TBL",
    .keep = c("ID", "Name", "Classes", "Methods", "Datatypes", "Platforms", "Modeltype", "Symbol"),
    .collapse = c("Features", "Betas"),
    .maps = c("Papers")
  )
  output$S_DS_TBL2 <- render_dt2(
    .name = "Datasets",
    .inputID = "S_DS_TBL",
    .keep = c("ID", "N", "M", "Symbol"),
    .collapse = c("Samples"),
    .maps = c("Papers")
  )

  for (id in c(
    "S_PA_TBL", "S_MO_TBL", "S_DS_TBL", "S_SA_TBL", "S_DT_TBL",
    "S_ME_TBL", "S_PF_TBL", "S_SE_TBL"
  )) {
    setup_cell_edit_observer(id)
  }
}


# Table Observer Helpers
setup_cell_edit_observer <- function(id) {
  edit_id <- paste0(id, "_cell_edit")
  observeEvent(
    eventExpr = input[[edit_id]],
    handlerExpr = {
      database$Settings <<- DT::editData(
        data = database$Settings,
        info = input[[edit_id]],
        proxy = id,
        rownames = FALSE,
        resetPaging = FALSE
      )
    }
  )
}


# Table Output Helpers
render_dt <- function(.tbl, .visible = "_all", .invisible = character()) {
  colvis <- if (.visible[[1]] == "_all") (seq_along(.tbl) - 1) else (which(colnames(.tbl) %in% .visible) - 1)
  colvis <- which(colnames(.tbl)[colvis + 1] %notin% .invisible) - 1
  DT::renderDT(
    # args: https://cran.r-project.org/web/packages/DT/DT.pdf
    # options: https://datatables.net/reference/option/
    expr = {
      .tbl
    }, server = TRUE, editable = "cell", selection = "single", rownames = FALSE,
    options = list(
      scrollX = FALSE, scrollY = PAHPX1_05(), scrollCollapse = TRUE, paging = FALSE,
      # sDom  = '<"top">lrt<"bottom">ipf',
      columnDefs = list(
        list(className = "dt-left", targets = "_all"),
        list(visible = TRUE, targets = colvis),
        list(visible = FALSE, targets = "_all")
      )
    )
  )
}

render_dt2 <- function(.name, .inputID, .keep, .collapse, .maps) {
  DT::renderDT(
    expr = {
      rows_sel <- input[[paste0(.inputID, "_rows_selected")]]
      if (is.null(rows_sel)) {
        NULL
      } else {
        row <- database[[.name]][rows_sel[[1]], ]
        props <- c(.keep, .collapse, .maps)
        df <- data.frame(Property = props, Value = rep(NA, length(props)))
        rownames(df) <- props
        df[.keep, "Value"] <- unlist(row[.keep])
        for (e in .collapse) {
          df[e, "Value"] <- collapseCS(row[[e]][[1]])
        }
        for (m in .maps) {
          map_tbl_name <- collapse_(c("Mapping", sort(c(.name, m), decreasing = TRUE))) # Mapping_Paper_Models
          map_tbl <- database[[map_tbl_name]]
          own_col <- collapse_(c(substr(.name, 1, nchar(.name) - 1), "ID")) # Paper_ID
          map_col <- collapse_(c(substr(m, 1, nchar(m) - 1), "ID")) # Model_ID
          map_ids <- map_tbl[[own_col]] == row$ID
          map_vals <- map_tbl[map_ids, map_col]
          df[m, "Value"] <- collapseCS(map_vals)
        }
        df
      }
    },
    server = FALSE, editable = FALSE, selection = "single", rownames = FALSE,
    options = list(scrollX = FALSE, scrollY = PAHPX1_05(), scrollCollapse = TRUE, paging = FALSE)
  )
}
