dataset_analysis_page <- function() {
  tabsetPanel(
    id = "DA_T_CS",
    selected = isolate(input$DA_T_CS) %||% ses$statics$settings[["DA_T_CS"]],
    tabPanel("Descriptions", withSpinner(verbatimTextOutput(outputId = "DA_D_T"))),
    tabPanel("MSD-Plot", {
			# Vars
			xtc <- ytc <- c("identity", "log2", "log10")
			xt <- isolate(input$MSDP_XT) %||% "identity"
			yt <- isolate(input$MSDP_YT) %||% "identity"
			mm <- ses$r$model$symbol_list()
			# Row 1 (plot only)
			MSDP_PO <- column(12, withSpinner(plotOutput(outputId="MSDP", height=PAHPX1(), width=PAWPX() )))
			row1 <- fluidRow(MSDP_PO)
			# Row 2 (input widgets)
			MSDP_XT_SI <- column(2, selectInput(inputId="MSDP_XT", label="x-Transformation", choices=xtc, selected=xt))
			MSDP_YT_SI <- column(2, selectInput(inputId="MSDP_YT", label="y-Transformation", choices=ytc, selected=yt))
			if (is.none(mm) ) {
				row2 <- fluidRow(MSDP_XT_SI, MSDP_YT_SI)
			} else {
				m <- isolate(input$MSDP_M) %||% ses$r$model$symbol_list()[[1]]
				MSDP_M_SI <- column(4, selectInput(inputId="MSDP_M", label="Model", choices=mm, selected=m))
				MSS_MFO_CI <- column(3, checkboxInput(inputId="MSDP_MFO", label="Model Features only", value=TRUE))
				row2 <- fluidRow(MSDP_XT_SI, MSDP_YT_SI, MSDP_M_SI, MSS_MFO_CI)
			}
			withSpinner(div(row1, row2))
		})
  )
}
