server <- function(input, output, session) {
	server__print_startup_message()
	data <- server__init_data(session)
	server__setup_observers(data)
	server__configure_bookmarking()
	output$webapp <- renderUI()
}

########## UI RENDERES #################################################################################################
	########## Webapp ##################################################################################################
		output$web_app <- renderUI({

		})
	########## Main Panel (layer 1 tabs) ###############################################################################
		# TODO: Database Overview - Tabset
		output$S_T <- renderUI({
			glueerr("isolate(input$S_T_CS): '{isolate(input$S_T_CS)}'")
			tabsetPanel(
				id="S_T_CS"
				, tabPanel("Paper",     uiOutput(outputId="S_PA_T"))
				, tabPanel("Models",    uiOutput(outputId="S_MO_T"))
				, tabPanel("Datasets",  uiOutput(outputId="S_DS_T"))
				# , tabPanel("Samples",   uiOutput(outputId="S_SA_T"))
				# , tabPanel("Datatypes", uiOutput(outputId="S_DT_T"))
				# , tabPanel("Methods",   uiOutput(outputId="S_ME_T"))
				# , tabPanel("Platforms", uiOutput(outputId="S_PF_T"))
				, tabPanel("Settings",  uiOutput(outputId="S_SE_T"))
			)
		})
		# OK: Data Analysis - Tabset
		output$DA_T <- renderUI({
			if (length(DD()) == 0) {
				div(HTML("Please choose a dataset first"))
			} else {
				glueerr("isolate(input$DA_T_CS):  '{isolate(input$DA_T_CS)}'")
				tabsetPanel(
					id="DA_T_CS", selected=isolate(input$DA_T_CS) %||% data$statics$settings[["DA_T_CS"]]
					, tabPanel("Descriptions", shinycssloaders::withSpinner(verbatimTextOutput(outputId="DA_D_T")))
					, tabPanel("MSD-Plot", shinycssloaders::withSpinner(uiOutput(outputId="DA_MSDP_T")))
				)
			}
		})
		# OK: Model Analysis - Tabset
		output$MA_T <- renderUI({
			if (length(MM()) == 0) {
				div(HTML("Please choose a model first"))
			} else {
				glueerr("isolate(input$MA_T_CS):  '{isolate(input$MA_T_CS)}'")
				tabsetPanel(
					id="MA_T_CS", selected=isolate(input$MA_T_CS) %||% data$statics$settings[["MA_T_CS"]]
					, tabPanel("Descriptions", verbatimTextOutput(outputId="MA_MD_TO"))
					, tabPanel("Predictions", uiOutput(outputId="MA_MP_T"))
					# , tabPanel("Survival Curves", uiOutput(outputId="MA_SC_T"))
					, tabPanel("Feature Effects", uiOutput(outputId="MA_FE_T"))
					# tabPanel("Feature Contributions", uiOutput(outputId="MA_FC_T"))
				)
			}
		})
	########## DBO Panel (layer 2 tabs) ################################################################################
		output$S_PA_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("S_PA_TBL") ) ), fluidRow( column( 12, DT::DTOutput("S_PA_TBL2") ) ) ) }) # Paper
		output$S_MO_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("S_MO_TBL") ) ), fluidRow( column( 12, DT::DTOutput("S_MO_TBL2") ) ) ) }) # Models
		output$S_DS_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("S_DS_TBL") ) ), fluidRow( column( 12, DT::DTOutput("S_DS_TBL2") ) ) ) }) # Datasets
		output$S_SA_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("S_SA_TBL") ) ), fluidRow( column( 12, DT::DTOutput("S_SA_TBL2") ) ) ) }) # Samples
		output$S_DT_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("S_DT_TBL") ) ), fluidRow( column( 12, DT::DTOutput("S_DT_TBL2") ) ) ) }) # Datatypes
		output$S_ME_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("S_ME_TBL") ) ), fluidRow( column( 12, DT::DTOutput("S_ME_TBL2") ) ) ) }) # Methods
		output$S_PF_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("S_PF_TBL") ) ), fluidRow( column( 12, DT::DTOutput("S_PF_TBL2") ) ) ) }) # Platforms
		output$S_SE_T <- renderUI({ div( fluidRow( column( 12, DT::DTOutput("S_SE_TBL") ) ), fluidRow( column( 12, DT::DTOutput("S_SE_TBL2") ) ) ) }) # Settings
	########## MA Panel (layer 2 tabs) #################################################################################
		# OK: Model Analysis - Survival Curves - Tab
		output$MA_SC_T <- renderUI({
			# logsne("MA_P_T: ")
			list(
				fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="SCP", height=PAHPX2(), width=PAWPX())))),
				fluidRow(column(3, selectInput(inputId="survPlotM", label="Model", choices=MODEL_SYMBOLS())))
			)
		})
		# OK: Model Analysis - Predictions - Tab
		output$MA_MP_T <- renderUI({
			# glueerr("MA_P_T: '{MM()}'")
			if (length(MM()) == 0) {
				div(HTML("Please choose a dataset and model first"))
			} else if (length(DD()) == 0) {
				div(HTML("Please choose a dataset and model first"))
			} else {
				div(fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="PHP", height=PAHPX1(), width=PAWPX() )))),
					fluidRow(column(2, checkboxInput(inputId="PHP_R", label="Rug", value=TRUE     )),
							column(2, checkboxInput(inputId="PHP_DL", label="Density", value=TRUE)),
							column(3, textInput(inputId="PHP_BW", label="Binwidth", value="0.1"  ))))
			}
		})
		# OK: Model Analysis - Survival Curvers - Tab
		output$MA_SC_T <- renderUI({
			# logsne("MA_P_T: ")
			list(
				fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(outputId="SCP", height=PAHPX2(), width=PAWPX())))),
				fluidRow(column(3, selectInput(inputId="survPlotM", label="Model", choices=MODEL_SYMBOLS())))
			)
		})
		# OK: Model Analysis - Feature Effects - Tab
		output$MA_FE_T <- renderUI({
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
				div(HTML("Usage of multiple models is not yet implemented"))
			}
		})
	########## DA Panel (layer 2 tabs) #################################################################################
		# TODO: Dataset Analysis - MSD-Plot - Tab
		output$DA_MSDP_T <- renderUI({
			# Vars
			xtc <- ytc <- c("identity", "log2", "log10")
			xt <- isolate(input$MSDP_XT) %||% "identity"
			yt <- isolate(input$MSDP_YT) %||% "identity"
			mm <- MM()
			# User info
			# logsne("DA_MSDP_T: ")
			# Row 1 (plot only)
			MSDP_PO <- column(12, shinycssloaders::withSpinner(plotOutput(outputId="MSDP", height=PAHPX1(), width=PAWPX() )))
			row1 <- fluidRow(MSDP_PO)
			# Row 2 (input widgets)
			MSDP_XT_SI <- column(2, selectInput(inputId="MSDP_XT", label="x-Transformation", choices=xtc, selected=xt))
			MSDP_YT_SI <- column(2, selectInput(inputId="MSDP_YT", label="y-Transformation", choices=ytc, selected=yt))
			if ( is.none(mm) ) {
				row2 <- fluidRow(MSDP_XT_SI, MSDP_YT_SI)
			} else {
				m <- isolate(input$MSDP_M) %||% MM()[[1]]
				MSDP_M_SI <- column(4, selectInput(inputId="MSDP_M", label="Model", choices=mm, selected=m))
				MSS_MFO_CI <- column(3, checkboxInput(inputId="MSDP_MFO", label="Model Features only", value=TRUE))
				row2 <- fluidRow(MSDP_XT_SI, MSDP_YT_SI, MSDP_M_SI, MSS_MFO_CI)
			}
			div(row1, row2)
		})

########## INPUT WIDGETS ###############################################################################################
	########## OK: Plot Area Size - Inputs #############################################################################
		output$PAS_W <- renderUI({
			widget1 <- list(
				selectInput(inputId="PASC", label="Plot Area Size", choices=c("Auto", "Fixed"),
							selected=if (is.null(PASC())) data$statics$settings[["PASC"]] else PASC() )
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
		output$FEP_S_SI <- renderUI({
			if (!is.null(input$FEP_D)) {
				column(2, selectInput(inputId="FEP_S", label="Sample", choices=SS()[[input$FEP_D]]))
			}
		})

	########## FIXME: Feature-Effects-Plot - Value - Slider-Input ######################################################
		output$FEP_V_SI <- renderUI({
			d <- input$FEP_D # dataset
			s <- input$FEP_S # sample
			f <- input$FEP_F # feature
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
		output$AI_TO <- renderUI({
			div( HTML( paste0(
				"<b>UserID:</b> ",         USER_ID(),                 "<br>",
				"<b>Version:</b> ",        V,                         "<br>",
				"<b>Browser Window:</b> ", BW(),  " x ", BH(), " px", "<br>",
				"<b>Main Panel:</b> ",     MPW(), " x ", MPH()," px", "<br>",
				"<b>Plot Area:</b> ",      PAW(), " x ", PAH()," px", "<br>",
				"<br>"
			) ) )
		})

	########## OK: Session-Info Text-Output ############################################################################
		output$SI_TO <- renderPrint({
			# logsne("SI_TO")
			catstr <- function(l, e) {
				s <- deparse(substitute(e))
				cat(glue("> {s}\n\n"))
				str(force(e), max.level=l)
				cat("\n\n")
			}
			# Runtime value of simple reactives
			catstr(1, input$FEP_D); catstr(1, input$FEP_F); catstr(1, input$FEP_S);
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
		output$DA_D_T <- renderPrint({
			# logsne(glue("DA_D_T: {collapseCS(DD())}"))
			walk(DD(), ~ cat(.x, "\n", describe_df(XXR[[.x]]()), "\n"))
			# for (i in seq_along(dfs)) {
			#     cat(names(dfs)[[i]], "\n", sep="")
			#     describe_df(dfs[[i]])
			#     cat("\n")
			# }
		})

	########## TODO: Model-Description Text-Output #####################################################################
		output$MA_MD_TO <- renderPrint({
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
	########## Overview Tables (Top) ###################################################################################
		output$S_PA_TBL <- render_dt(.tbl=database$Papers,   .invisible=c("ID", "Details", "Datasets", "Models", "Summary")) # Paper    , .visible=c("Year", "Author", "Title", "Summary")
		output$S_MO_TBL <- render_dt(.tbl=database$Models,   .invisible=c("ID", "Symbol", "Features", "Betas", "Modeltype")) # Models   , .visible=c("Platforms", "Methods", "Modeltype", "Symbol")
		output$S_DS_TBL <- render_dt(.tbl=database$Datasets, .invisible=c("Samples", "Symbol")                             ) # Datasets , .visible=c("N", "M")
		output$S_SA_TBL <- render_dt(.tbl=database$Samples                                                                 ) # Samples
		output$S_DT_TBL <- render_dt(.tbl=database$Datatypes                                                               ) # Datatypes
		output$S_ME_TBL <- render_dt(.tbl=database$Methods                                                                 ) # Methods
		output$S_PF_TBL <- render_dt(.tbl=database$Platforms                                                               ) # Platforms
		output$S_SE_TBL <- render_dt(.tbl=database$Settings                                                                ) # Settings
	########## Detail Tables (Top) #####################################################################################
		output$S_PA_TBL2 <- render_dt2(
			.name="Papers",
			.inputID="S_PA_TBL",
			.keep=c("ID", "Year", "Author", "Summary"),
			.collapse=c("Details"),
			.maps=c("Models", "Datasets")
		)
		output$S_MO_TBL2 <- render_dt2(
			.name="Models",
			.inputID="S_MO_TBL",
			.keep=c("ID", "Name", "Classes", "Methods", "Datatypes", "Platforms", "Modeltype", "Symbol"),
			.collapse=c("Features", "Betas"),
			.maps=c("Papers")
		)
		output$S_DS_TBL2 <- render_dt2(
			.name="Datasets",
			.inputID="S_DS_TBL",
			.keep=c("ID", "N", "M", "Symbol"),
			.collapse=c("Samples"),
			.maps=c("Papers")
		)
		# output$S_SA_TBL2 <- render_dt2(.tbl=database$Samples  ) # Samples
		# output$S_DT_TBL2 <- render_dt2(.tbl=database$Datatypes) # Datatypes
		# output$S_ME_TBL2 <- render_dt2(.tbl=database$Methods  ) # Methods
		# output$S_PF_TBL2 <- render_dt2(.tbl=database$Platforms) # Platforms
		# output$S_SE_TBL2 <- render_dt2(.tbl=database$Settings ) # Settings

	########## Editing Observers #######################################################################################
		observeEvent(input$S_PA_TBL_cell_edit, {database$Papers    <<- DT::editData(database$Papers,    input$S_PA_TBL_cell_edit, "S_PA_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$S_MO_TBL_cell_edit, {database$Models    <<- DT::editData(database$Models,    input$S_MO_TBL_cell_edit, "S_MO_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$S_DS_TBL_cell_edit, {database$Datasets  <<- DT::editData(database$Datasets,  input$S_DS_TBL_cell_edit, "S_DS_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$S_SA_TBL_cell_edit, {database$Samples   <<- DT::editData(database$Samples,   input$S_SA_TBL_cell_edit, "S_SA_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$S_DT_TBL_cell_edit, {database$Datatypes <<- DT::editData(database$Datatypes, input$S_DT_TBL_cell_edit, "S_DT_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$S_ME_TBL_cell_edit, {database$Methods   <<- DT::editData(database$Methods,   input$S_ME_TBL_cell_edit, "S_ME_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$S_PF_TBL_cell_edit, {database$Platforms <<- DT::editData(database$Platforms, input$S_PF_TBL_cell_edit, "S_PF_TBL", rownames=FALSE, resetPaging=FALSE)})
		observeEvent(input$S_SE_TBL_cell_edit, {database$Settings  <<- DT::editData(database$Settings,  input$S_SE_TBL_cell_edit, "S_SE_TBL", rownames=FALSE, resetPaging=FALSE)})





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
		output$S_PA_P <- renderImage(expr={.arglist <- list(x=1:10, main="S_PA_P"); asSVG(.pname="S_PA_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Paper
		output$S_MO_P <- renderImage(expr={.arglist <- list(x=1:10, main="S_MO_P"); asSVG(.pname="S_MO_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Models
		output$S_DS_P <- renderImage(expr={.arglist <- list(x=1:10, main="S_DS_P"); asSVG(.pname="S_DS_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Datasets
		output$S_SA_P <- renderImage(expr={.arglist <- list(x=1:10, main="S_SA_P"); asSVG(.pname="S_SA_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Samples
		output$S_DT_P <- renderImage(expr={.arglist <- list(x=1:10, main="S_DT_P"); asSVG(.pname="S_DT_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Datatypes
		output$S_ME_P <- renderImage(expr={.arglist <- list(x=1:10, main="S_ME_P"); asSVG(.pname="S_ME_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Methods
		output$S_PF_P <- renderImage(expr={.arglist <- list(x=1:10, main="S_PF_P"); asSVG(.pname="S_PF_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Platforms
		output$S_SE_P <- renderImage(expr={.arglist <- list(x=1:10, main="S_SE_P"); asSVG(.pname="S_SE_P", .func=plot, .arglist=.arglist)}, deleteFile=FALSE) # Settings


	########## MSDP ( Mean Standard Deviation Plot) (old name: Dataset Features Plot (DFP)) ############################
		AL$MSDP <- reactive({list(
			dd = DD(), xx = NXX(), xt = input$MSDP_XT, yt = input$MSDP_YT,
			ff = if ( not.none(input$MSDP_M) && input$MSDP_MFO ) FFR[[input$MSDP_M]]() else ""
		)})
		output$MSDP <- renderSVG(.name="MSDP", .func=makeMSDPlot, .argfunc=AL$MSDP)
		# output$MSDP <- renderImage(
		#     expr = {asSVG(.pname="MSDP", .func=makeMSDPlot, .arglist=MSDP_AL())},
		#     deleteFile=FALSE
		# )

	########## FEP (Feature Effects Plot) ##############################################################################
		AL$FEP <- reactive({
			.d <- input$FEP_D
			.s <- input$FEP_S
			.f <- input$FEP_F
			.v <- input$FEP_V
			.m <- if (not.none(MM())) MM()[[1]] else NULL
			b  <- if (not.none(.m)) PPR[[.m]]() else NULL
			X  <- if (not.none(.d)) XXR[[.d]]() else NULL
			s  <- if (not.none(.s) && not.none(X) && .s %in% rownames(X)) .s else NULL
			debug <- input$FEP_DBG
			n_extreme <- input$FEP_N
			threshold <- input$FEP_T
			list(b=b, X=X, s=s, debug=debug, n_extreme=n_extreme, threshold=threshold)
		})
		output$FEP <- renderSVG(.name="FEP", .func=plot_feature_effects, .argfunc=AL$FEP)
		# output$FEP <- renderImage(
		#     expr = asSVG(.pname="FEP", .func=plot_feature_effects, .arglist=AL$FEP()),
		#     deleteFile=FALSE
		# )

	########## PHP (Predictions Histogram Plot) ########################################################################
		output$PHP <- renderImage(
			expr = {
				.arglist <- list(
					predictions=YYY()[[MM()[[1]]]],
					density_lines=input$PHP_DL,
					rug=input$PHP_R,
					binwidth=as.numeric(input$PHP_BW)
				)
				asSVG(.pname="PHP", .func=plot_predictions_histogram, .arglist=.arglist)
			},
			deleteFile=FALSE
		)

	########## SCP (Survival Curves Plot) ##############################################################################
		output$SCP <- renderImage(
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
				user_id <- isolate(rv$user$id)
				if (!is.null(user_id)) {
					logsne("\tuser_id:", user_id)
					logsne("\tresource_id:", "url")
					logsne("\tresource_value:", url)
					idx <- which(database$Appstate$user_id == user_id & database$Appstate$resource_id == "url")
					if (length(idx) == 0) {
						logsne("Calling: INSERT INTO Appstate (user_id, resource_id, resource_value) VALUES (?, ?, ?)")
						logsne("With params:", dput2(list(user_id, "url", url)))
						db_send("INSERT INTO Appstate
								(user_id, resource_id, resource_value)
								VALUES (?, ?, ?)",
								params=list(user_id, "url", url)
						)
					} else {
						logsne("Calling: UPDATE Appstate SET resource_value = ? WHERE user_id = ? AND resource_id = ?")
						logsne("With params:", dput2(list(url, user_id, "url")))
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
