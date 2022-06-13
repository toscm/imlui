tab__model_analysis <- function(data) {
  ########## MSDP ( Mean Standard Deviation Plot) (old name: Dataset Features Plot (DFP)) ############################
  MSDP <- reactive({
    list(
      dd = DD(),
      xx = NXX(),
      xt = input$MSDP_XT,
      yt = input$MSDP_YT,
      ff = if (not.none(input$MSDP_M) && input$MSDP_MFO) ses$r$model$features[[input$MSDP_M]]() else ""
    )
  })
  output$MSDP <- renderSVG(.name = "MSDP", .func = makeMSDPlot, .argfunc = MSDP)


  ########## FEP (Feature Effects Plot) ##############################################################################
  FEP <- reactive({
    .d <- input$FEP_D
    .s <- input$FEP_S
    .f <- input$FEP_F
    .v <- input$FEP_V
    .m <- if (not.none(ses$r$model$symbol_list())) ses$r$model$symbol_list()[[1]] else NULL
    b <- if (not.none(.m)) ses$r$model$params[[.m]]() else NULL
    X <- if (not.none(.d)) df[[.d]]() else NULL
    s <- if (not.none(.s) && not.none(X) && .s %in% rownames(X)) .s else NULL
    debug <- input$FEP_DBG
    n_extreme <- input$FEP_N
    threshold <- input$FEP_T
    list(b = b, X = X, s = s, debug = debug, n_extreme = n_extreme, threshold = threshold)
  })
  output$FEP <- renderSVG(.name = "FEP", .func = plot_feature_effects, .argfunc = FEP)


  ########## PHP (Predictions Histogram Plot) ########################################################################
  output$PHP <- renderImage(
    expr = {
      .arglist <- list(
        predictions = YYY()[[ses$r$model$symbol_list()[[1]]]],
        density_lines = input$PHP_DL,
        rug = input$PHP_R,
        binwidth = as.numeric(input$PHP_BW)
      )
      asSVG(.pname = "PHP", .func = plot_predictions_histogram, .arglist = .arglist)
    },
    deleteFile = FALSE
  )

  ########## SCP (Survival Curves Plot) ##############################################################################
  output$SCP <- renderImage(
    expr = {
      .arglist <- list(x = 1:10, main = "dummy")
      asSVG(.pname = "SCP", .func = plot, .arglist = .arglist)
    },
    deleteFile = FALSE
  )
}
