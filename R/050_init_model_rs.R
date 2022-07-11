#' @title Return list of reactives related to Model Metadata
#' @param input Input object as handed over to server functions by shiny
#' @param rv List of reactive values as returned by
#' [init_reactive_values()]
#' @return list with following elements:
#' * `ids`: reactive, all elements of `tbl$ID` that may be accessed by the
#'   current user
#' * `symbols`: reactive, `R` objects corresponding to `ids()`
#' * `displaynames`: reactive, display names corresponding to `ids()`
#' * `pkgs`: reactive, R packages providing `symbols`
#' * `features`: list of reactives, feature names for each symbol in `symbols()`
#' * `betas`: list of reactives, model betas for each symbol in `symbols()`
#' * `symbols_list`: reactive, list of symbols currently selected by the user
#' * `features_list`: reactive, list of feature name vectors corresponding to
#'   `symbols_list()`
#' * `betas_list`: reactive, list of  model param vectors corresponding to
#'   `symbols_list()`
init_model_reactives <- function(input, rv) {
  logsne("Initializing model reactives ...")

  # Reactives depending only on `rv` that return vectors.
  ids <- reactive(init_accessible_model_ids(rv))
  symbols <- reactive(rv$db$models[ids(), "Symbol"])
  displaynames <- reactive(`names<-`(rv$db$models[ids(), "Name"], symbols()))
  pkgs <- reactive(`names<-`(rv$db$models[ids(), "Package"], symbols()))

  # Reactives depending only on `rv` that return lists of reactives. (Access via
  # betas()[["<symbol>"]]()
  betas <- reactive(
    clapply(symbols(), function(s) {
      reactive(getdata(sym = s, typ = "model", pkg = pkgs()[s]))
    })
  )
  features <- reactive(
    clapply(symbols(), function(s) {
      reactive(names(betas()[[s]]()))
    })
  )

  # Reactives depending on `rv` and `input` that return lists.
  symbols_list <- reactive(
    if (!is.null(input$model_names)) {
      symbols()[match(input$model_names, displaynames())]
    } else {
      character()
    }
  )
  betas_list <- reactive(clapply(symbols_list(), function(m) betas()[[m]]()))
  features_list <- reactive(clapply(symbols_list(), function(m) features()[[m]]()))

  return(function_locals())
}
