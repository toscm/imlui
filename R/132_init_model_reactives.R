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
#' * `params`: list of reactives, model params for each symbol in `symbols()`
#' * `symbols_list`: reactive, list of symbols currently selected by the user
#' * `features_list`: reactive, list of feature name vectors corresponding to
#'   `symbols_list()`
#' * `params_list`: reactive, list of  model param vectors corresponding to
#'   `symbols_list()`
init_model_reactives <- function(input, rv) {
  logsne("Initializing model reactives ...")

  # Reactives depending only on `rv` that return vectors.
  ids <- reactive(get_accessible_model_ids(rv))
  symbols <- reactive(rv$db$models[ids(), "Symbol"])
  displaynames <- reactive(`names<-`(rv$db$models[ids(), "Name"], symbols()))
  pkgs <- reactive(`names<-`(rv$db$models[ids(), "Package"], symbols()))

  # Lists of reactives depending only on `rv`. Each reactive from the list
  # returns a vector.
  params <- clapply(symbols(), function(s) {
    reactive(getdata(sym = s, typ = "model", pkg = pkgs()[s]))
  })
  features <- clapply(symbols(), function(s) {
    reactive(names(params[[s]]()))
  })

  # Reactives depending on `rv` and `input` that return lists.
  symbols_list <- reactive(
    if (!is.null(input$model_names)) {
      symbols()[match(input$model_names, displaynames())]
    } else {
      character()
    }
  )
  params_list <- reactive(clapply(symbols_list(), function(m) params[[m]]()))
  features_list <- reactive(clapply(symbols_list(), function(m) features[[m]]()))

  return(function_locals())
}

get_accessible_model_ids <- function(rv) {
  valid_ids <- rv$db$models[nchar(rv$db$models$Symbol) > 0, "ID"]
  if (grepl("admin", rv$user$group_ids)) {
    return(valid_ids)
  } else {
    mgm <- rv$db$mapping_groups_models
    idx1 <- stringr::str_detect(rv$user$group_ids, pattern = mgm$group_id)
    ids1 <- mgm$model_id[idx1]
    mum <- rv$db$mapping_users_models
    idx2 <- mum$user_id == rv$user$id
    ids2 <- mum$model_id[idx2]
    authorized_ids <- unique(c(ids1, ids2))
    return(intersect(valid_ids, authorized_ids))
  }
}
