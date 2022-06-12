#' @title Return list of reactives related to Model Metadata
#' @param input Input object as handed over to server functions by shiny
#' @param rv List of reactive values as returned by
#' [init_reactive_values()]
#' @param db DB object as returned by `DB$new()`
#' @return list with following reactive elements:
#' tbl: "Models" table fetched from `db`
#' ids: all elements of `tbl$ID` that may be accessed by the current user
#' symbols: objects corresponding to `ids`
#' names: display names corresponding to `ids`
#' pkgs: R packages providing `symbols`
#' features: features[<symbol>] == feature names corresponding to `<symbol>`
#' params: features[<symbol>] == model params corresponding to `<symbol>`
#' symbols_list: list of symbols currently selected by the user
#' features_list: list of feature name vectors corresponding to symbols_list
#' params_list: list of  model param vectors corresponding to symbols_list
init_model_reactives <- function(input, rv, db) {

  logsne("Initializing model reactives ...")

  # Reactives depending only on `rv$user$id` and `db`. `params` and `features` are
  # lists of reactives (one per model symbol).
  tbl <- reactive(`rownames<-`(x <- db$get_table("Models"), x$ID))
  ids <- reactive(get_accessible_model_ids(
    user_id = rv$user$id,
    group_ids = rv$user$group_ids,
    db = db
  ))
  symbols <- reactive(tbl()[ids(), "Symbol"])
  displaynames <- reactive(`names<-`(tbl()[ids(), "Name"], symbols()))
  pkgs <- reactive(`names<-`(tbl()[ids(), "Package"], symbols()))
  params <- clapply(symbols(), function(s) {
    reactive(getdata(sym = s, typ = "model", pkg = pkgs()[s]))
  })
  features <- clapply(symbols(), function(s) {
    reactive(names(params[[s]]()))
  })

  # Reactives depending on `input`, `rv$user$id` and `db` that return lists.
  symbols_list <- reactive( # list(char)
    if (!is.null(input$model_names)) {
      symbols()[match(input$model_names, displaynames())]
    } else {
      character()
    }
  )
  params_list <- reactive(clapply(symbols_list(), function(m) params[[m]]()))
  features_list <- reactive(clapply(symbols_list(), function(m) features[[m]]()))

  # Return current environment without functions args as list
  x <- as.list(environment())
  x[names(formals(init_model_reactives))] <- NULL
  return(x)
}

get_accessible_model_ids <- function(user_id, group_ids, db) {
  valid_ids <- db$execute("SELECT ID FROM Models WHERE Length(Symbol) > 0")$ID
  if (grepl("admin", group_ids)) {
    return(valid_ids)
  } else {
    mgm <- db$get_table("mapping_groups_models")
    idx1 <- stringr::str_detect(group_ids, pattern = mgm$group_id)
    ids1 <- mgm$model_id[idx1]
    mum <- db$get_table("mapping_users_models")
    idx2 <- mum$user_id == user_id
    ids2 <- mum$model_id[idx2]
    authorized_ids <- unique(c(ids1, ids2))
    return(intersect(valid_ids, authorized_ids))
  }
}
