#' @title Return list of reactives related to Dataset Metadata
#' @param input Input object as handed over to server functions by shiny
#' @param rv List of reactive values as returned by
#' [init_reactive_values()]
#' @param db DB object as returned by `DB$new()`
#' @return list with following reactive elements:
#' tbl, ids, symbols, displaynames, pkgs, transpose
#'    reactives returning vectors of all ids/symbols/displaynames/...
#'    accessible by the current user
#' df, df_cat, df_num, features, samples
#'    lists of reactives (one reactive per dataset)
#' symbols_list, df_list, df_cat_list, df_num_list, samples_list, features_list
#'    reactives returning vectors of ids/symbols/displaynames/...
#'    selected by the current user

init_dataset_reactives <- function(input, rv, db) {
  logsne("Initializing Dataset Reactives")

  # Reactives depending only on `rv$user$id` and `db`
  tbl <- reactive(`rownames<-`(x <- db$get_table("Datasets"), x$ID))
  ids <- reactive(get_accessible_dataset_ids(
    user_id = rv$user$id,
    group_ids = rv$user$group_ids,
    db = db
  ))
  symbols <- reactive(tbl()[ids(), "Symbol"])
  displaynames <- reactive(`names<-`(tbl()[ids(), "Name"], symbols()))
  pkgs <- reactive(`names<-`(tbl()[ids(), "Package"], symbols()))
  transpose <- reactive(
    `names<-`(as.logical(tbl()[ids(), "Transpose"]), symbols())
  )

  # Lists of reactives (one element per dataset) depending only on `rv$user$id`
  # and `db`. Access via `<reactive>[<dataset_symbol>]()`, e.g.
  # `df["lamis_test1"]()`.
  df <- clapply(symbols(), function(s) {
    reactive({
      x <- getdata( # dataframe of observed values
        sym = s,
        typ = "dataset",
        pkg = pkgs()[s],
        transpose = transpose()[s]
      )
      x <- do.call(dplyr::rename, list(x, FEATURE_MAPPINGS[[s]]))
    })
  })
  df_cat <- clapply(tbl()$Symbol, function(s) {
    reactive(select(df[[s]](), where(is.factor))) # categorical values
  })
  df_num <- clapply(tbl()$Symbol, function(s) {
    reactive(select(df[[s]](), !where(is.factor))) # numerical values
  })
  features <- clapply(symbols(), function(s) {
    reactive(colnames(df[[s]]()))
  })
  samples <- clapply(symbols(), function(s) {
    reactive(rownames(df[[s]]()))
  })

  # Reactives depending on `input`, `rv$user$id` and `db` that return lists.
  symbols_list <- reactive(
    if (!is.null(input$dataset_names)) {
      symbols()[match(input$dataset_names, displaynames())]
    } else {
      character()
    }
  )
  df_list <- reactive(
    clapply(symbols_list(), function(s) df[[s]]())
  )
  df_cat_list <- reactive(
    clapply(XX(), function(X) select(X, where(is.factor)))
  )
  df_num_list <- reactive(
    clapply(XX(), function(X) select(X, !where(is.factor)))
  )
  samples_list <- reactive(
    clapply(symbols_list(), function(s) samples[[s]]())
  )
  features_list <- reactive(
    clapply(symbols_list(), function(s) features[[s]]())
  )

  # Return current environment without functions args as list
  x <- as.list(environment())
  x[names(formals(init_dataset_reactives))] <- NULL
  return(x)
}

get_accessible_dataset_ids <- function(user_id, group_ids, db) {
  valid_ids <- db$execute("SELECT ID FROM Datasets WHERE Length(Symbol) > 0")$ID
  if (grepl("admin", group_ids)) {
    return(valid_ids)
  } else {
    mgd <- db$get_table("mapping_groups_datasets")
    idx1 <- stringr::str_detect(group_ids, pattern = mgd$group_id)
    ids1 <- mgd$dataset_id[idx1]
    mud <- db$get_table("mapping_users_datasets")
    idx2 <- mud$user_id == user_id
    ids2 <- mud$dataset_id[idx2]
    authorized_ids <- unique(c(ids1, ids2))
    return(intersect(valid_ids, authorized_ids))
  }
}
