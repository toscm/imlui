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

init_dataset_reactives <- function(input, rv) {
  logsne("Initializing Dataset Reactives")

  # Reactives depending only on `rv` that return vectors.
  ids <- reactive(init_accessible_dataset_ids(rv))
  symbols <- reactive(rv$db$datasets[ids(), "Symbol"])
  displaynames <- reactive(`names<-`(rv$db$datasets[ids(), "Name"], symbols()))
  pkgs <- reactive(`names<-`(rv$db$datasets[ids(), "Package"], symbols()))
  transpose <- reactive(
    `names<-`(as.logical(rv$db$datasets[ids(), "Transpose"]), symbols())
  )

  # Lists of reactives (one element per dataset) depending only on `rv`. Access
  # via `<reactive>()[<dataset_symbol>]()`, e.g. `df()[["lamis_test1"]]()`.
  df <- reactive(
    clapply(symbols(), function(s) {
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
  )
  df_cat <- reactive(
    clapply(rv$db$datasets$Symbol, function(s) {
      reactive(select(df()[[s]](), where(is.factor))) # categorical values
    })
  )
  df_num <- reactive(
    clapply(rv$db$datasets$Symbol, function(s) {
      reactive(select(df()[[s]](), !where(is.factor))) # numerical values
    })
  )
  features <- reactive(
    clapply(symbols(), function(s) {
      reactive(colnames(df()[[s]]()))
    })
  )
  samples <- reactive(
    clapply(symbols(), function(s) {
      reactive(rownames(df()[[s]]()))
    })
  )

  # Reactives depending on `input` and `rv` that return lists.
  symbols_list <- reactive(
    if (!is.null(input$dataset_names)) {
      symbols()[match(input$dataset_names, displaynames())]
    } else {
      character()
    }
  )
  df_list <- reactive(
    clapply(symbols_list(), function(s) df()[[s]]())
  )
  df_cat_list <- reactive(
    clapply(XX(), function(X) select(X, where(is.factor)))
  )
  df_num_list <- reactive(
    clapply(XX(), function(X) select(X, !where(is.factor)))
  )
  samples_list <- reactive(
    clapply(symbols_list(), function(s) samples()[[s]]())
  )
  features_list <- reactive(
    clapply(symbols_list(), function(s) features()[[s]]())
  )

  return(function_locals())
}
