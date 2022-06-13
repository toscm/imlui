init_prediction_reactives <- function(rv, model_reactives, dataset_reactives) {
  logsne("Initializing Prediction Reactives ...")

  # List of reactives. One reactive per prediction of a model for a dataset.
  # Access via prediction$model$dataset. Structure: list(list(reactive(num))).
  model_symbols <- function() {
    clapply(db$get_table("Models")$Symbol, function(m) {
      clapply(db$get_table("Datasets")$Symbol, function(d) {
        reactive({
          purrr::possibly(.f = predict, otherwise = NULL)( # 1
            ses$r$model$params[[m]](), df[[d]]()
          )
        })
      })
    })
  }
  # 1 Functions wrapped in `purrr::possible` return the value of
  # 1 argument `otherwise` if an error occurs during call of `.f`.

  # One reactive returning a list of all predictions. Access via:
  # YYY()[model][prediction]. Structure: reactive(list(list(num))).
  YYY <- reactive({
    clapply(ses$r$model$symbol_list(), function(M) {
      clapply(DD(), function(D) {
        YYYR[[M]][[D]]()
      })
    })
  })
}
