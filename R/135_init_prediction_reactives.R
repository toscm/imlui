init_prediction_reactives <- function() {
  logsne("Initializing Prediction Reactives ...")
  # List of reactives. One reactive per prediction. Access via:
  # YYYR[model][dataset](). Structure: list(list(reactive(num))).
  clapply(db$get_table("Models")$Symbol, function(m) {
    clapply(db$get_table("Datasets")$Symbol, function(d) {
      reactive({
        purrr::possibly(.f = predict, otherwise = NULL)(
          # Functions wrapped in `purrr::possible` return the value of
          # argument `otherwise` if an error occurs during call of `.f`.
          data$r$model$params[[m]](), df[[d]]()
        )
      })
    })
  })

  # One reactive returning a list of all predictions. Access via:
  # YYY()[model][prediction]. Structure: reactive(list(list(num))).
  YYY <- reactive({
    clapply(data$r$model$symbol_list(), function(M) {
      clapply(DD(), function(D) {
        YYYR[[M]][[D]]()
      })
    })
  })
}
