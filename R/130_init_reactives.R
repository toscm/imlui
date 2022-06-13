init_reactives <- function(input, output, session, const, rv) {
  model <- init_model_reactives(input, rv) # ok: unit test exists
  dataset <- init_dataset_reactives(input, rv) # ok: unit test exists
  size <- init_size_reactives(input) # ok: unit test exists
  prediction <- init_prediction_reactives(rv) # todo
  return(function_locals())
}





