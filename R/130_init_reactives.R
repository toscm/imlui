init_reactives <- function(input, output, session, const, rv) {
  model <- init_model_reactives(input, rv, db) # ok: unit test exists
  dataset <- init_dataset_reactives(input, rv, db) # ok: unit test exists
  size <- init_size_reactives(input) # todo
  prediction <- init_prediction_reactives(input, rv, db) # todo
}





