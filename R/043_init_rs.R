init_reactives <- function(input, output, session, const, rv) {
  model <- init_rs_model(input, rv) # ok: unit test exists
  dataset <- init_rs_dataset(input, rv) # ok: unit test exists
  size <- init_rs_size(input) # ok: unit test exists
  prediction <- init_rs_prediction(rv) # todo
  return(function_locals())
}
