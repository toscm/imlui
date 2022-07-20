init_data <- function(input, output, session) {
  sid <- pkg$sid <- pkg$sid + 1
  db <- pkg$db # todo
  const <- init_server_constants(session, db) # ok: tests exist
  rv <- init_reactive_values(db) # ok: trivial
  r <- init_reactives(input, output, session, const, rv) # ok: trivial
  return(function_locals(strip_function_args = FALSE))
}
