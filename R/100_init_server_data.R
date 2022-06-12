init_server_data <- function(input, output, session) {
  db <- DB$new() # ok: tested dynamically
  const <- init_server_constants(session, db) # ok: unit test exists
  rv <- init_reactive_values() # ok: trivial
  r <- init_reactives(input, output, session, const, rv) # todo
  return(environment())
}
