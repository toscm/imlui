srv_init_data <- function(input, output, session) {
  db <- DB$new() # ok: tested dynamically
  const <- init_server_constants(session, db) # ok: unit test exists
  rv <- init_reactive_values(db) # ok: trivial
  r <- init_reactives(input, output, session, const, rv) # todo
  return(environment())
}
