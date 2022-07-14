act_set_auth_state_based_on_credentials <- function() {
  infomsg("login button clicked:", input$login_button)
  users_idx <- which(database$users$user_id == input$login_user_name)
  if (input$login_user_name %in% database$users$user_id) {
    user_exists <- TRUE
    user_password <- database$users$password[database$users$user_id == input$login_user_name]
    if (SODIUM_HASHED) {
      password_matches <- sodium::password_verify(user_password, input$login_password)
    } else {
      password_matches <- identical(user_password, input$login_password)
    }
  } else {
    user_exists <- FALSE
  }
  if (user_exists && password_matches) {
    rv$user$is_authenticated <- TRUE
    rv$user$id <- input$login_user_name
    set_login_cookie(db=db, user_id = rv$user$id, session_id = ses$session$token)
    infomsg("Authenticated successfully as", rv$user$id)
  } else {
    # if not valid temporarily show error logsne to user
    shinyjs::toggle(id = "login_error", anim = TRUE, time = 1, animType = "fade")
    shinyjs::delay(5000, shinyjs::toggle(id = "login_error", anim = TRUE, time = 1, animType = "fade"))
  }
}
