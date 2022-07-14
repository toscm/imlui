act_update_cookie_expiry_date <- function() { # run when_valid_cookie_is_present
  infomsg("Variable `input$login_jscookie` is available, starting authentication...")
  if (rv$user$is_authenticated) {
    infomsg("\tNothing to do. Already authenticated.")
  } else if (is.null(input$login_jscookie) || nchar(input$login_jscookie) == 0) {
    infomsg("\tValue of `input$login_jscookie` is NULL or '', stopping authentication.")
  } else {
    db_cookie_data <- {
      x <- db_get_valid_cookies()
      x[x$session_id == input$login_jscookie, ]
    }
    if (nrow(db_cookie_data) != 1) {
      # Invalid valid cookie found (or multiple, which should never occur), so remove it
      infomsg("\tCookie invalid, removing it...")
      shinyjs::js$rmcookie()
      infomsg("\tSome more logging info about the cookie:")
      infomsg("\t\tinput$login_jscookie:", input$login_jscookie)
      infomsg("\t\tRV$user$is_authenticated:", rv$user$is_authenticated)
      infomsg("\t\t!is.null(input$login_jscookie):", !is.null(input$login_jscookie))
      infomsg("\t\tnchar(input$login_jscookie) > 0:", nchar(input$login_jscookie) > 0)
      infomsg("\t\tdput(db_cookie_data):", . <- dput2(db_cookie_data))
    } else {
      # Valid cookie found, so get corresponding user_id, set reactives values user$id and
      # user$is_authenticated and then regenerate cookie to update expiry date
      rv$user$id <- db_cookie_data$user_id
      rv$user$is_authenticated <- TRUE
      set_login_cookie(db=db, user_id = rv$user$id, session_id = ses$session$token)
      infomsg("\tCookie valid, authenticated successfully as", rv$user$id, "...")
    }
  }
}