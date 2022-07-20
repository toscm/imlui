init_hndl_sc <- function(ses) {
  observeEvent(ses$rv$user_id, hndl_sc_user_id(ses))
  observeEvent(
    ses$session$clientData,
    print_url_parts(ses$session),
    ignoreInit = TRUE
  )
  observeEvent(
    ses$input$login_jscookie,
    act_update_cookie_expiry_date()
  )
}
