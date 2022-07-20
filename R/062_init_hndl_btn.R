init_hndl_btn <- function(ses) {
  observeEvent(
    ses$input$login_button,
    act_set_auth_state_based_on_credentials()
  )
  observeEvent(
    ses$input$logout_button,
    act_remove_cookie_and_reload_session()
  )
  observeEvent(
    ses$input$login_github_button,
    act_start_github_oauth2_flow()
  )
}
