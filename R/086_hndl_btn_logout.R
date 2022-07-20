hook_act_remove_cookie_and_reload_session <- function(data) {
  # should be called after logout_button was clicked
  infomsg("Event `input$logout_button` observed:", input$logout_button)
  shinyjs::js$rmcookie()
  session$reload()
}
