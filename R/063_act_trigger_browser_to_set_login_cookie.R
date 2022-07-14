trigger_browser_to_set_login_cookie <- function() {
  infomsg(
    "Javascript is ready. Calling `shinyjs::js$getcookie()`",
    "to set `input$login_jscookie` ..."
  )
  shinyjs::js$getcookie()
}
