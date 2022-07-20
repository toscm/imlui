init_hndl_oauth <- function(ses) {
  observeEvent(eventExpr = 1, once = TRUE, handlerExpr = {
    hndl_oauth_github(ses)
    hndl_oauth_auth_spang_lab(ses)
    hndl_oauth_google(ses)
    hndl_oauth_gitlab(ses)
    hndl_oauth_gitlab_spang_lab(ses)
  })
}
