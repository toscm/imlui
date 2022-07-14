act_start_github_oauth2_flow <- function() {
  # start OAuth2.0 authentication, i.e. redirect to
  # <https://github.com/login/oauth/{authorize/access_token}>
  infomsg("input$login_button_github clicked:", input$login_button_github)
  url <- httr::oauth2.0_authorize_url(
    endpoint = httr::oauth_endpoints("github"),
    app = ses$const$shinygithub_oauth_app,
    scope = ""
  )
  output$web_app <- redirection_to(url)
}
