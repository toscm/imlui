init_hndl <- function(ses) {
  init_hndl_auth(ses)
  init_hndl_btn(ses)
  init_hndl_sc(ses)
  init_hndl_exit(ses)
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
  observeEvent(
    ses$rv$user_id,
    if (ses$rv$user$id != "public") { act_restore_appstate(data) }
  )
  observeEvent(
    ses$session$clientData,
    print_url_parts(ses$session),
    ignoreInit = TRUE
  )
  observeEvent(
    ses$input$login_jscookie,
    act_update_cookie_expiry_date()
  )
  observeEvent({
    reactiveValuesToList(ses$input), #1)
    ses$session$doBookmark() #2)
  })
  onBookmarked(
    function(url) {
      updateQueryString(queryString = url)
      ses$statics$url <- url
    }
  )
}

# 1) Not quite sure why this is necessary. Maybe to trigger this on any input
# change, however if this is the case, converting all reactive values to a list
# seems a bit costly in terms of runtime. Maybe improve later.

# 2) Update the query string (triggers onBookmark and onBookmarked callback
# functions). Based on:
# https://mastering-shiny.org/action-bookmark.html#action-bookmark