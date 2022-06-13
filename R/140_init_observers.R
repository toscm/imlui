setup_observers <- function(data) {
  observeEvent(
    "run_only_once_at_start",
    set_user_to_authenticated_if_coming_back_from_github_oauth(data),
    once = TRUE
  )
  observeEvent(
    input$login_button,
    set_login_cookie_if_credentials_are_ok()
  )
  observeEvent(
    input$logout_button,
    remove_cookie_and_reload_session()
  )
  observeEvent(
    input$login_github_button,
    start_github_oauth2_flow()
  )
  observeEvent(
    eventExpr = ses$rv$user_id,
    handlerExpr = {
      if (ses$rv$user$id != "public") {
        restore_appstate(data)
      }
    }
  )
  observeEvent(
    session$clientData,
    print_url_parts(data),
    ignoreInit = TRUE
  )
  observeEvent(
    input$login_jscookie,
    update_cookie_expiry_date()
  )
  observe({
    reactiveValuesToList(input)
    # Not quite sure why this is necessary. Maybe to trigger this on any input
    # change, however if this is the case, converting all reactive values to
    # a list seems a bit costly in terms of runtime. Maybe improve later.
    session$doBookmark()
    # Update the query string (triggers onBookmark and onBookmarked callback
    # functions). Based on:
    # https://mastering-shiny.org/action-bookmark.html#action-bookmark
  })
  onBookmarked(
    function(url) {
      updateQueryString(queryString = url)
      ses$statics$url <- url
    }
  )
}

set_user_to_authenticated_if_coming_back_from_github_oauth <- function(data) {
  # Should be called once at the beginning of the server function
  logsne("Checking if coming back from Github Auth...")
  reload_on_logout <- TRUE
  if (is.null(url_params$code)) {
    logsne("\tNope, we don't...")
    return()
  }
  logsne("\tYes, we do. Fetching user info...")
  github_oauth2_access_token <- httr::oauth2.0_access_token(
    endpoint = httr::oauth_endpoints("github"),
    app = shinygithub_oauth_app,
    code = url_params$code
    # returns: list(access_token="asdf", scope="", token_type="bearer")
  )
  github_oauth2_token <- httr::oauth2.0_token(
    app = shinygithub_oauth_app,
    endpoint = httr::oauth_endpoints("github"),
    credentials = github_oauth2_access_token,
    cache = FALSE
  )
  resp <- httr::GET("https://api.github.com/user", httr::config(token = github_oauth2_token))
  body <- httr::content(resp)
  # List of: 32
  # Colnames: login, id, node_id, avatar_url, gravatar_id, url, html_url,
  #           followers_url, following_url, gists_url, starred_url,
  #           subscriptions_url, organizations_url, repos_url, events_url,
  #           received_events_url, type, site_admin, name, company, blog,
  #           location, email, hireable, bio, twitter_username, public_repos,
  #           public_gists, followers, following, created_at, updated_at
  # Important: login:      "toscm"
  #            id:         "12760468"
  # 			 node_id:    "MDQ6VXNlcjEyNzYwNDY4"
  # 			 login:      "toscm"
  #            avatar_url: "https://avatars.githubusercontent.com/u/12760468?v=4"
  #            name:       "Tobias Schmidt"
  #            email:      NULL
  if (httr::status_code(resp) == 200) {
    logsne("Github login successful:")
    logsne("\tlogin", body$login %||% "NULL")
    logsne("\tid:", body$id %||% "NULL")
    logsne("\tnode_id:", body$node_id %||% "NULL")
    logsne("\tlogin:", body$login %||% "NULL")
    logsne("\tavatar_url:", body$avatar_url %||% "NULL")
    logsne("\tname:", body$name %||% "NULL")
    logsne("\temail:", body$email %||% "NULL")
    if (!(body$id %in% database$users$github_id)) { # new user -> create new entry in DB
      new_user <- list(
        user_id = paste0("github_user_", body$id),
        group_ids = "standard",
        display_name = body$name,
        github_id = body$id,
        avatar_url = body$avatar_url
      )
      db_send(
        "INSERT INTO users
				(user_id, group_ids, display_name, github_id, avatar_url)
				VALUES ($1, $2, $3, $4, $5)",
        params = unname(new_user)
      )
      database$users[new_user$user_id, names(new_user)] <<- new_user
    }
    rv$user$id <- database$users$user_id[which(database$users$github_id == body$id)] # use which to get rid of NAs
    rv$user$is_authenticated <- TRUE # TODO: remove this. If user$id is NULL, we're not authenticated...
    x <- randomString()
    set_login_cookie(db=db, user_id = rv$user$id, session_id = x)
  } else {
    shinyjs::show(id = "login_error")
  }
}

remove_cookie_and_reload_session <- function(data) { # should be called after logout_button was clicked
  logsne("Event `input$logout_button` observed:", input$logout_button)
  shinyjs::js$rmcookie()
  session$reload()
}

restore_appstate <- function(data) {
  # should be called after user is authenticated successfully
  last_url <- x[x$user_id == rv$user$id & x$resource_id == "url", "resource_value"]
  logsne("Event `RV$user$is_authenticated` triggered ...")
  if (grepl("&redirect=false", url_search)) {
    logsne("\tURL parameter `redirect=false` is present, so do no more redirections")
  } else if (length(last_url) == 0) {
    logsne("\tNo last URL stored for user", rv$user$id, "so nothing to restore ...")
  } else {
    if (length(last_url) > 1) {
      logsne("\tWarning: multiple URLs found. Using the last one...")
    }
    new_url <- paste0(last_url[length(last_url)], "&redirect=false")
    logsne("\tRestoring previous appstate by redirecting to url:", new_url)
    output$web_app <- redirection_to(new_url)
  }
}

force_browser_to_set_login_jscookie_input <- function() {
  logsne("Javascript is ready. Calling `shinyjs::js$getcookie()` to set `input$login_jscookie` ...")
  shinyjs::js$getcookie()

  observeEvent(shiny::isTruthy(shinyjs::js$getcookie()), force_browser_to_set_login_jscookie_input)
}

update_cookie_expiry_date <- function() { # run when_valid_cookie_is_present
  logsne("Variable `input$login_jscookie` is available, starting authentication...")
  if (rv$user$is_authenticated) {
    logsne("\tNothing to do. Already authenticated.")
  } else if (is.null(input$login_jscookie) || nchar(input$login_jscookie) == 0) {
    logsne("\tValue of `input$login_jscookie` is NULL or '', stopping authentication.")
  } else {
    db_cookie_data <- {
      x <- db_get_valid_cookies()
      x[x$session_id == input$login_jscookie, ]
    }
    if (nrow(db_cookie_data) != 1) {
      # Invalid valid cookie found (or multiple, which should never occur), so remove it
      logsne("\tCookie invalid, removing it...")
      shinyjs::js$rmcookie()
      logsne("\tSome more logging info about the cookie:")
      logsne("\t\tinput$login_jscookie:", input$login_jscookie)
      logsne("\t\tRV$user$is_authenticated:", rv$user$is_authenticated)
      logsne("\t\t!is.null(input$login_jscookie):", !is.null(input$login_jscookie))
      logsne("\t\tnchar(input$login_jscookie) > 0:", nchar(input$login_jscookie) > 0)
      logsne("\t\tdput(db_cookie_data):", . <- dput2(db_cookie_data))
    } else {
      # Valid cookie found, so get corresponding user_id, set reactives values user$id and
      # user$is_authenticated and then regenerate cookie to update expiry date
      rv$user$id <- db_cookie_data$user_id
      rv$user$is_authenticated <- TRUE
      set_login_cookie(db=db, user_id = rv$user$id, session_id = randomString())
      logsne("\tCookie valid, authenticated successfully as", rv$user$id, "...")
    }
  }
}

set_login_cookie_if_credentials_are_ok <- function() {
  logsne("login button clicked:", input$login_button)
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
    set_login_cookie(db=db, user_id = rv$user$id, session_id = randomString())
    logsne("Authenticated successfully as", rv$user$id)
  } else {
    # if not valid temporarily show error logsne to user
    shinyjs::toggle(id = "login_error", anim = TRUE, time = 1, animType = "fade")
    shinyjs::delay(5000, shinyjs::toggle(id = "login_error", anim = TRUE, time = 1, animType = "fade"))
  }
}

start_github_oauth2_flow <- function() {
  # start OAuth2.0 authentication, i.e. redirect to
  # <https://github.com/login/oauth/{authorize/access_token}>
  logsne("input$login_button_github clicked:", input$login_button_github)
  url <- httr::oauth2.0_authorize_url(
    endpoint = httr::oauth_endpoints("github"),
    app = shinygithub_oauth_app,
    scope = ""
  )
  output$web_app <- redirection_to(url)
}

print_url_parts <- function() {
  logsne("session$clientData changed:")
  logsne("url_protocol:", session$clientData$url_protocol)
  logsne("url_hostname:", session$clientData$url_hostname)
  logsne("url_port:", session$clientData$url_port)
  logsne("url_pathname:", session$clientData$url_pathname)
  logsne("url_search:", session$clientData$url_search)
  logsne("url_hash_initial:", session$clientData$url_hash_initial)
  logsne("url_hash:", session$clientData$url_hash)
  logsne("output_web_app_width:", session$clientData$output_web_app_width)
  logsne("output_web_app_height:", session$clientData$output_web_app_height)
}

store_appstate_in_db <- function() {
	# call from server func in onStop, i.e. will be called before onSessionEnded
  logsne("Event onStop triggered in server: storing appstate as URL in DB...")
  user_id <- isolate(rv$user$id)
  if (!is.null(user_id)) {
    logsne("\tuser_id:", user_id)
    logsne("\tresource_id:", "url")
    logsne("\tresource_value:", url)
    idx <- which(database$Appstate$user_id == user_id & database$Appstate$resource_id == "url")
    if (length(idx) == 0) {
      logsne("Calling: INSERT INTO Appstate (user_id, resource_id, resource_value) VALUES (?, ?, ?)")
      logsne("With params:", dput2(list(user_id, "url", url)))
      db_send("INSERT INTO Appstate
								(user_id, resource_id, resource_value)
								VALUES (?, ?, ?)",
        params = list(user_id, "url", url)
      )
    } else {
      logsne("Calling: UPDATE Appstate SET resource_value = ? WHERE user_id = ? AND resource_id = ?")
      logsne("With params:", dput2(list(url, user_id, "url")))
      db_send("UPDATE Appstate
								SET resource_value = ?
								WHERE user_id = ? AND resource_id = ?",
        params = list(url, user_id, "url")
      )
    }
  } else {
    logsne("\tSkipped, no user logged in.")
  }
}
