#' @title Set auth state based on Github OAuth Token
#' @param ses List of session specific objects as returned by `init_data` once
#' for every user session
#' @details Should be called once at the beginning of the server function
act_set_auth_state_if_returning_from_github_login <- function(ses) {
  if (!is.null(rv$user$id)) {
    return()
    # early stop in case we're already authenticated, e.g. through cookie based
    # login or google login etc.
  }
  oa_token_present <- if (is.null(ses$const$url_params$code)) FALSE else TRUE
  infomsg("Checking if coming back from Github Auth...", oa_token_present)
  github_oauth2_access_token <- httr::oauth2.0_access_token(
    endpoint = httr::oauth_endpoints("github"),
    app = ses$const$shinygithub_oauth_app,
    code = ses$const$url_params$code
    # returns: list(access_token="asdf", scope="", token_type="bearer")
  )
  github_oauth2_token <- httr::oauth2.0_token(
    app = ses$const$shinygithub_oauth_app,
    endpoint = httr::oauth_endpoints("github"),
    credentials = github_oauth2_access_token,
    cache = FALSE
  )
  resp <- httr::GET(
    "https://api.github.com/user",
    httr::config(token = github_oauth2_token)
  )
  body <- httr::content(resp) # 1)
  if (httr::status_code(resp) == 200) {
    infomsg("Github login successful:")
    infomsg("login", body$login %||% "NULL")
    infomsg("id:", body$id %||% "NULL")
    infomsg("node_id:", body$node_id %||% "NULL")
    infomsg("login:", body$login %||% "NULL")
    infomsg("avatar_url:", body$avatar_url %||% "NULL")
    infomsg("name:", body$name %||% "NULL")
    infomsg("email:", body$email %||% "NULL")
    if (!(body$id %in% rv$const$db$users$github_id)) {
      # new user -> create new entry in DB
      new_user <- list(
        user_id = paste0("github_user_", body$id),
        group_ids = "standard",
        display_name = body$name,
        github_id = body$id,
        avatar_url = body$avatar_url
      )
      ses$db$execute(
        "INSERT INTO users
				(user_id, group_ids, display_name, github_id, avatar_url)
				VALUES ($1, $2, $3, $4, $5)",
        params = unname(new_user)
      )
      rv$const$db$users[new_user$user_id, names(new_user)] <<- new_user
    }
    idx <- which(rv$const$db$users$github_id == body$id) # which removes NAs
    rv$user$id <- rv$const$db$users$user_id[idx]
    set_login_cookie(
      db = ses$db,
      user_id = rv$user$id,
      session_id = ses$session$token
    )
    ses$rv$login_checked$github <- TRUE
  } else {
    shinyjs::show(id = "login_error")
  }
}

# 1) List of: 32
# Colnames: login, id, node_id, avatar_url, gravatar_id, url, html_url,
#           followers_url, following_url, gists_url, starred_url,
#           subscriptions_url, organizations_url, repos_url, events_url,
#           received_events_url, type, site_admin, name, company, blog,
#           location, email, hireable, bio, twitter_username, public_repos,
#           public_gists, followers, following, created_at, updated_at
# Important: login:      "toscm"
#            id:         "12760468"
# 			     node_id:    "MDQ6VXNlcjEyNzYwNDY4"
# 			     login:      "toscm"
#            avatar_url: "https://avatars.githubusercontent.com/u/12760468?v=4"
#            name:       "Tobias Schmidt"
#            email:      NULL
