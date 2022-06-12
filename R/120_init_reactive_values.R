init_reactive_values <- function() {
  reactiveValues(
    user = list(
      id = NULL,
      group_ids = NULL,
      display_name = NULL,
      github_id = NULL,
      avatar_url = NULL,
      password = NULL,
      gitlab_id = NULL,
      google_id = NULL,
      spanglab_gitlab_id = NULL,
      spanglab_auth_id = NULL,
      is_authenticated = FALSE, # Deprecated. TODO: Remove
      cookie_already_checked = FALSE # Deprecated. TODO: Remove
    )
  )
}
