init_user_reactives <- function(rv, db) {
  logsne("Initializing User Data Reactives")
  user <- reactive(db$get_table("users")[rv$user$id, ])
  # 1x9 or 0x9 data.frame
  # [1] user_id            group_ids
  # [3] display_name       github_id
  # [5] avatar_url         password
  # [7] gitlab_id          google_id
  # [9] spanglab_gitlab_id spanglab_auth_id
  return(user)
}
