init_reactive_values <- function(db) {
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
    ),
    login_checked = list(
      cookie = FALSE,
      google = FALSE,
      github = FALSE,
      gitlab = FALSE,
      auth_spang_lab = FALSE,
      gitlab_spang_lab = FALSE
    ),
    db = list(
      models = `rownames<-`(x <- db$get_table("Models"), x$ID),
      datasets = `rownames<-`(x <- db$get_table("Datasets"), x$ID),
      mapping_groups_models = db$get_table("mapping_groups_models"),
      mapping_users_models = db$get_table("mapping_users_models"),
      mapping_groups_datasets = db$get_table("mapping_groups_datasets"),
      mapping_users_datasets = db$get_table("mapping_users_datasets")
    )
  )
}
