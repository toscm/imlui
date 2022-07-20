init_rs_accessible_model_ids <- function(rv) {
  valid_ids <- rv$db$models[nchar(rv$db$models$Symbol) > 0, "ID"]
  if (is.null(rv$user)) {
    stop("rv$user must not be null")
  } else if (is.null(rv$user$group_ids)) {
    stop("rv$user$group_ids must not be null")
  } else if (grepl("admin", rv$user$group_ids)) {
    return(valid_ids)
  } else {
    mgm <- rv$db$mapping_groups_models
    idx1 <- stringr::str_detect(rv$user$group_ids, pattern = mgm$group_id)
    ids1 <- mgm$model_id[idx1]
    mum <- rv$db$mapping_users_models
    idx2 <- mum$user_id == rv$user$id
    ids2 <- mum$model_id[idx2]
    authorized_ids <- unique(c(ids1, ids2))
    return(intersect(valid_ids, authorized_ids))
  }
}
