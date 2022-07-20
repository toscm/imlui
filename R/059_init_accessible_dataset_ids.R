init_accessible_dataset_ids <- function(rv) {
  valid_ids <- rv$db$datasets[nchar(rv$db$datasets$Symbol) > 0, "ID"]
  if (is.null(rv$user)) {
    stop("rv$user must not be null")
  } else if (is.null(rv$user$group_ids)) {
    stop("rv$user$group_ids must not be null")
  } else if (grepl("admin", rv$user$group_ids)) {
    return(valid_ids)
  } else {
    mgd <- rv$db$mapping_groups_datasets
    idx1 <- stringr::str_detect(rv$user$group_ids, pattern = mgd$group_id)
    ids1 <- mgd$dataset_id[idx1]
    mud <- rv$db$mapping_users_datasets
    idx2 <- mud$user_id == rv$user$id
    ids2 <- mud$dataset_id[idx2]
    authorized_ids <- unique(c(ids1, ids2))
    return(intersect(valid_ids, authorized_ids))
  }
}
