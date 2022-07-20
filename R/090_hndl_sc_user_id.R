act_restore_appstate <- function(data) {
  # Should be called after user is authenticated successfully
  idx <- x$user_id == rv$user$id & x$resource_id == "url"
  last_url <- x[idx, "resource_value"]
  infomsg("Event `RV$user$is_authenticated` triggered ...")
  if (grepl("&redirect=false", url_search)) {
    infomsg("URL parameter `redirect=false` is present, so do no more redirections")
  } else if (length(last_url) == 0) {
    infomsg("No last URL stored for user", rv$user$id, "so nothing to restore ...")
  } else {
    if (length(last_url) > 1) {
      infomsg("Warning: multiple URLs found. Using the last one...")
    }
    new_url <- paste0(last_url[length(last_url)], "&redirect=false")
    infomsg("Restoring previous appstate by redirecting to url:", new_url)
    output$web_app <- redirection_to(new_url)
  }
  # TODO: improve. Instead of redirecting, we should update the inputs directly
  # because redirecting has the overhead of going through the whole startup
  # process again.
}
