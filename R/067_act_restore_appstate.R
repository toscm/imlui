act_restore_appstate <- function(data) {
  # should be called after user is authenticated successfully
  last_url <- x[x$user_id == rv$user$id & x$resource_id == "url", "resource_value"]
  infomsg("Event `RV$user$is_authenticated` triggered ...")
  if (grepl("&redirect=false", url_search)) {
    infomsg("\tURL parameter `redirect=false` is present, so do no more redirections")
  } else if (length(last_url) == 0) {
    infomsg("\tNo last URL stored for user", rv$user$id, "so nothing to restore ...")
  } else {
    if (length(last_url) > 1) {
      infomsg("\tWarning: multiple URLs found. Using the last one...")
    }
    new_url <- paste0(last_url[length(last_url)], "&redirect=false")
    infomsg("\tRestoring previous appstate by redirecting to url:", new_url)
    output$web_app <- redirection_to(new_url)
  }
}
