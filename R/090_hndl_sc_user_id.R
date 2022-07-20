hndl_sc_user_id <- function(ses) {
  if (is.null(ses$rv$user$id) || ses$rv$user$id == "public") {
    return()
  } else {
    x <- ses$db$get_table("Appstate")
    idx <- x$user_id == rv$user$id & x$resource_id == "url"
    last_url <- x[idx, "resource_value"]
    if (grepl("&redirect=false", ses$const$url_search)) {
      debugmsg("URL contains `redirect=false`. Skipping redirection.")
    } else if (length(last_url) == 0) {
      debugmsg("No existing `Appstate$resource_value`. Skipping redirection.")
    } else {
      if (length(last_url) > 1) {
        warnmsg("Multiple URLs in `Appstate$resource_value`. Using latest.")
      }
      new_url <- paste0(last_url[length(last_url)], "&redirect=false")
      debugmsg("Restoring previous appstate by redirecting to url:", new_url)
    }
    ses$output$web_app <- redirection_to(new_url)
    # TODO: improve. Instead of redirecting, we should update the inputs directly
    # because redirecting has the overhead of going through the whole startup
    # process again.
  }
}
