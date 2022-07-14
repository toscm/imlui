act_store_appstate_in_db <- function() {
  # call from server func in onStop, i.e. will be called before onSessionEnded
  infomsg("Event onStop triggered in server: storing appstate as URL in DB...")
  user_id <- isolate(rv$user$id)
  if (!is.null(user_id)) {
    infomsg("\tuser_id:", user_id)
    infomsg("\tresource_id:", "url")
    infomsg("\tresource_value:", url)
    idx <- which(
      database$Appstate$user_id == user_id &
      database$Appstate$resource_id == "url"
    )
    if (length(idx) == 0) {
      query <- paste(
        "INSERT INTO Appstate",
        "(user_id, resource_id, resource_value)",
        "VALUES (?, ?, ?)"
      )
      infomsg("Calling:", query)
      infomsg("With params:", dput2(list(user_id, "url", url)))
      db_send(query, params = list(user_id, "url", url))
    } else {
      query <- paste(
        "UPDATE Appstate",
				"SET resource_value = ?",
				"WHERE user_id = ? AND resource_id = ?"
      )
      infomsg("Calling:", query)
      infomsg("With params:", dput2(list(url, user_id, "url")))
      db_send(query, params = list(url, user_id, "url")
      )
    }
  } else {
    infomsg("\tSkipped, no user logged in.")
  }
}
