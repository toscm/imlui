init_hndl_bookmark <- function(ses) {
  observeEvent(
    reactiveValuesToList(ses$input), # 1)
    ses$session$doBookmark() # 2)
  )
  onBookmarked(
    function(url) {
      updateQueryString(queryString = url)
      ses$statics$url <- url
    }
  )

  # 1) Not quite sure why this is necessary. Maybe to trigger this on any input
  # change, however if this is the case, converting all reactive values to a
  # list seems a bit costly in terms of runtime. Maybe improve later.

  # 2) Update the query string (triggers onBookmark and onBookmarked callback
  # functions). Based on:
  # https://mastering-shiny.org/action-bookmark.html#action-bookmark
}
