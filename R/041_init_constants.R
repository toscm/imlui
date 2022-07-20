init_server_constants <- function(session, db) {
  infomsg("Parsing session URL ...")
  x <- session$clientData
  url_search <- isolate(x$url_search)
  url_protocol <- isolate(x$url_protocol) # eg. "http:", "https:"
  url_hostname <- isolate(x$url_hostname) # eg. "localhost", "abc.spang-lab.de"
  url_port <- isolate(x$url_port) # eg. 443, 80, 99 (https, http, custom)
  url_pathname <- isolate(x$url_pathname) # eg. "/", "/shinyserver/imlui"
  url_params <- parseQueryString(url_search)
  url <- paste0(
    url_protocol, "//", url_hostname, ":", url_port, url_pathname,
    url_search
  )
  github_redirect_url <- paste0(
    url_protocol, "//", url_hostname, ":", url_port, url_pathname
  )
  shinygithub_oauth_app <- httr::oauth_app(
    appname = "shinygithub", # 1
    key = "51d46f96810d1fd182a2", # 1
    secret = "66eec8782825eeb61007dbef32f91afc9c3587aa", # 1
    redirect_uri = github_redirect_url
    # 1 TODO: take from database
  )
  infomsg("URL:", url)

  infomsg("Generating init values from database...")
  settings <- local({
    df <- db$get_table("settings")
    x <- df$Value
    names(x) <- df$ID
    x
  })

  return(function_locals(without = "x"))
}
