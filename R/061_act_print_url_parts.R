act_print_url_parts <- function(session) {
  infomsg("session$clientData changed:")
  infomsg("url_protocol:", session$clientData$url_protocol)
  infomsg("url_hostname:", session$clientData$url_hostname)
  infomsg("url_port:", session$clientData$url_port)
  infomsg("url_pathname:", session$clientData$url_pathname)
  infomsg("url_search:", session$clientData$url_search)
  infomsg("url_hash_initial:", session$clientData$url_hash_initial)
  infomsg("url_hash:", session$clientData$url_hash)
  infomsg("output_web_app_width:", session$clientData$output_web_app_width)
  infomsg("output_web_app_height:", session$clientData$output_web_app_height)
}
