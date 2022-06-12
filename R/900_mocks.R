#' @title Return a MockShinySession object (for Testing Purposes)
#' @return R6 MockShinySession object
mock_session_obj <- function() {
  reactiveConsole(enabled=TRUE)
  session <- shiny::MockShinySession$new()
  session$clientData <- list(
    url_search = "",
    url_hash_initial = "",
    singletons = paste0(
      "add739c82ab207ed2c80be4b7e4b181525eb7a75,",
      "f756633fccc17f84dff1d8dbfc0a750cc0a8ff65,",
      "fc993cee573540ff31da45e9f921c83f789af40c"
    ),
    url_protocol = "http:",
    url_port = "8081",
    url_hostname = "localhost",
    pixelratio = 1,
    url_hash = "",
    url_pathname = "/"
  )
  session
}
mock_session_obj_mem <- memoise(mock_session_obj)

#' @title Return a shiny server input object (for Testing Purposes)
#' @param ... Properties of returned input object
#' @example input <- mock_input_obj(x = 1, y = 2)
#' input$x == 1 && input$y == 2
mock_input_obj <- function(...) {
  session <- mock_session_obj_mem()
  session$setInputs(...)
  input <- session$input
}

mock_output_obj <- function() {
  session <- mock_session_obj_mem()
  output <- session$output
}

mock_args <- function(func, ...) {
  default_args <- as.list(formals(func))
  stubbed_args <- modifyList(default_args, list(...))
  arg_names <- names(stubbed_args)
  for (i in seq_along(stubbed_args)) {
    assign(arg_names[[i]], eval(stubbed_args[[i]]), envir = globenv)
  }
}

#' @title Return a reactivevalues Object (for Testing Purposes)
#' @param user_id Either 'toscm', 'max', 'public' or NULL
mock_rvs_obj <- function(user_id = "max") {
  reactiveConsole(enabled = TRUE)
  rv <- reactiveValues(
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
    )
  )
  if (user_id == "toscm") {
    rv$user$id <- "toscm"
    rv$user$group_ids <- "admin;spang;public"
  } else if (user_id == "max") {
    rv$user$id <- "max"
    rv$user$group_ids <- "spang;public"
  } else if (user_id == "public") {
    rv$user$id <- "public"
    rv$user$group_ids <- "public"
  } else if (is.null(user_id)) {
    rv$user$id <- NULL
  } else {
    stop("user_id must be 'toscm', 'max', 'public' or NULL, not:", user_id)
  }
  return(rv)
}

mock_db_obj <- function() {
  if (is.null(cache_env$mock_db)) {
    cache_env$mock_db <- DB$new(
      config = list(
        dbms = list(
          type = "sqlite",
          filepath = system.file(
            "assets/sqlite/mock_imlui_db.sqlite", package = "imlui"
          )
        )
      )
    )
  }
  return(cache_env$mock_db$connect())
}
