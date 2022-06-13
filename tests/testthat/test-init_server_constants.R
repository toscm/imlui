library("testthat")
withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  test_that("returned list has correctly named elements", {
    cat("\n")
    db <- mock_db_obj()
    session <- mock_session_obj()
    returned_obj <- init_server_constants(session, db)
    expected_names <- sort(c(
      "github_redirect_url",
      "settings",
      "shinygithub_oauth_app",
      "url",
      "url_hostname",
      "url_params",
      "url_pathname",
      "url_port",
      "url_protocol",
      "url_search"
    ))
    expect_equal(sort(names(returned_obj)), expected_names)
  })
})
