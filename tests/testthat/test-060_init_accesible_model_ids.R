library("testthat")

withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  mock_server_func <- function(input, output, session) {
    rv <- mock_rv_obj()
    ret <- reactive(init_accessible_model_ids(rv))
  }

  testServer(mock_server_func, {
    test_that("Correct IDs are returned for admins", {
      rv$user$id <- "toscm"
      rv$user$group_ids <- "admin;spang;public"
      expected <- c(
        "Seifert_2021_A", "Seifert_2021_B", "Nordmo_2020_A",
        "Nordmo_2020_B", "Reinders_2020", "Staiger_2020"
      )
      expect_equal(ret(), expected)
    })

    test_that("Correct IDs are returned for member of group 'spang'", {
      rv$user$id <- "max"
      rv$user$group_ids <- "spang;public"
      expected <- c(
        "Seifert_2021_A", "Seifert_2021_B", "Nordmo_2020_A",
        "Nordmo_2020_B", "Staiger_2020"
      )
      expect_equal(ret(), expected)
    })

    test_that("Correct IDs are returned for public user", {
      rv$user$id <- "public"
      rv$user$group_ids <- "public"
      expected <- c("Staiger_2020")
      expect_equal(ret(), expected)
    })

    test_that("An error is raised if `user$id` or `user$group_ids` == NULL", {
      rv$user$id <- NULL
      rv$user$group_ids <- NULL
      expect_error(ret())
    })
  })
})
