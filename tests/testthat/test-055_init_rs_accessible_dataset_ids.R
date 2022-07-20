library("testthat")

withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  mock_server_func <- function(input, output, session) {
    rv <- mock_rv_obj()
    ret <- reactive(init_accessible_dataset_ids(rv))
  }

  testServer(mock_server_func, {
    test_that("Correct IDs are returned for admins", {
      rv$user$id <- "toscm"
      rv$user$group_ids <- "admin;spang;public"
      expect_equal(ret(), c(
        "Nordmo_2020_Training_Dataset",
        "Seifert_2021_Training_Dataset",
        "Seifert_2021_Test_Dataset",
        "Staiger_2020_Training_Dataset",
        "Staiger_2020_Test1_Dataset",
        "Staiger_2020_Test2_Dataset"
      ))
    })
    test_that("Correct IDs are returned for member of group 'spang'", {
      rv$user$id <- "max"
      rv$user$group_ids <- "spang;public"
      expect_equal(ret(), c(
        "Nordmo_2020_Training_Dataset",
        "Seifert_2021_Training_Dataset",
        "Seifert_2021_Test_Dataset",
        "Staiger_2020_Training_Dataset",
        "Staiger_2020_Test1_Dataset"
      ))
    })
    test_that("Correct IDs are returned for public user", {
      rv$user$id <- "public"
      rv$user$group_ids <- "public"
      expect_equal(ret(), c(
        "Staiger_2020_Training_Dataset",
        "Staiger_2020_Test1_Dataset"
      ))
    })
    test_that("An error is raised if `user$id` or `user$group_ids` == NULL", {
      rv$user$id <- NULL
      rv$user$group_ids <- NULL
      expect_error(ret())
    })
  })
})
