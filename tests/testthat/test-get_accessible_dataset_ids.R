library("testthat")
withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  test_that("Correct IDs are returned for admins", {
    rv <- mock_rv_obj(user_id = "toscm")
    observed <- get_accessible_dataset_ids(rv)
    expected <- c(
      "Nordmo_2020_Training_Dataset",
      "Seifert_2021_Training_Dataset",
      "Seifert_2021_Test_Dataset",
      "Staiger_2020_Training_Dataset",
      "Staiger_2020_Test1_Dataset",
      "Staiger_2020_Test2_Dataset"
    )
    expect_equal(observed, expected)
  })

  test_that("Correct IDs are returned for member of group 'spang'", {
    rv <- mock_rv_obj(user_id = "max")
    observed <- get_accessible_dataset_ids(rv)
    expected <- c(
      "Nordmo_2020_Training_Dataset",
      "Seifert_2021_Training_Dataset",
      "Seifert_2021_Test_Dataset",
      "Staiger_2020_Training_Dataset",
      "Staiger_2020_Test1_Dataset"
    )
    expect_equal(observed, expected)
  })

  test_that("Correct IDs are returned for public user", {
    rv <- mock_rv_obj(user_id = "public")
    observed <- get_accessible_dataset_ids(rv)
    expected <- c("Staiger_2020_Training_Dataset", "Staiger_2020_Test1_Dataset")
    expect_equal(observed, expected)
  })
})
