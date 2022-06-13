library("testthat")
withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  test_that("Correct IDs are returned for admins", {
    rv <- mock_rv_obj(user_id = "toscm")
    observed <- get_accessible_model_ids(rv)
    expected <- c(
      "Seifert_2021_A", "Seifert_2021_B", "Nordmo_2020_A",
      "Nordmo_2020_B", "Reinders_2020", "Staiger_2020"
    )
    expect_equal(observed, expected)
  })

  test_that("Correct IDs are returned for member of group 'spang'", {
    rv <- mock_rv_obj(user_id = "max")
    observed <- get_accessible_model_ids(rv)
    expected <- c(
      "Seifert_2021_A", "Seifert_2021_B", "Nordmo_2020_A",
      "Nordmo_2020_B", "Staiger_2020"
    )
    expect_equal(observed, expected)
  })

  test_that("Correct IDs are returned for public user", {
    rv <- mock_rv_obj(user_id = "public")
    observed <- get_accessible_model_ids(rv)
    expected <- c("Staiger_2020")
    expect_equal(observed, expected)
  })
})
