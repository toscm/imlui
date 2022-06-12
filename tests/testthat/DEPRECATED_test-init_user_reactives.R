test_that("Correct user info is returned before login", {
  # Inputs
  rv <- mock_rvs_obj()
  db <- mock_db_obj()
  # Globals
  logsne <- function(...) {}
  # Outputs
  expected <- NULL
  observed <- init_user_reactives(rv, db)
  # Checks
  expect_equal(observed, expected)
})
