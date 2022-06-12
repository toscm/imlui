db <- mock_db_obj()

test_that("Correct IDs are returned for admins", {
  user_id <- "toscm"
  group_ids <- "admin;spang;public"
  db <- db
  observed <- get_accessible_dataset_ids(user_id, group_ids, db)
  expected <- c(
    "Seifert_2021_A", "Seifert_2021_B", "Nordmo_2020_A",
    "Nordmo_2020_B", "Reinders_2020", "Staiger_2020"
  )
  expect_equal(observed, expected)
})

test_that("Correct IDs are returned for member of group 'spang'", {
  user_id <- "max"
  group_ids <- "spang;public"
  db <- db
  observed <- get_accessible_dataset_ids(user_id, group_ids, db)
  expected <- c(
    "Seifert_2021_A", "Seifert_2021_B", "Nordmo_2020_A",
    "Nordmo_2020_B", "Staiger_2020"
  )
  expect_equal(observed, expected)
})

test_that("Correct IDs are returned for public user", {
  user_id = "public"
  group_ids = "public"
  db = db
  observed <- get_accessible_dataset_ids(user_id, group_ids, db)
  expected <- c("Staiger_2020")
  expect_equal(observed, expected)
})
