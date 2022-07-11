library("testthat")

withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  mock_dataset_names <- c(
    "Staiger 2020 Test1 Dataset", "Nordmo 2020 Training Dataset"
  )
  mock_server_func <- function(input, output, session) {
    rv <- mock_rv_obj(user_id = "max")
    ret <- init_dataset_reactives(input, rv)
  }
  testServer(mock_server_func, {
    test_that("Returned object has correct names", {
      session$setInputs(dataset_names = mock_dataset_names)
      exp_ret_names <- c(
        "df", "df_cat", "df_cat_list", "df_list", "df_num", "df_num_list",
        "displaynames", "features", "features_list", "ids", "pkgs",
        "samples", "samples_list", "symbols", "symbols_list",
        "transpose"
      )
      expect_equal(sort(names(ret)), sort(exp_ret_names))
    })
    test_that("Reactive `ret$symbols_list()` returns the correct values.", {
      session$setInputs(dataset_names = mock_dataset_names)
      expect_equal(ret$symbols_list(), c("lamis_test1", "nordmo_train"))
    })
  })
})
