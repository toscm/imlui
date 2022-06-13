library("testthat")
withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  test_that(
    desc = "Returned object has correct names",
    code = {
      input <- mock_input_obj(dataset_names = c(
        "Staiger 2020 Test1 Dataset",
        "Nordmo 2020 Training Dataset"
      ))
      rv <- mock_rv_obj(user_id = "max")
      ret <- init_dataset_reactives(input, rv)
      expect_equal(
        object = sort(names(ret)),
        expected = sort(
          c(
            "df", "df_cat", "df_cat_list", "df_list", "df_num", "df_num_list",
            "displaynames", "features", "features_list", "ids", "pkgs",
            "samples", "samples_list", "symbols", "symbols_list",
            "transpose"
          )
        )
      )
    }
  )
  testthat::test_that(
    desc = "Reactive `ret$symbols_list()` returns the correct values.",
    code = {
      input <- mock_input_obj(dataset_names = c(
        "Staiger 2020 Test1 Dataset",
        "Nordmo 2020 Training Dataset"
      ))
      rv <- mock_rv_obj(user_id = "max")
      ret <- init_dataset_reactives(input, rv)
      expect_equal(
        object = ret$symbols_list(),
        expected = c("lamis_test1", "nordmo_train")
      )
    }
  )
})
