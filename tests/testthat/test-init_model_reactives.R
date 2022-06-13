library("testthat")
withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  testthat::test_that(
    desc = "Returned object has correct names",
    code = {
      input <- mock_input_obj(model_names = c(
        "Staiger 2020 Survival Signature (LAMIS)",
        "Seifert 2021 COO Signature"
      ))
      rv <- mock_rv_obj(user_id = "max")
      ret <- suppressMessages(init_model_reactives(input, rv))
      expect_equal(
        object = names(ret),
        expected = c(
          "features_list", "params_list", "symbols_list", "features", "params",
          "pkgs", "displaynames", "symbols", "ids"
        )
      )
    }
  )
  testthat::test_that(
    desc = "Reactive `ret$symbols_list()` returns the correct values.",
    code = {
      input <- mock_input_obj(model_names = c(
        "Staiger 2020 Survival Signature (LAMIS)",
        "Seifert 2021 COO Signature"
      ))
      rv <- mock_rv_obj(user_id = "max")
      ret <- suppressMessages(init_model_reactives(input, rv))
      expect_equal(
        object = ret$symbols_list(),
        expected = c("lamis_signature", "seifert_coo_signature")
      )
    }
  )
})
