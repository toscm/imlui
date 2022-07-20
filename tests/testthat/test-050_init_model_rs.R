library("testthat")

withr::with_options(list(imlui.suppress_log_messages = TRUE), {

  # Stubs
  mock_model_names <- c(
    "Staiger 2020 Survival Signature (LAMIS)",
    "Seifert 2021 COO Signature"
  )
  mock_server_func <- function(input, output, session) {
    rv <- mock_rv_obj(user_id = "max")
    ret <- init_model_reactives(input, rv)
  }

  # Expected results
  ret_names <- c(
    "features_list", "betas_list", "symbols_list", "features", "betas",
    "pkgs", "displaynames", "symbols", "ids"
  )
  ret_symbols_list <- c("lamis_signature", "seifert_coo_signature")

  # Tests
  testServer(mock_server_func, {
    expect_equal(ret$symbols_list(), expected = character())
    expect_equal(names(ret), ret_names)
    session$setInputs(model_names = mock_model_names) # 1)
    expect_equal(object = ret$symbols_list(), ret_symbols_list)
    # 1) session object is provided by `testServer` func
  })
})
