library("testthat")

# Inputs
db <- mock_db_obj()
input <- mock_input_obj(
  model_names = c(
    "Staiger 2020 Survival Signature (LAMIS)",
    "Seifert 2021 COO Signature"
  )
)
rv <- mock_rvs_obj(user_id = "max")

# Outputs
ret <- suppressMessages(init_model_reactives(input, rv, db))

# Tests
testthat::test_that(
  desc = "Returned object has correct names",
  code = {
    expect_equal(
      object = names(ret),
      expected = c(
        "features_list", "params_list", "symbols_list", "features", "params",
        "pkgs", "displaynames", "symbols", "ids", "tbl"
      )
    )
  }
)
testthat::test_that(
  desc = "Reactive `ret$symbols_list()` returns the correct values.",
  code = {
    expect_equal(
      object = ret$symbols_list(),
      expected = c("lamis_signature", "seifert_coo_signature")
    )
  }
)

