library("testthat")

withr::with_options(list(imlui.suppress_log_messages = TRUE), {
  mock_server_func <- function(input, output, session) {
    ret <- init_size_reactives(input)
  }

  testServer(mock_server_func, {
    testthat::test_that("Returned object has correct names", {
      session$setInputs(
        dim = c(789, 678), MPAW = 840, MPAH = 600, PASC = "Fixed"
      )
      expected_names <- sort(c(
        "PAHPX3", "PAHPX2", "PAHPX1_05", "PAHPX1", "PAHPX0", "PAWPX", "PAW",
        "PAH", "MPW", "MPH", "APAW", "APAH", "PASC", "BH", "BW", "MPAH", "MPAW"
      ))
      expect_true(all(sort(names(ret)) == expected_names))
    })
    testthat::test_that("Return values are ok when `input$PASC == 'Auto'`.", {
      session$setInputs(
        dim = c(789, 678), MPAW = 840, MPAH = 600, PASC = "Auto"
      )
      expected_names <- c(
        "PAHPX3", "PAHPX2", "PAHPX1_05", "PAHPX1", "PAHPX0", "PAWPX", "PAW",
        "PAH", "MPW", "MPH", "APAW", "APAH", "PASC", "BH", "BW", "MPAH", "MPAW"
      )
      expect_equal(ret$MPAW(), 840)
      expect_equal(ret$MPAH(), 600)
      expect_equal(ret$BW(), 789)
      expect_equal(ret$BH(), 678)
      expect_equal(ret$PASC(), "Auto")
      expect_equal(ret$MPW(), 591.75) # 789 * 0.75
      expect_equal(ret$MPH(), 636) # 678 - 42.0
      expect_equal(ret$APAW(), 540) # c(240<..<480<540<x<600<..<1920), x=PAW
      expect_equal(ret$APAH(), 480) # c(240<..<480<540<600<x<..<1920), x=PAH-120
      expect_equal(ret$PAW(), 540) # if (PASC == Fixed) MPW else APAW
      expect_equal(ret$PAH(), 480) # if (PASC == Fixed) MPH else APAH
      expect_equal(ret$PAWPX(), "540px") # "%spx", PAW
      expect_equal(ret$PAHPX0(), "480px") # "%spx", PAH
      expect_equal(ret$PAHPX1(), "360px") # "%spx", PAH - 120
      expect_equal(ret$PAHPX1_05(), "180px") # "%spx", (PAH - 120) * 0.5
      expect_equal(ret$PAHPX2(), "240px") # "%spx", PAH - 240
      expect_equal(ret$PAHPX3(), "120px") # "%spx", PAH - 360
    })
    testthat::test_that("Return values are ok when `input$PASC == 'Fixed'`.", {
      session$setInputs(
        dim = c(789, 678), MPAW = 840, MPAH = 600, PASC = "Fixed"
      )
      expected_names <- c(
        "PAHPX3", "PAHPX2", "PAHPX1_05", "PAHPX1", "PAHPX0", "PAWPX", "PAW",
        "PAH", "MPW", "MPH", "APAW", "APAH", "PASC", "BH", "BW", "MPAH", "MPAW"
      )
      expect_equal(ret$MPAW(), 840)
      expect_equal(ret$MPAH(), 600)
      expect_equal(ret$BW(), 789)
      expect_equal(ret$BH(), 678)
      expect_equal(ret$PASC(), "Fixed")
      expect_equal(ret$MPW(), 591.75) # 789 * 0.75
      expect_equal(ret$MPH(), 636) # 678 - 42.0
      expect_equal(ret$APAW(), 540) # c(240<..<480<540<x<600<..<1920), x=PAW
      expect_equal(ret$APAH(), 480) # c(240<..<480<540<600<x<..<1920), x=PAH-120
      expect_equal(ret$PAW(), 840) # if (PASC == Fixed) MPW else APAW
      expect_equal(ret$PAH(), 600) # if (PASC == Fixed) MPH else APAH
      expect_equal(ret$PAWPX(), "840px") # "%spx", PAW
      expect_equal(ret$PAHPX0(), "600px") # "%spx", PAH
      expect_equal(ret$PAHPX1(), "480px") # "%spx", PAH - 120
      expect_equal(ret$PAHPX1_05(), "240px") # "%spx", (PAH - 120) * 0.5
      expect_equal(ret$PAHPX2(), "360px") # "%spx", PAH - 240
      expect_equal(ret$PAHPX3(), "240px") # "%spx", PAH - 360
    })
  })
})
