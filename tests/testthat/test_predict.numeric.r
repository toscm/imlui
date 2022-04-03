library("testthat")

test_that("`predict(lamis_signature, lamis_test1)`` works", {
    m <- "lamis_signature"
    for (d in c("lamis_test2")) {  # very slow: ,"lamis_test1", "lamis_train"
        p <- getdata(m)
        X <- getdata(d)
        X2 <- rename(X, globals$FEATURE_MAPPINGS[[d]])
        y <- safely(predict)(p, X2)
        expect_null(y$error)
    }
})
