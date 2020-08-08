test_that("There is warning", {
  expect_warning(getncp(df=5, alpha = 0.05, beta = 0.2,
                        d = 1e-04, start = 5),NA)
})
