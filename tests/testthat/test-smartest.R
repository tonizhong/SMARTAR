test_that("There is warning", {
  addTaskCallback(function(...) {set.seed(1);TRUE})
  #generate a pesudo SMART data
  expect_warning(smartest(Dat_1_1_0,family = "binomial",
                          method = "Gest", digits = 2,
                          common = TRUE,adjust = "Bon",ntest = 3),NA)
  expect_warning(smartest(Dat_1_0_0,method = "IPW", family = "binomial"),NA)
  expect_warning(smartest(Dat_2_0_0,method = "Gest",family = "binomial"),NA)
  expect_warning(smartest(Dat_2_1_0,method = "Gest",family = "binomial"),NA)
  expect_warning(smartest(Dat_3_0_0,method = "Gest",family = "binomial"),NA)
  expect_warning(smartest(Dat_1_0_1,method = "Gest",family = "gaussian"),NA)
  expect_warning(smartest(Dat_1_1_1,method = "Gest",family = "gaussian"),NA)
  expect_warning(smartest(Dat_2_0_1,method = "Gest",family = "gaussian"),NA)
  expect_warning(smartest(Dat_2_1_1,method = "Gest",family = "gaussian"),NA)
  expect_warning(smartest(Dat_3_0_1,method = "Gest",family = "gaussian"),NA)
})
