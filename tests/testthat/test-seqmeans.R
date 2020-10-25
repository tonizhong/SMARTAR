test_that("There is warning", {
  addTaskCallback(function(...) {set.seed(1);TRUE})
  expect_warning(seqmeans(Dat_1_0_0,family = "binomial",
                          digits = 2,
                          color = c("yellow", "lightblue"),
                          pch = c(18,15),
                          title = "Design",
                          xlab = "treatment",
                          ylab="outcome",
                          xtext = 1:2,
                          legend = 1:2,
                          reference = FALSE),NA)
  
  expect_warning(seqmeans(Dat_1_0_0,family = "binomial",
                          digits = 2,
                          color = c("yellow", "lightblue"),
                          pch = c(18,15),
                          title = "Design",
                          xlab = "treatment",
                          ylab="outcome",
                          xtext = 1:2,
                          legend = 1:2,
                          reference = TRUE),NA)
  expect_warning(seqmeans(Dat_2_0_0,family = "binomial",digits = 2),NA)
  expect_warning(seqmeans(Dat_3_0_0,family = "binomial",digits = 2),NA)
  expect_warning(seqmeans(Dat_1_1_0,family = "binomial",digits = 2),NA)
  expect_warning(seqmeans(Dat_2_1_0,family = "binomial",digits = 2),NA)
  expect_warning(seqmeans(Dat_1_0_1),NA)
  expect_warning(seqmeans(Dat_1_1_1),NA)
  expect_warning(seqmeans(Dat_2_0_1),NA)
  expect_warning(seqmeans(Dat_2_1_1),NA)
  expect_warning(seqmeans(Dat_3_0_1),NA)

  expect_warning(seqmeans(Dat_1_0_0,family = "binomial",
                          digits = 2,plot = "s",
                          legend = 1,xlab = "treatment",
                          xtext = 1:2,ylab = "outcome"),NA)
  expect_warning(seqmeans(Dat_1_1_0,family = "binomial",
                          digits = 2,plot = "s",title = "Primary Outcome",
                          color = c("yellow", "lightblue"),pch=c(18,14),
                          xlab = "treatment",
                          ylab="Outcome",xtext = 1:4,
                          legend = c("evaluation", "treatment"),
                          reference = FALSE),NA)
  expect_warning(seqmeans(Dat_2_1_0,family = "binomial",
                          digits = 2,plot = "s"),NA)
  expect_warning(seqmeans(Dat_2_0_0,family = "binomial",
                          digits = 2,plot = "s"),NA)
  expect_warning(seqmeans(Dat_3_0_0,family = "binomial",
                          digits = 2,plot = "s"),NA)
  expect_warning(seqmeans(Dat_1_0_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  expect_warning(seqmeans(Dat_1_1_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  expect_warning(seqmeans(Dat_2_0_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  expect_warning(seqmeans(Dat_2_1_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  expect_warning(seqmeans(Dat_3_0_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  a<-seqmeans(Dat_2_1_0,family = "binomial",
             digits = 2,plot = "s")
  summary(a)
  expect_warning(summary(a),NA)
})
