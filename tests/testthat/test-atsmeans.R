test_that("There is warning", {
  addTaskCallback(function(...) {set.seed(1);TRUE})
  expect_warning(atsmeans(Dat_1_0_0,plot = TRUE,xtext = 1:2,
                          ylim = c(-1,3),family = "binomial",
                          method = "IPW",common = FALSE,xlab = "strategy",
                          ylab = "strategy value",
                          title = "Strategy",
                          alpha = 0.06,color = "lightblue"),NA)
  expect_warning(atsmeans(Dat_1_1_0,method = "Gest",
                          family = "binomial",common = TRUE,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          title = "Strategy",
                          ylim = c(0,1),plot = TRUE),NA)
  expect_warning(atsmeans(Dat_2_0_0,method = "IPW",
                          family = "binomial",common = TRUE,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          title = "Strategy",ylim=c(-1,2),plot = TRUE),NA)
  expect_warning(atsmeans(Dat_2_0_0,method = "Gest",
                          family = "binomial",common = FALSE,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          title = "Strategy",ylim=c(-1,2),plot = TRUE),NA)
  expect_warning(atsmeans(Dat_2_1_0,method = "IPW",
                          family = "binomial",common = TRUE,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          title = "Strategy",plot = TRUE),NA)
  expect_warning(atsmeans(Dat_2_1_0,method = "Gest",
                          family = "binomial",common = FALSE,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          title = "Strategy",plot = TRUE),NA)
  expect_warning(atsmeans(Dat_3_0_0,conf = FALSE,method = "Gest",
                          family = "binomial",common = TRUE,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          title = "Strategy"),NA)
  expect_warning(atsmeans(Dat_3_0_0,conf = FALSE,method = "IPW",
                          family = "binomial",common =FALSE,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          title = "Strategy"),NA)
  expect_warning(atsmeans(Dat_1_0_1,plot = TRUE,digits = 2,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          conf = TRUE,title = "Strategy"),NA)
  expect_warning(atsmeans(Dat_1_0_1,plot = TRUE,digits = 2,
                          ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          conf = TRUE,title = "Strategy",common = TRUE),NA)
  expect_warning(atsmeans(Dat_1_1_1,plot = TRUE,ylab = "strategy value",
                          xtext = 1:4,pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          conf = TRUE,title = "Strategy"),NA)
  expect_warning(atsmeans(Dat_1_1_1,plot = TRUE,ylab = "strategy value",
                          xtext = 1:4,pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          conf = TRUE,title = "Strategy",common = TRUE),NA)
  expect_warning(atsmeans(Dat_2_0_1,plot = TRUE,ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,xtext = 1:8,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          conf = TRUE,title = "Strategy"),NA)
  expect_warning(atsmeans(Dat_2_0_1,plot = TRUE,ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,xtext = 1:8,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          conf = TRUE,title = "Strategy",common = TRUE),NA)
  expect_warning(atsmeans(Dat_2_1_1,plot = TRUE,ylab = "strategy value",
                         pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                         conf = TRUE,title = "Strategy"),NA)
  expect_warning(atsmeans(Dat_2_1_1,plot = TRUE,ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          conf = TRUE,title = "Strategy",common = TRUE),NA)
  expect_warning(atsmeans(Dat_3_0_1,plot = TRUE,ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                         conf = TRUE,title = "Strategy"),NA)
  expect_warning(atsmeans(Dat_3_0_1,plot = TRUE,ylab = "strategy value",
                          pch = 16, cex=1,lwd=2,
                          xlab="strategy",alpha=0.06,color = "lightblue",
                          conf = TRUE,title = "Strategy",method = "IPW",
                          common = TRUE,cex.axis = 1,mar=c(5,4,4,1)),NA)
  a<-atsmeans(Dat_2_0_1,plot = TRUE,ylab = "strategy value",
             pch = 16, cex=1,lwd=2,xtext = 1:8,
             xlab="strategy",alpha=0.06,color = "lightblue",
             conf = TRUE,title = "Strategy")
  expect_warning(summary(a),NA)

})
