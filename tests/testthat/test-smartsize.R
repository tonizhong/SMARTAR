test_that("There is warning", {
  addTaskCallback(function(...) {set.seed(1);TRUE})
# baseline=0

## stage=1
  SEQ <- 1:2
  A1 <- c(rep(0,1),rep(1,1))
  PI1 <- rep(0.5,2)
  MEAN <- 1:2
  SD <- rep(10,2)
  SIMatrix <- as.data.frame(cbind(SEQ,A1,PI1,MEAN,SD))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "gaussian"),NA)

  expect_warning(smartsize(delta=0.0435,df=5,global=TRUE,
                           alpha=0.05,beta=0.20,
                           family = "gaussian"),NA)
  expect_warning(smartsize(delta=0.45,global=FALSE,
                           alpha=0.05,beta=0.20,family = "gaussian"),NA)
## stage=2
  SEQ <- 1:8
  A1 <- c(rep(0,4),rep(1,4))
  PI1 <- rep(0.5,8)
  O2 <- rep(c(0,0,1,1),2)
  P2 <- c(0.7,0.7,0.3,0.3,0.6,0.6,0.4,0.4)
  A2 <- rep(c(0,1),4)
  PI2 <- rep(0.5,8)
  MEAN <- 1:8
  SD <- rep(10,8)
  SIMatrix <- as.data.frame(cbind(SEQ,A1,PI1,O2,P2,A2,PI2,MEAN,SD))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "gaussian"),NA)
## stage=3
  SEQ <- 1:32
  A1 <- c(rep(0,16),rep(1,16))
  PI1 <- rep(0.5,32)
  O2 <- rep(c(rep(0,8),rep(1,8)),2)
  P2 <- c(rep(0.7,16),rep(0.3,16))
  A2 <- rep(c(rep(0,4),rep(1,4)),4)
  PI2 <- rep(0.5,32)
  O3 <- rep(c(rep(0,2),rep(1,2)),8)
  P3 <- c(rep(0.7,2),rep(0.3,2),rep(0.6,2),rep(0.4,2),rep(0.45,2),rep(0.55,2),
          rep(0.8,2),rep(0.2,2),rep(0.9,2),rep(0.1,2),rep(0.75,2),rep(0.25,2),
          rep(0.65,2),rep(0.35,2),rep(0.95,2),rep(0.05,2))
  A3 <- rep(c(0,1),16)
  PI3 <- rep(0.5,32)
  MEAN <- 1:32
  SD <- rep(10,32)
  SIMatrix <- as.data.frame(cbind(SEQ,A1,PI1,O2,
                                  P2,A2,PI2,O3,P3,A3,PI3,MEAN,SD))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "gaussian"),NA)

  # baseline=1
  ## stage=1
  SEQ <- 1:4
  O1 <- c(rep(0,2),rep(1,2))
  P1 <- c(rep(0.7,2),rep(0.3,2))
  A1 <- rep(c(0,1),2)
  PI1 <- rep(0.5,4)
  MEAN <- 1:4
  SD <- rep(10,4)
  SIMatrix <- as.data.frame(cbind(SEQ,O1,P1,A1,PI1,MEAN,SD))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "gaussian"),NA)

  ## stage=2
  SEQ <- 1:16
  O1 <- c(rep(0,8),rep(1,8))
  P1 <- c(rep(0.7,8),rep(0.3,8))
  A1 <- rep(c(rep(0,4),rep(1,4)),2)
  PI1 <- rep(0.5,16)
  O2 <- rep(c(0,0,1,1),4)
  P2 <- c(rep(0.7,2),rep(0.3,2),rep(0.6,2),rep(0.4,2),rep(0.45,2),rep(0.55,2),
          rep(0.8,2),rep(0.2,2))
  A2 <- rep(c(0,1),8)
  PI2 <- rep(0.5,16)
  MEAN <- 1:16
  SD <- rep(10,16)
  SIMatrix <- as.data.frame(cbind(SEQ,O1,P1,A1,PI1,O2,P2,A2,PI2,MEAN,SD))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "gaussian"),NA)
  #### stage3
  ##SEQ <- 1:32
  ##O1 <- c(rep(0,16),rep(1,16))
  ##P1 <- c(rep(0.7,16),rep(0.3,16))
  ##A1 <- rep(c(rep(0,8),rep(1,8)),2)
  ##PI1 <- rep(0.5,32)
  ##O2 <- rep(c(rep(0,4),rep(1,4)),4)
  ##P2 <- c(rep(0.7,4),rep(0.3,4),rep(0.6,4),rep(0.4,4),rep(0.45,4),
  ##        rep(0.55,4),rep(0.8,4),rep(0.2,4))
  ##A2 <- rep(c(rep(0,2),rep(1,2)),8)
  ##PI2 <- rep(0.5,32)
  ##O3 <- rep(c(0,1),16)
  ##P3 <- c(0.7,0.3,0.6,0.4,0.45,0.55,0.8,0.2,0.9,0.1,0.95,0.05,
  ##        0.85,0.15,0.75,0.25,
  ##        0.7,0.3,0.6,0.4,0.45,0.55,0.8,0.2,0.9,0.1,0.95,0.05,
  ##        0.85,0.15,0.75,0.25)
  ##A3 <- rep(c(0,1),16)
  ##PI3 <- rep(1,32)
  ##MEAN <- 1:32
  ##SD <- rep(10,32)
  ##SIMatrix <- as.data.frame(cbind(SEQ,O1,P1,A1,PI1,O2,P2,A2,PI2,O3,P3,
  ##                                A3,PI3,MEAN,SD))
  ##expect_warning(smartsize(sim=SIMatrix,global=TRUE,
  ##                         alpha=0.05,beta=0.20,family = "gaussian"),NA)


  # family=="binomial"
  # baseline=0

  ## stage=1
  SEQ <- 1:2
  A1 <- c(rep(0,1),rep(1,1))
  PI1 <- rep(0.5,2)
  set.seed(1)
  MEAN <- runif(2,0,1)
  SIMatrix <- as.data.frame(cbind(SEQ,A1,PI1,MEAN))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "binomial"),NA)

  ## stage=2
  SEQ <- 1:8
  A1 <- c(rep(0,4),rep(1,4))
  PI1 <- rep(0.5,8)
  O2 <- rep(c(0,0,1,1),2)
  P2 <- c(0.7,0.7,0.3,0.3,0.6,0.6,0.4,0.4)
  A2 <- rep(c(0,1),4)
  PI2 <- rep(0.5,8)
  set.seed(1)
  MEAN <- runif(8,0,1)

  SIMatrix <- as.data.frame(cbind(SEQ,A1,PI1,O2,P2,A2,PI2,MEAN))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "binomial"),NA)
  ## stage=3
  SEQ <- 1:32
  A1 <- c(rep(0,16),rep(1,16))
  PI1 <- rep(0.5,32)
  O2 <- rep(c(rep(0,8),rep(1,8)),2)
  P2 <- rep(c(rep(0.7,8),rep(0.3,8)),2)
  A2 <- rep(c(rep(0,4),rep(1,4)),4)
  PI2 <- rep(0.5,32)
  O3 <- rep(c(rep(0,2),rep(1,2)),8)
  P3 <- c(rep(0.7,2),rep(0.3,2),rep(0.6,2),rep(0.4,2),rep(0.45,2),rep(0.55,2),
          rep(0.8,2),rep(0.2,2),rep(0.9,2),rep(0.1,2),rep(0.75,2),rep(0.25,2),
          rep(0.65,2),rep(0.35,2),rep(0.95,2),rep(0.05,2))
  A3 <- rep(c(0,1),16)
  PI3 <- rep(0.5,32)
  set.seed(1)
  MEAN <- runif(32,0,1)
  SIMatrix <- as.data.frame(cbind(SEQ,A1,PI1,O2,P2,A2,PI2,O3,P3,A3,PI3,MEAN))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "binomial"),NA)

  # baseline=1
  ## stage=1
  SEQ <- 1:4
  O1 <- c(rep(0,2),rep(1,2))
  P1 <- c(rep(0.7,2),rep(0.3,2))
  A1 <- rep(c(0,1),2)
  PI1 <- rep(0.5,4)
  set.seed(1)
  MEAN <- runif(4,0,1)
  SIMatrix <- as.data.frame(cbind(SEQ,O1,P1,A1,PI1,MEAN))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "binomial"),NA)

  ## stage=2
  SEQ <- 1:16
  O1 <- c(rep(0,8),rep(1,8))
  P1 <- c(rep(0.7,8),rep(0.3,8))
  A1 <- rep(c(rep(0,4),rep(1,4)),2)
  PI1 <- rep(0.5,16)
  O2 <- rep(c(0,0,1,1),4)
  P2 <- c(rep(0.7,2),rep(0.3,2),rep(0.6,2),rep(0.4,2),rep(0.45,2),rep(0.55,2),
          rep(0.8,2),rep(0.2,2))
  A2 <- rep(c(0,1),8)
  PI2 <- rep(0.5,16)
  set.seed(1)
  MEAN <- runif(16,0,1)
  SIMatrix <- as.data.frame(cbind(SEQ,O1,P1,A1,PI1,O2,P2,A2,PI2,MEAN))

  expect_warning(smartsize(sim=SIMatrix,global=TRUE,
                           alpha=0.05,beta=0.20,family = "binomial"),NA)
})
