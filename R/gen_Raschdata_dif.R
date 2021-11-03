gen_Raschdata_dif <- function(N = 500, I = 10, DIF_dichot = 0.5, DIF_cont = 0.5, whichDIF_dichot = I, whichDIF_cont = I-1){
  # define magic numbers
  betas <- scale(runif(I,-2,2), scale = FALSE)
  thetas <- rnorm(N)

  # DIF group 1
  DIFdichot1 <- rep(0,I)
  DIFdichot1[whichDIF_dichot] <- DIF_dichot

  DIFgroup_dichot <- rbinom(n = N, size = 1, prob = .5)

  # # continuous DIF group
  DIFcont <- rep(0,I)
  DIFcont[whichDIF_cont] <- DIF_cont
  DIFgroup_cont <- round(runif(n = N, min = 1, max = 100),0)
  DIFgroup_contInd <- ifelse(DIFgroup_cont > 40,1,0)

  # generate data
  dat_temp <- matrix(NA,nrow=N,ncol=I)
  u <- matrix(runif(I*N,0,1),nrow=N,ncol=I)
  for (n in 1:N){
    for (i in 1:I){
      dat_temp[n,i] <- plogis(thetas[n] - betas[i] +
                                DIFdichot1[i] * DIFgroup_dichot[n] +
                                DIFcont[i] * DIFgroup_contInd[n])
    }
  }
  dat <- ifelse(u<dat_temp,1,0)
  # add group indicator
  dat <- data.frame(dat)
  dat$sums <- as.factor(rowSums(dat))
  dat$group_dichot <- as.factor(DIFgroup_dichot)
  dat$group_cont <- DIFgroup_cont
  return(dat)
}
