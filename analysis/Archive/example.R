rm(list=ls())

data(NPI)
head(NPI)

temp <- as.matrix(NPI[,2:41] )
temp[temp==0] <- NA

NPI$resp <- temp - 1
NPI <- NPI[,-c(2:41)]
NPI <- subset(NPI, rowMeans(resp, na.rm = TRUE) > 0 & + rowMeans(resp, na.rm = TRUE) < 1)
NPI$gender <- factor(NPI$gender)
NPI <- dplyr::sample_n(NPI, 3000)


stopfun <- function(z, kidids) {
  temp <- list(z, kidids)
  temp$bla <- TRUE
  return(temp$bla)
}

stopfun(z = NULL, kidids = NULL)

raschTree <- raschtree(resp ~ age + gender, data = NPI, verbose = TRUE, stopfun = stopfun)

plot(raschTree)
str(raschTree)
plot(raschTree_MH$raschtree)
raschTree_MH$mantelHaenszel
