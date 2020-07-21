rm(list=ls())

data(NPI)
head(NPI)

temp <- as.matrix(NPI[,2:41] )
temp[temp==0] <- NA

NPI$resp <- temp - 1
NPI <- NPI[,-c(2:41)]
NPI <- subset(NPI, rowMeans(resp, na.rm = TRUE) > 0 & + rowMeans(resp, na.rm = TRUE) < 1)
NPI$gender <- factor(NPI$gender)

(raschTree <- raschtree(resp ~ age + gender, data = NPI))

shorterTree <- use_MH(raschTree)
plot(raschTree)
plot(shorterTree)
