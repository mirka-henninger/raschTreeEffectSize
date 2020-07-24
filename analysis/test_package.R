install.packages("/home/mhenninger/Documents/Git/partykit", repos = NULL, type = "source")
install.packages("/home/mhenninger/Documents/Git/psychotree", repos = NULL, type = "source")

devtools::load_all()

NPI <- readr::read_csv("~/Arbeit/Forschung/Data/NPI/data.csv")
head(NPI)

temp <- as.matrix(NPI[,2:41] )
temp[temp==0] <- NA

NPI$resp <- temp - 1
NPI <- NPI[,-c(2:41)]
NPI <- subset(NPI, rowMeans(resp, na.rm = TRUE) > 0 & + rowMeans(resp, na.rm = TRUE) < 1)
NPI$gender <- factor(NPI$gender)

(raschTree <- psychotree::raschtree(resp ~ age + gender, data = NPI,
                                    stopfun = mantelHaenszel(purification = "iterative", stopkrit = c("A","B")), verbose = TRUE))
plot(raschTree)
MH <- get_mantelHaenszel(raschTree, purification = "iterative")
MH_sum <- summary_mantelhaenszel(MH)
apply(MH_sum$classification,2,table)

make_numeric <- function(x){
  ifelse(x == "A",1, 3)
}

col <- list(
  "4" = make_numeric(MH$node3$classification),
  "5" = make_numeric(MH$node3$classification),
  "6" = make_numeric(MH$node2$classification),
  "9" = make_numeric(MH$node8$classification),
  "11" = make_numeric(MH$node10$classification),
  "12" = make_numeric(MH$node10$classification),
  "13" = make_numeric(MH$node7$classification)
)

plot(raschTree, col = col)

