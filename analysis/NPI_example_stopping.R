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
                                    stopfun = mantelHaenszel(purification = "iterative", stopkrit = c("A")), verbose = TRUE))

plot(raschTree)
MH <- get_mantelHaenszel(raschTree, purification = "iterative")
MH_sum <- summary_mantelhaenszel(MH)
apply(MH_sum$classification,2,table)

make_numeric <- function(x, a){
  ifelse(x == "A", 1, a)
}
make_pch <- function(x){
  ifelse(x == "C", 23, 19)
}

col <- list(
  "4" = make_numeric(MH$node2$classification, 2),
  "5" = make_numeric(MH$node2$classification, 2),
  "6" = make_numeric(MH$node2$classification, 3),
  "9" = make_numeric(MH$node7$classification, 4),
  "11" = make_numeric(MH$node7$classification, 4),
  "12" = make_numeric(MH$node7$classification, 4),
  "13" = make_numeric(MH$node7$classification, 5)
)
pch <- list(
  "4" =  make_pch(MH$node2$classification),
  "5" =  make_pch(MH$node2$classification),
  "6" =  make_pch(MH$node2$classification),
  "9" =  make_pch(MH$node7$classification),
  "11" = make_pch(MH$node7$classification),
  "12" = make_pch(MH$node7$classification),
  "13" = make_pch(MH$node7$classification)
)

# png(filename="analysis/colored_tree.png", width = 1200, height = 600)
plot(raschTree, col = col, pch = pch)
# dev.off()


