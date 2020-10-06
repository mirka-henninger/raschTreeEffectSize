
install.packages("/home/mhenninger/Documents/Git/partykit", repos = NULL, type = "source")
install.packages("/home/mhenninger/Documents/Git/psychotree", repos = NULL, type = "source")

devtools::load_all()

rm(list=ls())


load("data/spisa_ges.RData")

spisa$fedState <- ifelse(spisa$UniBL == "Hes", "Hessen",
                         ifelse(spisa$UniBL == "R-P", "Rheinland-Pfalz", c("B-W, Bayern, Berlin, ...                   ")))
spisa$fedState <- factor(spisa$fedState)
spisa <- spisa[!spisa$fedState=="missing",]

spisa$resp <- as.matrix(spisa[ , 1:10])
spisa <- spisa[ , -(1:10)]

spisa$resp <- ifelse(spisa$resp, 1, 0)

spisa <- subset(spisa, rowMeans(resp, na.rm = TRUE) > 0 &
                   rowMeans(resp, na.rm = TRUE) < 1)

dat <- dplyr::sample_n(spisa, 300000)

mytree <- raschtree(resp ~ fedState, data = dat)

png(filename="plotspisa.png", width = 600, height = 400)
plot(mytree, cex = .7)
dev.off()

MH <- get_mantelHaenszel(mytree, purification = "iterative")


(raschTree <- psychotree::raschtree(resp ~ fedState, data = dat,
                                    stopfun = mantelHaenszel(purification = "iterative", stopkrit = c("A")), verbose = TRUE))

make_numeric <- function(x, a){
  ifelse(x == "A", 1, a)
}
make_pch <- function(x){
  ifelse(x == "A", 19, 23)
}

col <- list(
  "2" = make_numeric(MH$node1$classification, 2),
  "4" = make_numeric(MH$node1$classification, 4),
  "5" = make_numeric(MH$node1$classification, 4)
)
pch <- list(
  "2" =  make_pch(MH$node1$classification),
  "4" =  make_pch(MH$node1$classification),
  "5" =  make_pch(MH$node1$classification)
)

png(filename="plotspisa_color.png", width = 600, height = 400)
plot(mytree, col = col, pch = pch, cex =.8)
dev.off()

png(filename="plotspisa_stopped.png", width = 600, height = 300)
plot(raschTree, col = col[[1]], pch = pch[[1]], cex = .8)
dev.off()



