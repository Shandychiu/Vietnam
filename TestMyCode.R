# Network Evolution
source("Vietnam2.0.R")
TT <- FindForward.threshold(TestEL, "Day", c("K", "E"), 1, 3, 5)
TTT <- lapply(TT, function(x) lapply(x, function(y) y$Matrix))

# Time Evolve Networks
D1 <- TTT[[1]][[1]]
D1
plot.network(as.network(D1), displaylabels = T, main = "D1")

D2 <- TTT[[1]][[2]] + TTT[[2]][[1]]
D2
plot.network(as.network(D2), displaylabels = T, main = "D2" )

D3 <- TTT[[1]][[3]] + TTT[[2]][[2]] + TTT[[3]][[1]]
D3
plot.network(as.network(D3), displaylabels = T, main = "D3" )

D4 <- TTT[[2]][[3]] + TTT[[3]][[2]] + TTT[[4]][[1]]
D4
plot.network(as.network(D4), displaylabels = T, main = "D4" )

D5 <- TTT[[3]][[3]] + TTT[[4]][[2]] + TTT[[5]][[1]]
D5
plot.network(as.network(D5), displaylabels = T, main = "D5" )

getElement(D1, "D1")
