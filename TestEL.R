setwd("~/Desktop/Project/")
library("statnet")
library("Matrix")
source("createNet.R")
source("Vietnam2.0.R")
TestEL <- data.frame(From = c("L","K","K","E","G","F","P", 
                              "K","G","H","B", "K","E", "K","N","D", "I"),
                     To =  c("K","G","D","B","F","P","Q",
                             "E","H","I","A", "N","D", "M","O","C", "J"),
                     Day = c(1,1,1,1,1,1,1, 2,2,2,2, 3,3, 5,5,5, 8),
                     stringsAsFactors = FALSE
)

TestEL$ID <- 1:nrow(TestEL)
Ms <- GetMatrixList(TestEL, "Day", 1, 8)


TestG <- createNet(TestEL, timeVarName = "Day", edgeIdVarName = "ID")
get.edge.attribute(TestG, "time")
get.node.attr(TestG, "vertex.names")

# Temporal <- Reach(edgeList = TestEL, timeVarName = "Day", 
#       rootNode = "K", timeOrigin = 1, traceDuration = 8, saveMatrix = T)
# 
# sumM = 0
# for(i in 1:8) {
#   tmp <- Temporal[[i]]$Matrix
#   sumM <- sumM + Temporal[[i]]$Matrix
# }
#plot(as.network(sumM), displaylabels = T)
# for demo
mycoord <- plot(TestG)
mylabel.col <- rep(1, network.size(TestG))
#make white
idx <- which(get.vertex.attribute(TestG, "vertex.names") %in% c("A", "B", "C", "L", "J"))
mylabel.col[idx] <- 'white'

plot(TestG, 
     coord = mycoord,
     displaylabels = T, 
     edge.label = get.edge.attribute(TestG, "time"),
     vertex.col = c(1,1,1, 2,2,2,2,2,2, 1, 3,1, 2,2,2,2,2),
     edge.col = c(1, 2,2,1,2,2,2,2,2,2, 1, 2,2,2,2, 1,1),
     label.pos = 5,
     vertex.cex = 2.5,
     edge.lwd = 3,
     edge.label.cex = 2,
     arrowhead.cex = 2,
     label.col = mylabel.col)

legend("bottomleft", 
       legend = c("Outgoing-infection chain", "Root", "Not Relevent", 
                  "High Risk Contact", "Not High Risk Contact"),
       pch = c(20, 20, 20, NA, NA),
       pt.cex = 2,
       lty = c(NA, NA,NA, 1, 1),
       lwd = c(NA, NA,NA, 3, 3),
       col = c("red","green", "black", "red", "black"),
       bty = "n")



plot(as.network(Ms[[1]]), displaylabels = T)


TT <- FindForward.threshold(TestEL, "Day", "K", 1, 3, 5, savecurRoots = T, saveMatrix = T) #start, threshold, end
lapply(TT, function(ls) unique(unlist(ls[2])))
CumDs <- vector("list", length = 5)
D <- Matrix(0, nrow = nrow(TT[[1]][[1]]), ncol = ncol(TT[[1]][[1]]))
for(i in 1:5) {
  tmp <- Network.atTime(TT, 3, i) 
  print(class(tmp))
  D <- D + tmp
  CumDs[i] <- D
}

Out <- sapply(CumDs, function(x, origin) {
  Count.reach(x, origin)}, "K")

# Time Evolve Networks
D1 <- TT[[1]][[1]]
D1
plot.network(as.network(D1), displaylabels = T, main = "D1")
D1 <- Matrix(D1)
idx <- which(colnames(D1) == "K")

(rowSums(D1) + colSums(D1))[-idx]


D2 <- TT[[1]][[2]] + TT[[2]][[1]]
D2
plot.network(as.network(D2), displaylabels = T, main = "D2" )

D3 <- TT[[1]][[3]] + TT[[2]][[2]] + TT[[3]][[1]]
D3
plot.network(as.network(D3), displaylabels = T, main = "D3" )

D4 <- TT[[2]][[3]] + TT[[3]][[2]] + TT[[4]][[1]]
D4
plot.network(as.network(D4), displaylabels = T, main = "D4" )

D5 <- TT[[3]][[3]] + TT[[4]][[2]] + TT[[5]][[1]]
D5
plot.network(as.network(D5), displaylabels = T, main = "D5" )



as.network(D1+D2)
as.network(D1+D2+D3)
as.network(D1+D2+D3+D4+D5)
