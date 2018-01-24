# Vietnam 2.0: evolution of networks with time-spanning constraint on path formation

# Vietnam: a handy forward search tool

library("statnet")
library("Matrix")
#### Count.reach: count #farms that are approachable by the origin farm. 
#### By defult, the origin is not counted
###
Count.reach <- function(Matrix, origin, excludeSelf = TRUE) {
  Flag <- (rowSums(Matrix) + colSums(Matrix)) >= 1 
  if(excludeSelf) {
  idx <- which(colnames(Matrix) == origin)
  #print(Flag[-idx])
  Return <- sum(Flag[-idx])
  } else {
    Return <- sum(Flag)
  }
  return(Return)
}



#### Network.atTime : get the network @ a specific time point
# Input: time = @ what time's network
# Outout: an adjacency matrix of network at the given time 
Network.atTime <- function(temporalM, windowLength, atTime) {
  dt <- matrix(0, 
               nrow = nrow(temporalM[[1]][[1]]), 
               ncol = ncol(temporalM[[1]][[1]]))
  
  rownames(dt) <- rownames(temporalM[[1]][[1]])
  colnames(dt) <- colnames(temporalM[[1]][[1]])
  
  if(atTime < windowLength) {
    counterEnd <- atTime
    counter <- 1
    i <- 1   
    j <- atTime
    
    while(counter < counterEnd + 1) {
      # print("***")
      # print(i)
      # print(j)
      tmp <- temporalM[[i]][[j]]
      dt <- dt + tmp
      # update
      i <- i + 1
      j <- j - 1
      counter <- counter + 1
    } 
    return(dt)
  }
  
  if(atTime >= windowLength){
    counterEnd <- windowLength
    counter <- 1
    i <- atTime - windowLength + 1
    j <- windowLength
    
    while(counter < counterEnd + 1) {
      # print("!!!!!")
      # print(i)
      # print(j)
      tmp <- temporalM[[i]][[j]]
      dt <- dt + tmp
      # update
      i <- i + 1
      j <- j - 1
      counter <- counter + 1
    }
    return(dt)
  }
}
  

#### Cumulative
Cumilative.threshold <- function(timeStart, timeEnd, TemporalM, windowLength) {
  
}

#### FindForward.threshold: Forward search with a threshold on the time spanning on each path
# Input: edgelist, timeVarname, days to search, origin (a single node or a vector or nodes)
# Output: List of root notes

FindForward.threshold <- function(edgeList, timeVarName, origin, 
                                  timeStart, threshold, timeEnd,...) {
  Idx_for_time <- which(colnames(edgeList) == timeVarName)
  
  if(class(edgeList[,Idx_for_time]) != class(timeStart)) {
    stop("timeStart and timeVar must be in same units")
  }
  
  if(is.numeric(edgeList[ ,Idx_for_time]) & is.numeric(timeStart) & is.numeric(timeEnd)) {
    
  } else {
    PRINT <- paste("converting time variables of class", 
                   class(timeStart),
                   "to numeric...might want to check")
    print(PRINT)
    edgeList[ ,Idx_for_time] <- as.numeric(edgeList[ ,Idx_for_time])
    timeStart <- as.numeric(timeStart)
    timeEnd <- as.numeric(timeEnd)
  }
  # create Sub edgelist. Edges active between timeStart and till 
  till <- timeEnd + 1
  rowIndex <- which(edgeList[ ,Idx_for_time] >= timeStart & edgeList[ ,Idx_for_time] < till)
  subEL <- edgeList[rowIndex, ]
  #print(head(subEL))
  
  # if origin has no sending movements in the entire search period ,stop
  #allNodes <- unique(c(subEL[,1], subEL[,2]))
  senders <- unique(subEL[,1])
  
  if(!any(origin %in% senders)) {
    print(paste("No sending in the assigned period of time", origin))
    print(timeStart)
    print(timeEnd)
    print(origin)
    Out <- NULL
    return(Out)
  }
  # generate a list of matrix with length t
  # Ms[[t]] is the adjacency matrix @ t (snapshot)
  Ms <- GetMatrixList(subEL, timeVarName, timeStart, till-1)
  
  # the RootNodes to search in each time window. They are the origin's 
  # direct neighbors updated for every unit of time
  
  RootNodeSets <- lapply(Ms, function(x) {
    Flag <- apply(x[origin, ,drop = F], 2, sum) >= 1
    return(rownames(x)[Flag])
    })
  #print(RootNodeSets)
  
  Out <- lapply(1:length(RootNodeSets),
                   function(i, RootNodeSets) {
                     Reach(Ms,
                           rootNodeSet = RootNodeSets[[i]],
                           origin = origin,
                           timeIndexStart = i,
                           traceDuration = threshold,
                           useThreshold = TRUE)
                   },RootNodeSets)
  return(Out)  
}

#### FindForward: Forward search with no limit on the time spanning on each path
# Input: edgelist, timeVar, origin, time to start, how long to trace
# Output: the nodes contacted by day
FindForward <- function(edgeList, timeVarName, origin, timeStart, 
                        traceDuration, saveMatrix = FALSE) {
  # Generate subEL for matrix list, Ms  
  Idx_for_time <- which(colnames(edgeList) == timeVarName)
  
  if(class(edgeList[,Idx_for_time]) != class(timeStart)) {
    stop("timeStart and timeVar must be in same units")
  }
  
  if(is.numeric(edgeList[ ,Idx_for_time]) & is.numeric(timeStart)) {
    
  } else {
    PRINT <- paste("converting time variables of class", 
                   class(timeStart),
                   "to numeric...might want to check")
    print(PRINT)
    edgeList[ ,Idx_for_time] <- as.numeric(edgeList[ ,Idx_for_time])
    timeStart <- as.numeric(timeStart)
  }
  # create Sub edgelist. Edges active between timeStart and till 
  till <- timeStart + traceDuration + 1
  rowIndex <- which(edgeList[ ,Idx_for_time] >= timeStart & edgeList[ ,Idx_for_time] < till)
  subEL <- edgeList[rowIndex, ]
  print(head(subEL))
  
  # if no movements ,stop
  allNodes <- unique(c(subEL[,1], subEL[,2]))
  if(!all(rootNode %in% allNodes)) {
    stop("No Movements of root node(s) in assigned period of time")
  }
  # Ms[[t]] is the adjacency matrix @ t (snapshot)
  Ms <- GetMatrixList(subEL, timeVarName, timeStart, till-1)
  
  RootsByDay <- Reach(Ms, origin, traceDuration, saveMatrix) 
  return(RootsByDay)
} 

#### Reach: Given root(s), search paths extended from that root(s) within a time window (timeDuration)
# Input: matrixList = daily snapshots
# origin = the begining node to search
# traceDuation = how long do you want to trace? 
# The longer the time, the longer the path can be
Reach <- function(matrixList, rootNodeSet, origin, timeIndexStart = 1, 
                  traceDuration, useThreshold = FALSE, 
                  saveMatrix = TRUE, savecurRoots = TRUE) {
  timeIndexEnd <- min((timeIndexStart + traceDuration - 1), length(matrixList))
  
  #print(paste("timeIndexStart:", timeIndexStart))
  #print(paste("timeIndexEnd:", timeIndexEnd))
  #print(rootNodeSet)
  
  
  # Return value: T_ij = a list of matrices
  # Return valuses: Nds = a list of nodes
  T_ij <- vector("list", length = timeIndexEnd - timeIndexStart + 1) # T_ij
  Nds <- vector("list", length = timeIndexEnd - timeIndexStart + 1)
  
  j <- 1 # index for j in T_ij
  curRoots <- rootNodeSet # initialize
  ZERO <- matrix(0, nrow = nrow(matrixList[[1]]), ncol = ncol(matrixList[[1]]))
  colnames(ZERO) <- colnames(matrixList[[1]])
  rownames(ZERO) <- rownames(matrixList[[1]])
  for(t in timeIndexStart:timeIndexEnd) {
    
    # if use threshold, force origin's row and col to be zero so that we won't find any movement 
    # back to origin and go out again
    if(useThreshold) {
      matrixList[[t]][origin, ] <- 0
      matrixList[[t]][ ,origin] <- 0
      
    }
    # if there exsists no movement of any root node @ time t, then Ms[[t]] = 0,  move to time t+1
    if(all(matrixList[[t]][curRoots, ] == 0)) {
      #print("!!!!")
      matrixList[[t]] <- ZERO  # update Ms[[t]] according to curRoots

      if(j == 1) {
        matrixList[[t]][origin, curRoots] = 1   # put back the "to" from origin to curRoots
      }
      
      if(saveMatrix) {
        T_ij[[j]] <- Matrix(matrixList[[t]]) # make it a sparse matrix 
      }
      
      if(savecurRoots) {
        Nds[[j]] <- curRoots
      }
      j <- j+1
      next } 
    
    # Mi's row of the root node includes all NBs, direct or indirect, of that root node
    # diameter = the power that matrixList[[i]] will take up to
   # print("****")
    #print(paste("t:", t))
  
    diameter <- max(unlist(geodist(matrixList[[t]], count.paths = F, inf.replace = -Inf)))
    
    #print(paste("diameter:", diameter))
    
    Mt <- 0
    tmp <- matrixList[[t]]
    for(y in 1:diameter) {  # take power sum of matrixList[[t]] up to diameter
      Mt <- Mt + tmp
      tmp <- tmp %*% matrixList[[t]]
    }
    
    Flag_toDrop <- apply(Mt[curRoots,,drop = F ], MARGIN = 2, function(x) sum(x) == 0)
    Flag_toDrop[curRoots] <- FALSE
    
    nodes_toDrop <- names(Flag_toDrop)[which(Flag_toDrop == TRUE)]
    matrixList[[t]][nodes_toDrop, ] <- 0 # undate matrixList[[t]]
    
    NextRootToSearch <- setdiff(colnames(matrixList[[1]]), nodes_toDrop)
    
    if(j == 1) {
      matrixList[[t]][origin, curRoots] = 1
    }
    
    if(saveMatrix) {
      T_ij[[j]] <- Matrix(matrixList[[t]]) # make it a sparse matrix
    }
    
    
    curRoots <- NextRootToSearch # update curRoots
    if(savecurRoots) {
      Nds[[j]] <- curRoots
    }
    j <- j+1
  }
  
    return(list(T_ij, Nds))
}


GetMatrixList <- function(EL, timeVarName, start, end) { #use number for time
  N <- end - start + 1
  RowIndex <- intersect(which(EL[,timeVarName] <= end), which(EL[,timeVarName] >= start))
  EL <- EL[RowIndex, ]
  Vs <- unique(c(EL[,1], EL[,2])) # column1: from, column2: to
  Vs <- sort(Vs)
  MatrixList <- vector("list", length = N)
  template <- matrix(NA, ncol = length(Vs), nrow = length(Vs), dimnames = list(Vs, Vs))
  
  for(i in 1:N) {
    curTime <- start + i - 1
    curRowIndex <- which(EL[ ,timeVarName] == curTime)
    curEL <- EL[curRowIndex, ]
    if(nrow(curEL) > 0) {
      m <- as.matrix(as.network(curEL, matrix.type = "edgelist"))
      curM <- template
      curM[rownames(m), colnames(m)] <- m
    } else {
      curM <- template
    }
    MatrixList[[i]] <- curM
  }
  
  for(i in 1:N) {
    index <- which(is.na(MatrixList[[i]]))
    MatrixList[[i]][index] <- 0
  }
  return(MatrixList)
}

GetMatrixList.2 <- function(EL, timeVarName, start, end) { #use number for time
  
  N <- end - start + 1
  RowIndex <- intersect(which(EL[,timeVarName] <= end), which(EL[,timeVarName] >= start))
  EL <- EL[RowIndex, ]
  Vs <- unique(c(EL[,1], EL[,2])) # column1: from, column2: to
  Vs <- sort(Vs)
  template <- matrix(NA, ncol = length(Vs), nrow = length(Vs), dimnames = list(Vs, Vs))
  times <- start + (1:N) - 1
  timesInEL <- levels(as.factor(EL[,timeVarName]))
  NoShow <- setdiff(times, timesInEL)
  timesInEL_Idx <- as.numeric(as.character(timesInEL)) - start + 1
  NoShow_Idx <- as.numeric(as.character(NoShow)) - start + 1
  
  Out <- vector("list", N)

  TT <- by(EL, EL[, timeVarName], function(x) as.matrix(as.network(x, matrix.type = 'edgelist')))
  
  MatrixList <- lapply(TT, function(m) {
    template[rownames(m), colnames(m)] <- m
    template
    })
  #print(MatrixList)
  Out[timesInEL_Idx] <- MatrixList
  Out[NoShow_Idx] <- list(template) # put a list instead of a single element
  
  Out <- lapply(Out, function(m) {
    m[is.na(m)] <- 0 
    return(m)
    })
  return(Out)
}


