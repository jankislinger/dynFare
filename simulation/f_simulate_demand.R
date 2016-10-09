simulate.demand <- function(from, to, price, occupancy = rep(0,5), nproc = 1, nmix = 1) {
  
  from <- 0; to <- 10
  
  ## simulate demand to get matrix with new passengers
  processes <- lapply(rep(1:choose(nstation, 2), each = nproc), function(id) {
    time <- generate.process(beta[id,], from, to, price[id])
    if (length(time) > 0) {
      cbind(routes[id, 2], routes[id, 3], time, price[id])
    } else {
      NULL
    }
  })
  
  ## define empty object for results
  return.out <- rep(NA, nmix)
  occupancy.out <- matrix(NA, nmix, nstation - 1)
  
  ## loop through samples
  for (i in 1:nmix) {
    
    ## select processes for sample
    
    ## simulate demand to get matrix with new passengers
    new.passengers <- combine.arrays(processes[1 + 1:choose(nstation, 2) * nproc - sample(1:nproc, choose(nstation, 2), T)])
    
    ## return zero return and the same occupancy if there is no new passenger
    if (is.null(new.passengers)) {
      return(list(return = 0, occupancy = occupancy))
    }
    
    ## identify passengers that exceeds the capacity and remove these
    data.cleaned <- F
    while (!data.cleaned) {
      new.occupancy <- outer(1:nrow(new.passengers), 1:(nstation - 1) + 0.5, function(i,j) {
        as.integer(j > new.passengers[i,1] & j < new.passengers[i,2])
      })
      cumul.occupancy <- apply(rbind(occupancy, new.occupancy), 2, cumsum)
      if (max(cumul.occupancy) > nseat) {
        row <- which(apply(cumul.occupancy, 1, max) > nseat)[1] - 1
        col <- which(cumul.occupancy[row + 1,] > nseat)[1]
        new.passengers <- new.passengers[new.occupancy[,col] == 0 | 1:nrow(new.occupancy) < row,]
      } else {
        data.cleaned <- T
      }
    }
    
    ## add actual results
    return.out[i] <- sum(new.passengers[,4])
    occupancy.out[i,] <- cumul.occupancy[nrow(cumul.occupancy),]
  }
  
  ## return sum of all ticket prices and new occupancy
  list(return = return.out, occupancy = occupancy.out)
}

#-----------------------------------------------------------------------
# AUXILIARY FUNCTIONS AND VARIABLES

## combine list of arrays to one large array
combine.arrays <- function(x) {
  if (sum(unlist(sapply(x, nrow))) == 0) {
    return(NULL)
  }
  y <- matrix(0, sum(unlist(sapply(x, nrow))), 4)
  k <- 0
  for (i in which(!sapply(x, is.null))) {
    y[k + 1:nrow(x[[i]]),] <- x[[i]]
    k <- k + nrow(x[[i]])
  }
  colnames(y) <- c('from', 'to', 'time', 'price')
  y[order(y[,3], method = 'radix'),, drop = F]
}