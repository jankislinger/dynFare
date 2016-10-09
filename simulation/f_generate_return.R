generate.return <- function(decision.time, price, occupancy = rep(0,5)) {
  
  if (any(price < 1)) {
    NA
  } else if (length(decision.time) == 2) {
    mean(simulate.demand(decision.time[1], decision.time[2], price, occupancy)$return)
  } else {
    actual.simul <- simulate.demand(decision.time[1], decision.time[2], price, occupancy)
    actual.simul$return + maximize.return(decision.time[-1], price, occupancy + actual.simul$occupancy)$objective
  }
}