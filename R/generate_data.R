GenerateData <- function(time.bp, price, beta, nstations, ntrains, nseats) {

  ndays <- 60
  routes <- getRoutes(nstations, ndays, c(0.3, 0.4, 0.5))

  ## loop through
  foreach(j = 1:ntrains) %do% {

    ## set initial values for reward and occupancy
    total.reward <- 0
    actual.occupancy <- rep(0, nstations - 1)

    ## loop through all time intervals
    foreach(i = 1:(length(time.bp) - 1), .combine = rbind) %do% {

      ## get i-th time interval
      from <- time.bp[i]
      to   <- time.bp[i+1]

      ## simulate demand
      mc <- SimulateMarkovProcess(1, routes, beta, price, from, to, nseats, actual.occupancy)

      ## change reward and occupancy based on simulated data
      total.reward <- total.reward + mc$reward
      actual.occupancy <- unname(mc$occupancy)

      ## return df with tickets (to be rbinded)
      mc$tickets
    }
  }
}
