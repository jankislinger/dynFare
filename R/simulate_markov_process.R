SimulateMarkovProcess <- function(model, routes, beta, price, from, to, nseats, init.occupancy) {

  ## set maximal from and to values for time
  from <- pmin(from, routes[,'dep_daytime'])
  to   <- pmin(to,   routes[,'dep_daytime'])

  ## simulate Poisson process for each route
  tickets <- foreach(i = 1:nrow(routes), .combine = rbind) %do% {

    pois <- SimulatePoissonProcess(model, beta[i,], price[i], from[i], to[i])

    if (length(pois) > 0) {
      cbind(route = i, time = pois, count = 1)
    } else {
      matrix(0, 0, 3) %>% setColnames(c('route', 'time', 'count'))
    }
  }

  ## return empty if no passenger
  if(nrow(tickets) == 0) {
    return(list(tickets = NULL, occupancy = init.occupancy, reward = 0))
  }

  ## order passangers by purchase time
  tickets <- tickets[order(tickets[,'time']),, drop = F]

  ## bind route information to each ticket
  tickets %<>% cbind(routes[tickets[,'route'], c('from', 'to'), drop = F])
  tickets <<-tickets; init.occupancy <<- init.occupancy
  ## calculate cumulated occupancy
  occupancy <- routes[tickets[, 'route'], grepl('^occup.*', colnames(routes)), drop = F] %>%
    sweep(1, tickets[,'count'], '*') %>%
    apply(2, cumsum) %>%
    KeepAsMatrix() %>%
    sweep(2, init.occupancy, '+')

  out <- DropExceeding(tickets, occupancy, nseats)
  out$reward <- price[out$tickets[,'route']] %*% out$tickets[,'count'] %>% drop
  return(out)

}


# drop exceeding passengers -----------------------------------------------

DropExceeding <- function(tickets, occupancy, nseats) {

  ## if the train did not exceed its capacity return all tickets
  if (nrow(occupancy) == 0 || max(occupancy) <= nseats) {
    return(list(tickets = tickets, occupancy = occupancy[nrow(occupancy),,drop = F]))
  }

  ## find the first passenger (ticket) that exceeds the capacity
  which.exceed <- apply(occupancy > nseats, 1, any) %>% which %>% first

  ## if the exceeding passenger is the last one then return all except the exceeding one
  if (which.exceed == nrow(tickets)) {
    return(list(tickets = tickets[-which.exceed,, drop = F], occupancy = occupancy[which.exceed - 1,]))
  }

  which.drop <- (tickets[which.exceed, 'from'] < tickets[(which.exceed + 1):nrow(tickets), 'to'] |
    tickets[which.exceed, 'to'] < tickets[(which.exceed + 1):nrow(tickets), 'from']) %>%
    which %>% c(0) + which.exceed

  DropExceeding(tickets[-which.drop,, drop = F], occupancy[-which.drop,, drop = F], nseats)
}

printDim <- function(x) {
  if (x %>% dim %>% is.null) {
    x %>% length %>% print
  } else {
    x %>% dim %>% print
  }
}
