#' Simulate demand for whole train
#' 
#' @description
#' Simulates demand for whole train over given time interval as an inhomogeneous
#' Markov process (combination of multiple Poisson processes).
#' 
#' @param routes Output of \link{get_routes}. Do not build this argument manually.
#' @param model Model selection; either 1 or 2. See details.
#' @param beta Numeric matrix of model parameters. Each row contains parameters for one route.
#'             The number of columns corresponds with the model selection. See details.
#' @param price Vector of ticket prices. Each element represents one route.
#' @param from Lower bound of the time interval over which the process should be simulated.
#' @param to Upper bound of the time interval over which the process should be simulated.
#' @param nseat Number of seats in the train.
#' @param init_occupancy Occupancy of the train at time \code{from}. Represents state of the Markov process.
#' 
#' @details 
#' 	The models for intensity function are described in documentation for function \link{SimulatePoissonProcess}.
#' 	
#' @return 
#' 	\describe{
#' 	  \item{tickets}{Matrix containing information about purchased tickets during given time interval.}
#' 	  \item{occupancy}{Occupancy of the train at time \code{to}.}
#' 	  \item{reward}{Cumulative reward of the transitions. Sum of prices of sold tickets.}
#' 	}
#' 
#' @examples 
#' routes         <- get_routes(4, 60, c(0.3, 0.4, 0.5))
#' beta           <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
#' price          <- rep(250, 6)
#' init_occupancy <- rep(0, 3)
#' 
#' simulate_markov_process(model = 1, routes, beta, price = price,
#'                         from = 0, to = 1, nseats = 10, init_occupancy)
#' 
#' @export


simulate_all_routes <- function(routes, model, beta, price, from, to, nseats, init_occupancy) {

  ## set maximal from and to values for time
  from <- pmin(from, routes[,'dep_daytime'])
  to   <- pmin(to,   routes[,'dep_daytime'])

  ## simulate Poisson process for each route
  tickets <- foreach(i = 1:nrow(routes), .combine = rbind) %do% {

    pois <- simulate_one_route(model, beta[i,], price[i], from[i], to[i])

    if (length(pois) > 0) {
      cbind(route = i, time = pois, count = 1)
    } else {
      matrix(0, 0, 3, dimnames = list(NULL, c('route', 'time', 'count')))
    }
  }

  ## return empty if no passenger
  if(nrow(tickets) == 0) {
    return(list(tickets = NULL, occupancy = init_occupancy, reward = 0))
  }

  ## order passangers by purchase time
  tickets <- tickets[order(tickets[,'time']),, drop = F]

  ## bind route information to each ticket
  tickets %<>% cbind(routes[tickets[,'route'], c('from', 'to'), drop = F])

  ## calculate cumulated occupancy
  occupancy <- routes[tickets[, 'route'], grepl('^occup.*', colnames(routes)), drop = F] %>%
    sweep(1, tickets[,'count'], '*') %>%
    apply(2, cumsum) %>%
    KeepAsMatrix() %>%
    sweep(2, init_occupancy, '+')

  out <- drop_exceeding(tickets, occupancy, nseats)
  out$reward <- price[out$tickets[,'route']] %*% out$tickets[,'count'] %>% drop
  return(out)

}


# drop exceeding passengers -----------------------------------------------

drop_exceeding <- function(tickets, occupancy, nseats) {

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

  drop_exceeding(tickets[-which.drop,, drop = F], occupancy[-which.drop,, drop = F], nseats)
}
