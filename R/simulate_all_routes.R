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
#' @param keep_all Logical; Should the passengers that exceed the capacity be returned as well?
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
#' simulate_all_routes(model = 1, routes, beta, price = price,
#'                     from = 0, to = 1, nseats = 10, init_occupancy)
#' 
#' @export


simulate_all_routes <- function(routes, model, beta, price, from, to, nseats, init_occupancy, keep_all) {

  ## set maximal from and to values for time
  from <- pmin(from, routes[,'dep_daytime'])
  to   <- pmin(to,   routes[,'dep_daytime'])

  ## simulate Poisson process for each route
  tickets <- foreach(i = 1:nrow(routes), .combine = rbind) %do% {

    pois <- simulate_one_route(model, beta[i,], price[i], from[i], to[i], nseats, keep_all)

    if (length(pois) > 0 && keep_all) {
      cbind(route = i, time = pois, count = 1, price = price[i]) %>% 
        as.data.frame() %>% 
        mutate(max_price = maximum_price(beta[i,], price, time)) %>%
        as.matrix()
    } else if (length(pois) > 0) {
      cbind(route = i, time = pois, count = 1, price = price[i])
    } else if (keep_all) {
      matrix(0, 0, 5, dimnames = list(NULL, c('route', 'time', 'count', 'price', 'max_price')))
    } else {
      matrix(0, 0, 4, dimnames = list(NULL, c('route', 'time', 'count', 'price')))
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
  
  if (!keep_all) {
    tickets <- drop_exceeding(tickets, init_occupancy, nseats, routes)
  }

  ## calculate cumulated occupancy
  occupancy <- routes[tickets[, 'route'], grepl('^occup.*', colnames(routes)), drop = F] %>%
    sweep(1, tickets[,'count'], '*') %>%
    apply(2, cumsum) %>%
    KeepAsMatrix() %>%
    sweep(2, init_occupancy, '+')
  
  list(
    tickets = tickets,
    occupancy = occupancy_by_passengers(tickets, init_occupancy, routes) %>% tail(1),
    reward = price[tickets[,'route']] %*% tickets[,'count'] %>% drop()
  )
}


#' Drop exceeding passengers
#' 
#' Loops iteratively through all tickets and excludes passengers that do not fit the cappacity of a train.
#' 
#' @param tickets Matrix containing information about the tickets.
#' @param occupancy Matrix of train occupancy after a passanger buys a ticket.
#'                  Rows are related to passengers, columns to pairs of consecutive stations.
#' @param nseat Number of seats in the train.
#' 
#' @export

drop_exceeding <- function(tickets, init_occupancy, nseats, routes) {

  occupancy <- occupancy_by_passengers(tickets, init_occupancy, routes)
  
  ## if the train did not exceed its capacity return all tickets
  if (occupancy %>% tail(1) %>% max() %>% is_weakly_less_than(nseats)) {
    return(tickets)
  }

  ## find the first passenger (ticket) that exceeds the capacity
  which_exceed <- apply(occupancy > nseats, 1, any) %>% which() %>% first()

  ## if the exceeding passenger is the last one then return all except the exceeding one
  if (which_exceed == nrow(tickets)) {
    return(tickets[-which_exceed,, drop = F])
  }

  which_drop <- (tickets[which_exceed, 'from'] >= tickets[(which_exceed + 1):nrow(tickets), 'from'] &
    tickets[which_exceed, 'to'] <= tickets[(which_exceed + 1):nrow(tickets), 'to']) %>%
    which %>% c(0) + which_exceed

  drop_exceeding(tickets[-which_drop,, drop = F], init_occupancy, nseats, routes)
}

occupancy_by_passengers <- function(tickets, init_occupancy, routes) {
  env <<- environment()
  routes[tickets[, 'route'], grepl('^occup.*', colnames(routes)), drop = F] %>%
    sweep(1, tickets[,'count'], '*') %>%
    apply(2, cumsum) %>%
    KeepAsMatrix() %T>%
    assign(x = 'occup_temp', envir = .GlobalEnv) %>% 
    sweep(2, init_occupancy, '+')
}
