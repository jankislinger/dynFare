#' Generate data
#' 
#' @description 
#' The function generates example data for the statistical model.
#' 
#' @param beta Numeric matrix of model parameters. Each row contains parameters for one route.
#'             The number of columns corresponds with the model selection. See details.
#' @param price Vector of ticket prices. Each element represents one route.
#' @param time_bp Vector of times when the price changes. Values between 0 and 1.
#' @param ntrains Number of datasets to be generated.
#' @param nstations Number of stations of the train.
#' @param nseat Number of seats in the train.
#' 
#' @return 
#' List of datasets of simulated tickets.
#' Each element represents one train.
#' 
#' @examples
#' beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
#' price <- matrix(250, 2, 6)
#' generate_data <- function(beta, price, time_bp = 0.5, ntrains = 3, nstations = 4, nseats = 20)
#' 
#' @export

generate_data <- function(beta, price, time_bp, ntrains, nstations, nseats, keep_all = F) {

  dep_day <- 60
  routes <- get_routes(nstations, dep_day, seq(0.3, 0.5, length.out = nstations - 1))
  

  foreach(j = seq(ntrains), .combine = rbind) %do% {
    single <- simulate_train_demand(routes, 1, beta, price, time_bp, nseats, keep_all)$tickets
    cbind(train = j, single) %>% as.data.frame()
  }
}
