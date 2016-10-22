#' Simulate train demand for whole train and whole selling period
#' 
#' @description 
#' Simulate the demand for whole train and whole selling period divided to multiple periods over which the price is constant.
#' 
#' @param routes Output of \link{get_routes}. Do not build this argument manually.
#' @param model Model selection; either 1 or 2. See details.
#' @param beta Numeric matrix of model parameters. Each row contains parameters for one route.
#'             The number of columns corresponds with the model selection. See details.
#' @param price Matrix of ticket prices. Each column represents one route, each row represents one time period.
#' @param time_bp Vector of times when the price changes. Values between 0 and 1.
#' @param nseat Number of seats in the train.
#' @param keep_all Logical; Should the passengers that exceed the capacity be kept in the output?
#' 
#' @details 
#' If \code{keep_all} is TRUE all demanded tickets are returned no matter the capacity.
#' For each passenger the maximum acceptable price is returned.
#' 
#' @export

simulate_train_demand <- function (routes, model, beta, price, time_bp, nseats, keep_all) {
  
  if (nrow(price) != length(time_bp) + 1)
    stop('Number of rows of "price" (', nrow(price), ') should be greater than length of "time_bp" (', length(time_bp), ') by one.')
  
  time_bp <- c(0, time_bp, 1)
  
  occupancy <- colnames(routes) %>% grepl(pattern = '^occup_') %>% sum() %>% rep(x = 0)
  tickets <- NULL
  reward <- 0
  
  for (i in seq(length(time_bp) - 1)) {
    
    from <- time_bp[i]
    to <- time_bp[i+1]
    
    new_data <- simulate_all_routes(routes, model, beta, price[i,], from, to, nseats, occupancy, keep_all)
    
    tickets <- rbind(tickets, new_data$tickets)
    occupancy <- new_data$occupancy
    reward <- reward + new_data$reward
  }
  
  list(tickets = tickets, occupancy = occupancy, reward = reward)
}