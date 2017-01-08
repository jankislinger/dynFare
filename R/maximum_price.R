#' Maximum acceptable price for passangers
#' 
#' Simulate maximum price that a passanger is available to pay for the ticket.
#' The distribution of maximum price is conditional on current price - only passengers that would buy the ticket for \code{price}.
#' 
#' @param beta Parameters of the Poisson process
#' @param price Actual price of the tickets.
#' @param time Vector of times of purchases for individual passengers
#' 
#' @export

maximum_price <- function(beta, price, time) {
  warning('Dummy function "maximum_price".')
  price + runif(length(time), max = 50)
}