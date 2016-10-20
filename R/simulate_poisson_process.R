#' Simulate demand for single route
#' 
#' @description 
#' Simulates demand for single route over given time interval as an inhomogeneous Poisson process.
#' 
#' @param model Model selection; either 1 or 2. See details.
#' @param beta Numeric vector of model parameters. The length corresponds with the model selection. See details.
#' @param price Price of a ticket for selected time interval and route.
#' @param from Lower bound of the time interval over which the process should be simulated.
#' @param to Upper bound of the time interval over which the process should be simulated.
#' 
#' @details
#' There are two models for the intensity function:
#' \deqn{\lambda_1(price, time) = exp(\beta_1 + \beta_2 log (price) + \beta_3 time + \beta_4 time log(price))}
#' \deqn{\lambda_2(price, time) = exp(not ready yet)}
#' Argument \code{model} selects the designated model.
#' 
#' @return Vector of arrival (purchase) times. If there is no purchase \code{numeric(0)} is returned.
#' 
#' @examples 
#' simulate_poisson_process(model = 1, beta = c(18,-4, 4, 1.2), price = 250,
#'                          from = 0, to = 1)
#' 
#' @export


simulate_poisson_process <- function(model, beta, price, from, to) {
  
  if (! model %in% 1:2)
    warning('Unspecified model: taking model = 1')
  
  # .lambda    intensity function
  # .Lambda    comulative intensity (integral of .lambda from t1 to t2)
  # .LambdaInv inverse cumulative intensity (t2: .Lambda(t1, t2) == r)
  
  if (model == 1) {
    A <- beta[c(1,3)] + beta[c(2,4)] * log(price)
    .lambda <- function(t) exp(cbind(1, t) %*% A) %>% as.vector
    .Lambda <- function(t1, t2) (.lambda(t2) - .lambda(t1)) / (A[2])
    .LambdaInv <- function(r, t1) log(A[2] * r / exp(A[1]) + exp(A[2] * t1)) / (A[2])
  } else if (model == 2) {
    ## periodicky model
  }
  
  ## get total cumulative intensity for selected interval and simulate number of tickets
  Lambda <- .Lambda(from, to)
  N <- rpois(1, Lambda)
  rexp(N+1) %>% relative_cumsum(Lambda) %>% .LambdaInv(from)
}


relative_cumsum <- function(x, total = 1) {
  cumsum(x)[-length(x)] / sum(x) * total
}

simulate_poisson_process(model = 1, beta = c(18,-4,4, 1.2), price = 250,
                         from = 0, to = 1)
