#' Control for Cross-Entropy Algorithm
#'
#' @param k Integer; dimension of price.
#' @param niter Integer; number of iterations.
#' @param nsimul Integer; number of simulations in one iteration.
#' @param sigma Numeric matrix (\code{k x k}, positive definite); initial variance matrix of prices.
#' @param rho Numeric (\code{0 < rho < 1}); how many (ratio of total) best performers to take.
#'
#' @return List with control parameters.
#' @export
#' 
#' @seealso \code{\link{find_next_price}}

ce_control <-
  function (
    k,
    niter  = 20,
    nsimul = 2000,
    sigma  = diag(500, k),
    rho    = 0.25
  ) {
    as.list(environment())
  }

#' Control for Response Surface Algorithm
#'
#' @param niter Integer; number of iterations.
#' @param nsimul Integer; number of simulations in one iteration.
#' @param max_step Numeric; maximum change of price between two iterations in one dimension.
#' @param step_len Numeric; coefficient of step length. Estimated gradient is multiplied by this value.
#' @param sigma Numeric (positive); standard deviation of price simulated around current iteration.
#' @param alpha Numeric (\code{0 < alpha < 1}); how many (ratio of total) nearest observations to take for kernel regression.
#'
#' @return List with control parameters.
#' @export
#' 
#' @seealso \code{\link{find_next_price}}

rs_control <-
  function (
    niter = 200,
    nsimul = 500,
    max_step = 15,
    step_len = 1/20,
    sigma = 10,
    alpha = 0.3
  ) {
    as.list(environment())
  }
