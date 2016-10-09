generate.process <- function(beta, from, to, price) {
  
  ## intensity function - lambda
  lambda.fun <- function(t) {
    as.vector(exp(cbind(1, log(t), log(price)) %*% beta))
  }
  
  ## cumulated intensity function - Lambda
  Lambda.fun <- function(t) {
    sapply(t, function(t) integrate(lambda.fun, from, t)$value)
  }
  
  ## inverse of cumulated intensity function - Lambda^(-1)
  Lambda.inv.fun <- function(t) {
    solve <- function(t) uniroot(function(x) Lambda.fun(x) - t, c(from, to))$root
    sapply(t, solve)
  }
  
  ## generate ordered values from uniform distribution and find inverse Lambda
  time <- rexp(rpois(1, Lambda.fun(to)) + 1, 1)
  Lambda.inv.fun(cumsum(time)[-length(time)] / sum(time) * Lambda.fun(to))
}