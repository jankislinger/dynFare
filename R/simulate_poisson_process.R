SimulatePoissonProcess <- function(model, beta, price, from, to) {
  
  if (! model %in% 1:2) {
    warning('Unspecified model: taking model = 1')
  }
  
  if (model == 1) {
    A <- sapply(c(0,2), function(i) {beta[1+i] + beta[2+i] * log(price)})
    .lambda <- function(t) exp(cbind(1, t) %*% A) %>% as.vector
    .Lambda <- function(t1, t2) (.lambda(t2) - .lambda(t1)) / (A[2])
    .LambdaInv <- function(r, t1) log(A[2] * r / exp(A[1]) + exp(A[2] * t1)) / (A[2])
  } else if (model == 2) {
    ## periodicky model
  }
  
  ## get total cumulative intensity for selected interval and simulate number of tickets
  Lambda <- .Lambda(from, to)
  N <- rpois(1, Lambda)
  rexp(N+1) %>% relativeCumsum(Lambda) %>% .LambdaInv(from)
  
}