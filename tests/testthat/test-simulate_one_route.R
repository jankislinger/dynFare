library(dynFare)
context("Simulate Poisson process")

test_that('standard return', {
  
  beta <- c(18,-4, 4, 1.2)
  
  expect_silent(x <- simulate_one_route(1, beta, 250, 0, 1))
  expect_type(x, 'double')
})