library(dynamicFare)
context("Simulate Poisson process")

test_that('standard return', {
  x <- simulate_poisson_process(1, rep(0.1, 4), 150, 0, 1)
  expect_type(x, 'double')
})