library(dynFare)
context("Simulate Poisson process")

test_that('standard return', {
  x <- simulate_one_route(model = 1, beta = c(18,-4, 4, 1.2), price = 250,
                                from = 0, to = 1)
  expect_type(x, 'double')
})