library(dynFare)
context("Simulate Markov process")

test_that('standard return', {
  
  routes         <- get_routes(4, 60, c(0.3, 0.4, 0.5))
  beta           <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
  price          <- rep(250, 6)
  init_occupancy <- rep(0, 3)
  
  x <- simulate_all_routes(model = 1, routes, beta, price = price,
                           from = 0, to = 1, nseats = 10, init_occupancy)
  
  expect_type(x, 'list')
  expect_named(x, c('tickets', 'occupancy', 'reward'))
  expect_type(x$tickets, 'double')
  expect_type(x$occupancy, 'double')
  expect_type(x$reward, 'double')
})