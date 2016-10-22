library(dynFare)
context("Simulate all routes")

test_that('standard return', {
  
  routes         <- get_routes(4, 60, c(0.3, 0.4, 0.5))
  beta           <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
  price          <- rep(250, 6)
  init_occupancy <- rep(0, 3)
  
  expect_silent(x <- simulate_all_routes(routes, 1, beta, price, 0, 1, 10, init_occupancy, F))
  
  expect_type(x, 'list')
  expect_named(x, c('tickets', 'occupancy', 'reward'))
  expect_type(x$tickets, 'double')
  expect_type(x$occupancy, 'double')
  expect_type(x$reward, 'double')
})