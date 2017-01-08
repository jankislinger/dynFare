library(dynFare)
context("Simulate demand")

test_that('simulate one route', {
  
  beta <- c(18,-4, 4, 1.2)
  
  expect_silent(x <- simulate_one_route(model = 1, beta, price = 250, from = 0, to = 1))
  expect_type(x, 'double')
})

test_that('simulate all routes', {
  
  expect_silent(
    routes <- get_routes(4, 60, c(0.3, 0.4, 0.5))
  )
  
  expect_is(routes, 'matrix')
})


test_that('simulate all routes', {
  
  routes         <- get_routes(4, 60, c(0.3, 0.4, 0.5))
  beta           <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
  price          <- rep(250, 6)
  init_occupancy <- rep(0, 3)
  
  expect_silent(
    x <- simulate_all_routes(routes, model = 1, beta, price, from = 0, to = 1, nseats = 10, init_occupancy, keep_all = F)
  )
  
  expect_type(x, 'list')
  expect_named(x, c('tickets', 'occupancy', 'reward'))
  expect_type(x$tickets, 'double')
  expect_type(x$occupancy, 'double')
  expect_type(x$reward, 'double')
})

test_that('simulate whole train', {
  
  routes         <- get_routes(4, 60, c(0.3, 0.4, 0.5))
  beta           <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
  price          <- matrix(250, 2, 6)
  time_bp        <- 0.5
  init_occupancy <- rep(0, 3)
  
  expect_silent(
    x <- simulate_train_demand(routes, model = 1, beta, price, time_bp, nseats = 10, keep_all = F)
  )
  
  expect_type(x, 'list')
  expect_named(x, c('tickets', 'occupancy', 'reward'))
  expect_is(x$tickets, 'matrix')
  expect_type(x$tickets, 'double')
  expect_type(x$occupancy, 'double')
  expect_type(x$reward, 'double')
})