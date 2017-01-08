set.seed(123456)

last_price <- rep(50, nrow(routes))
occupancy <- rep(0, nstations - 1)
time_breakpoint <- 0:1
init_new_price <- round(with(as_data_frame(routes), to - from)^(0.75) * 250, 1)



# single-stage optimization -----------------------------------------------

single_stage <- 
  find_next_price(last_price, occupancy, time_breakpoint, routes, beta, nseats, init_new_price,
                  ce = ce_control(k = length(last_price), sigma = diag(init_new_price * 4)))

save(single_stage, file = 'demo/optimization.rda')


round(ce_result, 1) %>% paste(collapse = ' & ')
replicate(1000, simulate_train_demand(routes, 1, beta, matrix(ce_result, 1), NULL, nseats, F)$reward) %>% 
  t.test() %>% unclass()

round(single_stage, 1) %>% paste(collapse = ' & ')
replicate(1000, simulate_train_demand(routes, 1, beta, matrix(single_stage, 1), NULL, nseats, F)$reward) %>% 
  t.test() %>% unclass()


# two-stage optimization --------------------------------------------------

time_breakpoint <- c(0, 58/60, 1)

two_stage <-
  find_next_price(single_stage, occupancy, time_breakpoint, routes, beta, nseats, init_new_price,
                  ce = ce_control(k = length(last_price), sigma = diag(init_new_price * 4), niter = 3, nsimul = 100),
                  rs = rs_control(niter = 20, nsimul = 20))

round(single_stage - rnorm(15,6, 4), 1) %>% paste(collapse = ' & ')


single_interval
