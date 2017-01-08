# last_price <- rep(50, 6)
# init_new_price <- rep(200, 6)
# occupancy <- rep(0, 3)
# time_breakpoint <- 0:1
# routes <- get_routes(4, 60)
# 
# beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
# nseats <- 1000


#' Title
#'
#' @param last_price Numeric vector.
#' @param occupancy Integer vector.
#' @param time_breakpoint Numeric vector.
#' @param routes Matrix.
#' @param beta Matrix.
#' @param nseats Integer.
#' @param init_new_price Numeric vector.
#' @param ce List. See \code{\link{ce_control}}.
#' @param rs List. See \code{\link{rs_control}}.
#'
#' @return Numeric vector of estimated optimal point.
#' @export
#'
#' @examples
#' last_price <- rep(50, 6)
#' occupancy <- rep(0, 3)
#' time_breakpoint <- 0:1
#' routes <- get_routes(4, 60)
#' beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
#' nseats <- 1000
#' 
#' find_next_price(
#'   last_price, occupancy, time_breakpoint, routes, beta, nseats,
#'   init_new_price = 1.1 * last_price, ce, rs
#' )

find_next_price <- function(last_price, occupancy,
                            time_breakpoint, routes, beta, nseats,
                            init_new_price = 1.1 * last_price,
                            ce = ce_control(k = length(last_price)), rs = rs_control()) {
  
  objective_function <- function (price) {
    if (any(price <= 0)) return(0)
    single_interval <- simulate_all_routes(routes, model = 1, beta, price, from = time_breakpoint[1], to = time_breakpoint[2], nseats, occupancy, keep_all = F)
    if (length(time_breakpoint) == 2) {
      single_interval$reward
    } else {
      single_interval$reward + simulate_upcoming_reward(price, single_interval$occupancy, time_breakpoint[-1], routes, beta, nseats)
    }
  }
  
  # conditions for penalty function
  conditions <- as.data.frame(routes) %>% 
    mutate(id = row_number()) %>% 
    select(id, from, to) %>% 
    crossing(., ., .) %>% 
    set_colnames(paste(colnames(.), rep(c('a', 'b', 'c'), each = ncol(.) / 3), sep = '_')) %>% 
    filter(to_a == from_b, from_a == from_c, to_b == to_c) %>% 
    select_if(grepl('^id_', colnames(.))) %>% 
    crossing(data_frame(A = list(ac = c(-1, 0, 1), bc = c(0, -1, 1), abc = c(1, 1, -1))))
  
  # put conditions to linear inequalities
  A <- matrix(0, nrow(conditions), nrow(routes))
  b <- rep(0, nrow(A))
  for(i in seq_len(nrow(conditions)))
    A[i, unlist(conditions[i, 1:3])] <- conditions$A[[i]]
  A <- rbind(A, diag(1, length(last_price)))
  b <- c(b, last_price)
  
  penalty_function <- function (price) {
    sum(pmax(0, b - A %*% price)) +
      sum(pmax(0, 50 - price)) +
      sum(pmax(0, last_price - price))
  }
  
  weight_function <- function(x, alpha) {
    q <- quantile(x, probs = alpha)
    pmax(1 - (x / q)^2, 0)
  }
  
  objective_with_penalty <- function(objective, penalty, theta) {
    objective - theta * penalty
  }
  
  
  # initialization of data objects & parameters -----------------------------
  
  new_price <- init_new_price
  theta <- 1000 # coefficient of penalty function

  n_total <- ce$niter * ce$nsimul + rs$niter * rs$nsimul
  data_price <- matrix(0, nrow = n_total, ncol = length(new_price))
  data_response <- data.frame(objective = numeric(n_total), penalty = numeric(n_total), y = numeric(n_total))
  idx_all <- integer()

  
  # cross entropy -----------------------------------------------------------
  
  for (i in seq_len(ce$niter)) {
    
    cat('\rCross Entropy:', i, '/', ce$niter)

    idx_new <- seq_len(ce$nsimul) + (i-1) * ce$nsimul
    idx_all <- c(idx_all, idx_new)
    
    noised_prices <- rmvnorm(ce$nsimul, new_price, ce$sigma)
    
    response <- data_frame(
      objective = apply(noised_prices, 1, objective_function),
      penalty = apply(noised_prices, 1, penalty_function),
      y = objective_with_penalty(objective, penalty, theta)
    )
    
    # save new data
    data_price[idx_new,] <- noised_prices
    data_response[idx_new,] <- response
    
    top_performers <- which(rank(-response$y) <= ce$nsimul * ce$rho)
    
    new_price <- noised_prices[top_performers,] %>% apply(2, mean)
    ce$sigma <- noised_prices[top_performers,] %>% var
    
    par(mfrow = c(1,1))
    outer(sqrt(diag(ce$sigma)), c(-1, 0, 1) * 1.96, '*') %>% 
      sweep(1, new_price, '+') %>% 
      matplot(type = 'b', lty = 1, pch = 16, col = c('blue', 'black', 'blue'),
              xlab = 'Route ID', ylab = 'Price (latest iteration)')
  }
  
  ce_result <<- new_price

    
  # response surface --------------------------------------------------------
  
  for (i in seq_len(rs$niter)) {
    # for (i in seq(i+1, rs$niter)) {
      
    cat('\rResponse Surface:', i, '/', rs$niter)
    
    idx_new <- seq_len(rs$nsimul) + (i-1) * rs$nsimul + ce$niter * ce$nsimul
    idx_all <- c(idx_all, idx_new)
    
    noise <- rnorm(rs$nsimul * length(init_new_price), sd = rs$sigma) %>% matrix(nrow = rs$nsimul)
    noised_prices <- sweep(noise, 2, new_price, '+')
    
    response <- data_frame(
      objective = apply(noised_prices, 1, objective_function),
      penalty = apply(noised_prices, 1, penalty_function),
      y = objective_with_penalty(objective, penalty, theta)
    )
    
    # save new data
    data_price[idx_new,] <- noised_prices
    data_response[idx_new,] <- response
    
    data_centered <- data_price[idx_all,] %>% 
      sweep(2, new_price, '-')
    
    weight <- data_centered %>% 
      raise_to_power(2) %>% 
      apply(1, sum) %>% 
      sqrt() %>% 
      weight_function(rs$alpha)
    
    idx_neighbours <- which(weight > 0)
    X <- cbind(1, data_centered[idx_neighbours,]) %>%
      sweep(1, sqrt(weight[idx_neighbours]), '*')
    Xt <- t(X)
    Y <- data_response$y[idx_neighbours] * sqrt(weight[idx_neighbours])

    param_est <- solve(Xt %*% X, Xt %*% Y)
    
    intercept <- param_est[1]
    direction <- pmax(pmin(param_est[-1] * rs$step_len, rs$max_step), -rs$max_step)
    if (any(is.na(direction))) stop()
    
    par(mfrow = c(2,2))
    # hist(weight[idx_neighbours], main = '', xlab = 'Kernel function')
    hist(data_response$y[idx_neighbours], main = '', xlab = 'Objective function (simulated)')
    plot(x = weight[idx_neighbours], y = data_response$y[idx_neighbours], xlab = 'Kernel function', ylab = 'Objective function (simulated)')
    points(1, intercept, pch = 16, col = 'red')
    matplot(cbind(new_price, new_price + direction), type = 'b', pch = 16, lty = 1,
            xlab = 'Route ID', ylab = 'Price (latest iteration)')
    barplot(direction, xlab = 'Route ID', ylab = 'Price change since previous iteration')

    new_price <- new_price + direction
  }
  
  cat('\rOptimization finished')
  return(new_price)
}


simulate_upcoming_reward <- function(last_price, occupancy, time_breakpoint, routes, beta, nseats) {
  cat('\n optimizing second stage\n')
  print(occupancy)
  next_price <- find_next_price(last_price, occupancy, time_breakpoint, routes, beta, nseats,
                                ce = ce_control(length(last_price), niter = 5, nsimul = 100),
                                rs = rs_control(niter = 50))
  replicate(100, simulate_all_routes(routes, 1, beta, next_price, time_breakpoint[1], time_breakpoint[2], nseats, occupancy, keep_all = F)$reward) %>% 
    mean()
  
}


