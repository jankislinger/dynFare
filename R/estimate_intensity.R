#' Estimate intensity
#' 
#' Estimates parameters of intensity function
#'
#' @param data 
#' @param ticket_price 
#' @param routes 
#' @param nseats 
#'
#' @return
#' @export
#'
#' @examples

estimate_intensity <- function(data, ticket_price, routes, nseats) {
  
  routes <- cbind(route = 1:nrow(routes), routes) %>% as.data.frame()
  beta <- matrix(rep(c(18,-4,4, 1.2), each = nrow(routes)), nrow = nrow(routes)) # initial value for iterations

  U_sum <- data %>% 
    group_by(route) %>% 
    summarise(beta_1 = n(), beta_2 = sum(log(price)), beta_3 = sum(time), beta_4 = sum(time * log(price))) %>% 
    select(-route) %>% 
    as.matrix()
  
  ntrains <- length(unique(data$train))
  from_st <- routes$from %>% unique()
  stations_occup <- expand.grid(from = from_st, occup_from = from_st,to = from_st + 1, occup_to = from_st + 1) %>% 
    filter(from <= occup_from, occup_from + 1 == occup_to, occup_to <= to)
  
  available <- data %>% 
    left_join(stations_occup, by = c('from', 'to')) %>% 
    group_by(train, occup_from, occup_to) %>% 
    summarise(time_until = available_until(time, nseats)) %>% 
    ungroup() %>% 
    left_join(stations_occup, by = c('occup_from', 'occup_to')) %>% 
    group_by(train, from, to) %>% 
    summarise(time_until = min(time_until)) %>% 
    ungroup() %>% 
    left_join(routes, by = c('from', 'to')) %>% 
    mutate(time_until = pmin(time_until, dep_daytime)) %>% 
    select(train, route, time_until)
  
  # ticket prices by intervals
  by_intervals <- ticket_price %>% 
    left_join(available, c('train', 'route')) %>% 
    mutate(valid_from = pmin(valid_from, time_until), valid_to = pmin(valid_to, time_until)) %>% 
    group_by(route, valid_from, valid_to, price) %>% 
    summarise(count = n())

  U_int <- function(beta) {
    environment() %>% as.list() %>% print()
    by_intervals %>% 
      mutate(beta = lapply(route, function(i) data.frame(index = 1:4, value = beta[i,]))) %>% 
      mutate(U_int = Map(integrate_U, as.list(valid_from), as.list(valid_to), as.list(price), beta)) %>% 
      unnest(U_int) %>% 
      group_by(route, index) %>% 
      summarise(U_int = sum(value)) %>% 
      ungroup() %>% 
      spread(index, U_int) %>% 
      select(-route) %>% 
      as.matrix()
  }
  
  I <- function(beta) {
    by_intervals %>% 
      mutate(beta = apply(beta, 1, function(x) data.frame(index = 1:4, value = x))[route]) %>% 
      mutate(I = Map(integrate_I, as.list(valid_from), as.list(valid_to), as.list(price), beta)) %>% 
      group_by(route) %>% 
      summarise(nI = list(Reduce('+', I))) %>% 
      extract2('nI')
  }
  
  newton_raphson <- function(beta) {
    delta <- 1e-3
    U <- U_sum - U_int(beta)
    nI <- I(beta)
    j <- 0
    while (sum(U^2) > delta) {
      for (i in 1:nrow(beta)) {
        beta[i,] <- beta[i,] + solve(nI[[i]], U[i,])
      }
      U <- U_sum - U_int(beta)
      nI <- I(beta)
      j <- j + 1
      cat('done', j, ', dist =', sum(U^2), '\n')
    }
    I <- lapply(nI,  '/', ntrains)
    se <- I %>% lapply(diag) %>% do.call(what = 'rbind') %>% {1 / sqrt(.) / sqrt(ntrains)}
    list(beta = beta, U = U, I = I, se = se)
  }
  
  estimator <- newton_raphson(beta)
  return(estimator)
}

available_until <- function(time, nseats) {
  if (length(time) < nseats) 1 else max(time)
}

integrate_U <- function(t_low, t_up, price, beta) {
  U <- foreach(i = beta$index, .combine = c) %do% {
    f <- function(t) {psi[[i]](t, price) * lambda(t, price, beta$value)}
    integrate(f, t_low, t_up)$value
  }
  data.frame(index = beta$index, value = U)
}
#' @export
integrate_I <- function (t_low, t_up, price, beta) {
  outer(
    beta$index, beta$index,
    Vectorize(
      function(i,j) {
        f <- function(t) {psi[[i]](t, price) * psi[[j]](t, price) * lambda(t, price, beta$value)}
        integrate(f, t_low, t_up)$value
      }
    )
  )
}
#' @export
psi <- list(
  psi_1 = function(t, p) rep(1, length(t)),
  psi_2 = function(t, p) rep(log(p), length(t)),
  psi_3 = function(t, p) t,
  psi_4 = function(t, p) t * log(p)
)

lambda <- function(t, p, beta) {
  sapply(psi, function(f) f(t, p)) %*% beta %>% 
    as.vector() %>% exp()
}


