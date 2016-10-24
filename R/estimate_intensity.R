

estimate_intensity <- function(data, routes, nseats) {
  
  U_sum <- data %>% 
    lapply(as.data.frame) %>% 
    rbindlist() %>% 
    group_by(route) %>% 
    summarise(beta_1 = n(), beta_2 = sum(log(price)), beta_3 = sum(time), beta_4 = sum(time * log(price))) %>% 
    select(-route) %>% 
    as.matrix()
  
  data %>% 
    sapply(available_until, routes = routes, nseats = nseats) %>% 
    score_integral(beta)
}

data <- data[[1]]
i <- 1

available_until <- function(data, routes, nseats) {
  foreach(i = 1:nrow(routes), .combine = c) %do% {
    grep('^occup_', colnames(routes)) %>%
      .[as.logical(routes[i, .])] %>% 
      routes[data[,'route'], ., drop = F] %>% 
      apply(2, cumsum) %>% 
      apply(1, max) %>% 
      (function(x) data[which(x == nseats)[1], 'time']) %>% 
      min(routes[i, 'dep_daytime'], na.rm = T)
  }
}

n <- 1
matrix(1, 5, n) %>% 
  apply(2, cumsum)

# t je vektor hornich mezi integralu - jedna trasa, vsechny (N) vlaky
# beta je vektor pro jednu trasu
# pro kazdou horni mez pocitat 4 integraly (4-dim beta)
# vystup je matice N x 4
score_integral <- function(up_bound, beta) {
  nroutes <- nrow(time)
  foreach(i = 1:nroutes, .combine = rbind) %do% {
    # time[i,]
    # beta[i,]
    # funkci pro tohle
    matrix(0, nrow(time), 4) %>% 
      apply(2, sum)
  }
}

matrix(0, 3, 5) %>% 
  apply(1, diag)

