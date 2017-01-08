require(dynFare)
set.seed(123456)

# generate data -----------------------------------------------------------

time_bp <- c(30, 46, 54, 58) / 60
ntrains <- 500
nstations <- 6
nseats <- 1500
routes <- get_routes(nstations, dep_day = 60, dep_time = c(0.4, 0.44, 0.48, 0.52, 0.56, 0.6))

beta_sigma <- with(list(L = chol(diag(1.2, 4) - matrix(0.2, 4, 4)), sigma = c(0.5, 0.2, 0.2, 0.1)), t(L) %*% diag(sigma) %*% L)

beta <- matrix(rep(c(16,-4,4, 1.2), each = nrow(routes)), nrow = nrow(routes))
beta[,1] <- beta[,1] +
  as_data_frame(routes) %$% identity(to - from)
beta <- beta + rmvnorm(nrow(beta), sigma = beta_sigma)
beta[,1] <- beta[,1] + c(1.5, -0.7, 4.5, 0, -0.7, -0.6, rep(3.5, 2), 7.5, 1.5, 1.3, 3.5, 4.3, 0, 3) - 1.2
beta <- round(beta, 2)
price <- as_data_frame(routes) %$%
  outer(1:5, to - from, function(m, k) {100 * k^(6.05/7) * 1.15^m}) %>% 
  round(0)

data <- generate_data(beta, price, time_bp, ntrains, nstations, nseats)
# data <- rbind(
#   generate_data(beta, price, time_bp, ntrains, nstations, nseats),
#   generate_data(beta, price * 1.2, time_bp, ntrains, nstations, nseats) %>% 
#     mutate(train = train + ntrains)
# )
  
# summary statistics
data %>% 
  group_by(route) %>%
  summarise(count = n())

data %>% 
  group_by(train) %>%
  summarise(count = n())

data %>% 
  filter(route == 4) %$% 
  hist(time, breaks = 50)

save(data, beta, price, time_bp, file = 'generated_data.rda')

# estimate coefficients ---------------------------------------------------

ticket_price <- as_data_frame(t(price)) %>% 
  mutate(route = row_number()) %>% 
  gather(interval_chr, price, -route) %>% 
  mutate(interval = as.integer(gsub("[^0-9]", "", interval_chr))) %>% 
  mutate(valid_from = c(0,time_bp)[interval], valid_to = c(time_bp, 1)[interval]) %>% 
  crossing(train = seq_len(ntrains)) %>% 
  # crossing(group = 0:1) %>% 
  # mutate(train = train + group * ntrains, price = price * 1.2^group) %>% 
  select(train, route, valid_from, valid_to, price) %>% 
  arrange(train, route, valid_from)
  

estim <- estimate_intensity(data, ticket_price, routes, nseats)


# lower bound for variance of estimates -----------------------------------

no_constraints <- ticket_price %>% 
  filter(train == 1) %>% 
  mutate(beta = apply(beta, 1, function(x) data.frame(index = 1:4, value = x))[route]) %>% 
  mutate(I = Map(integrate_I, as.list(valid_from), as.list(valid_to), as.list(price), beta)) %>% 
  group_by(route) %>% 
  summarise(fisher = list(Reduce('+', I))) %>% 
  mutate(sigma = lapply(fisher, solve) %>% lapply('/', sqrt(ntrains)))

no_constraints$sigma[[1]] %>% 
  xtable::xtable() %>% print(include.rownames = F, include.colnames = F)



# outputs -----------------------------------------------------------------

thesis_folder <- C://LaTeX/master_thesis

# distribution of selling times
pdf(paste(thesis_folder, 'figures/histTimes.pdf', sep = '/'), 7, 5)
hist(data$time, breaks = 120, xlab = 'Time', ylab = 'Number of Tickets', main = '')
abline(v = time_bp)
dev.off()

# actual beta and prices
as_data_frame(routes) %>% 
  mutate(route = paste0('(', from, ',', to, ')')) %$%
  cbind(route, cbind(beta, t(price)) %>% as_data_frame()) %>%
  set_colnames(c('route', paste0('beta_', 1:4), paste0('p_', 1:5))) %>% 
  xtable::xtable(digits = c(0, 0, rep(2,4), rep(0,5))) %>% print(include.rownames = F)
  
