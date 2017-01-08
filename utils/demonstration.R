require(dynFare)


# Simulate Poisson process ------------------------------------------------

model <- 1
beta <- c(18,-4,4, 1.2)
price <- 250
from <- 0
to <- 0.98

simulate_one_route(model, beta, price, from, to)


# Simulate Markov process -------------------------------------------------

model <- 1
routes <- get_routes(4, 60, c(0.3, 0.4, 0.5))
beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price <- rep(250, 6)
from <- 0; to <- 1
nseats <- 10
init_occupancy <- rep(0, 3)


simulate_all_routes(routes, model, beta, price, from, to, nseats, init_occupancy, keep_all = F)


# simulate whole train ----------------------------------------------------

nstations <- 4
routes <- get_routes(nstations, 60, c(0.3, 0.4, 0.5))
model <- 1
beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price <- matrix(250, 2, 6)
time_bp <- 0.5
nseats <- 10

set.seed(1)
simulate_train_demand(routes, model, beta, price, time_bp, nseats, keep_all = F)
simulate_train_demand(routes, model, beta, price, time_bp, nseats, keep_all = T)


# generate data -----------------------------------------------------------

set.seed(123456)

time_bp <- c(30, 46, 54, 58) / 60
ntrains <- 500
nstations <- 6
nseats <- 1250
routes <- get_routes(nstations, dep_day = 60, dep_time = c(0.4, 0.44, 0.48, 0.52, 0.56, 0.6))

beta <- matrix(rep(c(18,-4,4, 1.2), each = nrow(routes)), nrow = nrow(routes))
beta[,1] <- as_data_frame(routes) %$% identity(16 + (to - from))
beta <- beta + rmvnorm(nrow(beta), sigma = diag(c(1, 0.2, 0.15, 0.01)))
beta <- round(beta, 2)
price <- as_data_frame(routes) %$%
  outer(1:5, to - from, function(m, k) {100 * k^(6.05/7) * 1.15^m}) %>% 
  round(0)

data <- generate_data(beta, price, time_bp, ntrains, nstations, nseats)

# ticket_price <- expand.grid(train = 1:ntrains, route = 1:nrow(routes), valid_from = c(0, time_bp)) %>%
#   mutate(valid_to = pmin(valid_from + 0.8, 1), price = 250 + 100 * valid_from / 0.8)

ticket_price <- as_data_frame(t(price)) %>% 
  mutate(route = row_number()) %>% 
  gather(interval_chr, price, -route) %>% 
  mutate(interval = as.integer(gsub("[^0-9]", "", interval_chr))) %>% 
  mutate(valid_from = c(0,time_bp)[interval], valid_to = c(time_bp, 1)[interval]) %>% 
  crossing(train = seq_len(ntrains)) %>% 
  select(train, route, valid_from, valid_to, price) %>% 
  arrange(train, route, valid_from)

estim <- estimate_intensity(data, ticket_price, routes, nseats)

estim$beta
estim$I %>% lapply(diag) %>% do.call(what = 'rbind') %>% 
{1/sqrt(.) * 1.96}

abs(beta - estim$beta) / estim$se


.lambda(0:1)
.Lambda(0,1)
devtools::use_testthat()

get_routes(5, 20, seq(0.2, 0.8, length.out = 4))

data %>%
  group_by(train) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = count)) + 
  geom_histogram()

data %>%
  mutate(route_char = paste(from, to, sep = ' -> ')) %>% 
  ggplot(aes(x = route_char)) +
  geom_bar() +
  theme_light()

data %>%
  ggplot(aes(x = time)) +
  geom_histogram()
