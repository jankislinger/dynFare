#' Get information about routes
#' 
#' Generate a matrix containing all necessary information about the routes
#' 
#' @param nstations Integer; Number of stations of the train.
#' @param dep_day Integer; Which day the train departs. See details.
#' @param dep_time Numeric vector; At which time the train departs from individual stations. See details.
#' 
#' @details 
#' The tickets begin to purchase at midnight before day 1.
#' The train departs at day \code{dep_day}.
#' If \code{dep_time} == 0 the train departs at the midnight before \code{dep_day}-th day.
#' If \code{dep_time} == 1 the train departs at the midnight after \code{dep_day}-th day.
#' All values should be between 0 and 1.
#' Length of vector \code{dep_time} is \code{nstation - 1}.
#' 
#' @export
#' 
#' @examples
#' nstations <- 4
#' dep_day <- 60
#' routes <- get_routes(nstations, dep_day)

get_routes <- function(nstations, dep_day, dep_time = seq(0, 1 - 1 / nstations, length.out = nstations - 1)) {
  
  routes <- combn(nstations, 2) %>% t %>% set_colnames(c('from', 'to'))
  
  day_time <- cbind(dep_day, dep_time, 1 + (dep_time - 1) / dep_day) %>%
    set_colnames(c('dep_day', 'dep_time', 'dep_daytime'))
  
  occupancy <- apply(routes, 1, function(x) {
    c(rep(0, x[1]-1), rep(1, diff(x)), rep(0, nstations - x[2]))
  }) %>% t %>%
    set_colnames(paste0('occup_', 1:(nstations-1), '_', 2:nstations))
  
  cbind(routes, day_time[routes[,'from'],], occupancy)
}
