
# set dimension names to array --------------------------------------------

setDimnames <- function(array, dimnames) {
  dimnames(array) <- dimnames
  array
}


# set column names to matrix ----------------------------------------------

setColnames <- function(matrix, colnames) {
  colnames(matrix) <- colnames
  matrix
}


# relative cumulative sums ------------------------------------------------

relativeCumsum <- function(x, total = 1) {
  cumsum(x)[-length(x)] / sum(x) * total
}


# get routes --------------------------------------------------------------

getRoutes <- function(nstations, dep_day, dep_time = NULL) {
  routes <- combn(nstations, 2) %>% t %>% setColnames(c('from', 'to'))
  day_time <- cbind(dep_day, dep_time, 1 + (dep_time - 1) / dep_day) %>%
    setColnames(c('dep_day', 'dep_time', 'dep_daytime'))
  occupancy <- apply(routes, 1, function(x) {
    c(rep(0, x[1]-1), rep(1, diff(x)), rep(0, nstations - x[2]))
  }) %>% t %>%
    setColnames(paste0('occup_', 1:(nstations-1), '_', 2:nstations))
  cbind(routes, day_time[routes[,'from'],], occupancy)
}


# keep as matrix ----------------------------------------------------------

KeepAsMatrix <- function(x, column = F) {

  if(is.matrix(x)) {
    x
  } else if(column) {
    x %>% as.matrix
  } else {
    x %>% as.matrix %>% t
  }
}


# save copy of a variable -------------------------------------------------

SaveCopy <- function(x) {

  cat('saving\n')
  if(!exists('debug', envir = .GlobalEnv)) {
    debug <<- list()
  }

  debug[[length(debug) +1]] <<- x

  return(x)
}
