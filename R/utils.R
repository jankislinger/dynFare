


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
