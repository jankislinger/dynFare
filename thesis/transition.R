transition <- function (intensities,
                        states = seq(from = 1, to = length(intensities)),
                        begining = 0, precision = 5) {
  
  # check length of lists
  if (length(intensities) != length(states)) {
    warning("List of intensities and list of states need to have the same length")
    stop()
  }
  
  # calculate total intensity from list of intensities
  q <- function(t) {
    q.r <- 0
    for (qi in intensities) {
      q.r <- q.r + qi(t)
    }
    return(q.r)
  }
  
  # transform input into another variables
  s <- begining
  delta <- 10^(-precision)
  
  # set initial values
  x <- 0
  y <- 0
  
  # set initial values for bisection method
  low <- 0
  up <- Inf
  y.low <- 0
  y.up <- 1
  
  # generate random number as a quantile
  alpha <- runif(1)
  
  # set counter
  k <- 0
  
  # loop for Newton's/bisection method
  while (abs(y - alpha) > delta) {
    # increment counter
    k <- k + 1
    
    # calculating auxiliary values at point x
    exp.int <- exp (-integrate(f = q, lower = s, upper = s + x)$value)
    q.x <- q(s+x)
  
    # modifying boundaries for bisection method
    if (x > low && 1-exp.int < alpha) {
      low <- x
      y.low <- 1-exp.int
    } else if (x < up && 1-exp.int > alpha) {
      up <- x
      y.up <- 1-exp.int
    }
    
    # iteration for Newton's/bisection method
    if (k %% 5 == 0 && up < Inf) {
      x <- low + (up-low)*(alpha-y.low)/(y.up-y.low)
    } else if (k %% 5 == 0) {
      x <- x + 0.1
    } else if (x < 0) {
      x <- low + (up-low)*(alpha-y.low)/(y.up-y.low)
    } else if (q.x == 0 && up < Inf) {
      x <- low + (up-low)*(alpha-y.low)/(y.up-y.low)
    } else if (q.x == 0 && 1-exp.int < alpha) {
      x <- x + 0.1
    } else {
      x <- x + (alpha + exp.int - 1)/(exp.int * q.x)
    }
    
    # find CDF for new iteration
    y <- 1 - exp (-integrate(f = q, lower = s, upper = max(s, s + x))$value)
  }
  
  # define vector of conditional probabilities of transitions into states
  prob <- list()
  for (qi in intensities) {
    prob <- append(prob, (qi(x)/q(x)))
  }
  
  # generate target state
  j <- sample(states, 1, prob = prob)[[1]]
  
  # return dwell time and new state
  r <- list(dwellTime = x, state = j, iterations = k, y=y,alpha=alpha)
  return(r)
}