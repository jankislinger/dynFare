\name{SimulateMarkovProcess}
\alias{SimulateMarkovProcess}
\title{Simulate demand for whole train}

\description{Simulates demand for whole train over given time interval as an inhomogeneous Markov process (combination of multiple Poisson processes).}

\usage{SimulateMarkovProcess(model, routes, beta, price, from, to, nseats, init.occupancy)}

\arguments{
	\item{model}{Model selection; either 1 or 2. See details.}
	\item{routes}{Numeric matrix containing information about the routes. Each row represents one route. The order of routes needs to be the same in all arguments. See examples.}
	\item{beta}{Numeric matrix of model parameters. Each row contains parameters for one route. The number of columns corresponds with the model selection. See details.}
	\item{price}{Vector of ticket prices. Each element represents one route.}
	\item{from, to}{Boundaries of the time interval over which the process should be simulated.}
	\item{nseats}{Number of seats in the train.}
	\item{init.occupancy}{Occupancy of the train at time \code{from}. Represents state of the Markov process.}
}

\details{
	The models for intensity function are described in documentation for function \link{SimulatePoissonProcess}.
}

\value{List of details about the generated process.
		\item{tickets}{Matrix containing information about purchased tickets during given time interval.}
		\item{occupancy}{Occupancy of the train at time \code{to}}
		\item{reward}{Cumulative reward of the transitions. Sum of prices of sold tickets.}
}

\examples{
## demonstrational arguments:
routes         <- getRoutes(4, 60, c(0.3, 0.4, 0.5))
beta           <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price          <- rep(250, 6)
init.occupancy <- rep(0, 3)

## call of the function
SimulateMarkovProcess(model = 1, routes, beta, price = price,
  from = 0, to = 1, nseats = 10, init.occupancy)
}
