\name{SimulatePoissonProcess}
\alias{SimulatePoissonProcess}
\title{Simulate demand for single route}

\description{Simulates demand for single route over given time interval as an inhomogeneous Poisson process.}

\usage{SimulatePoissonProcess(model, beta, price, from, to)}

\arguments{
	\item{model}{Model selection; either 1 or 2. See details.}
	\item{beta}{Numeric vector of model parameters. The length corresponds with the model selection. See details.}
	\item{price}{Price of a ticket for selected time interval and route.}
	\item{from, to}{Boundaries of the time interval over which the process should be simulated.}
}

\details{
	There are two models for the intensity function:
	\deqn{\lambda_1(price, time) = exp(\beta_1 + \beta_2 log (price) + \beta_3 time + \beta_4 time log(price))}
	\deqn{\lambda_2(price, time) = exp(not ready yet)}
	Argument \code{model} selects the designated model.
}

\value{Vector of arrival (purchase) times. If there is no purchase \code{numeric(0)} is returned.}

\examples{
SimulatePoissonProcess(model = 1, beta = c(18,-4,4, 1.2), price = 250,
  from = 0, to = 1)
}
