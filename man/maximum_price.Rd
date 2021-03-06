% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maximum_price.R
\name{maximum_price}
\alias{maximum_price}
\title{Maximum acceptable price for passangers}
\usage{
maximum_price(beta, price, time)
}
\arguments{
\item{beta}{Parameters of the Poisson process}

\item{price}{Actual price of the tickets.}

\item{time}{Vector of times of purchases for individual passengers}
}
\description{
Simulate maximum price that a passanger is available to pay for the ticket.
The distribution of maximum price is conditional on current price - only passengers that would buy the ticket for \code{price}.
}

