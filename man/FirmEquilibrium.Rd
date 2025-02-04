% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FirmEquilibrium.R.R
\name{FirmEquilibrium}
\alias{FirmEquilibrium}
\title{Analyze Firm Equilibrium}
\usage{
FirmEquilibrium(TR, TC)
}
\arguments{
\item{TR}{An expression for the total revenue function as a function of quantity (Q).}

\item{TC}{An expression for the total cost function as a function of quantity (Q).}
}
\value{
A list containing equilibrium quantity, revenue, cost, and other related metrics.
}
\description{
This function calculates the equilibrium quantity, revenue, cost, and other metrics for a firm based on given total revenue (TR) and total cost (TC) functions.
}
\examples{
library(Deriv)
library(ggplot2)
library(ggthemes)
TR <- expression(100 * Q - 8 * Q^2)
TC <- expression(50 + 20 * Q)
results <- FirmEquilibrium(TR, TC)
print(results)
}
