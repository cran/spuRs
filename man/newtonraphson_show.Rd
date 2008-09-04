\name{newtonraphson_show}
\alias{newtonraphson_show}

\title{ A function of the Newton-Raphson algorithm, plotting the path. }

\description{
Applies the Newton-Raphson algorithm to find x such that ftn(x)[1] == 0,
and plots the trace of the estimate.
}

\usage{
newtonraphson_show(ftn, x0, xmin = x0 - 1, xmax = x0 + 1)
}

\arguments{
  \item{ftn}{ the function. }
  \item{x0}{ the initial guess of the fixed point. }
  \item{xmin}{ lower limit for plotting. }
  \item{xmax}{ upper limit for plotting. }
}

\value{
Returns the value of x at which ftn(x)[1] == 0.  If the function fails to
converge within \kbd{max.iter} iterations, returns \kbd{NULL}.
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{ \code{\link{newtonraphson}} }

\keyword{ optimize }

