\name{fixedpoint_show}
\alias{fixedpoint_show}

\title{ A function of the fixed point algorithm. }

\description{
Applies the fixed point algorithm to find x such that ftn(x) == x, and
plots the process.
}

\usage{
fixedpoint_show(ftn, x0, xmin = x0 - 1, xmax = x0 + 1)
}

\arguments{
  \item{ftn}{ the function. }
  \item{x0}{ is the initial guess at the fixed point. }
  \item{xmin}{ ~~Describe \kbd{xmin} here~~ }
  \item{xmax}{ ~~Describe \kbd{xmax} here~~ }
}

\details{
We assume that \kbd{ftn} is a function of a single variable.
}

\value{
Returns the value of x at which ftn(x) == x.  If the function fails to
converge within \kbd{max.iter} iterations, returns \kbd{NULL}.
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link{fixedpoint}}}

\keyword{ optimize }

