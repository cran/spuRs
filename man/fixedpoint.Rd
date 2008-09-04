\name{fixedpoint}
\alias{fixedpoint}

\title{ A function of the fixed point algorithm. }

\description{
Applies the fixed point algorithm to find x such that ftn(x) == x.
}

\usage{
fixedpoint(ftn, x0, tol = 1e-09, max.iter = 100)
}

\arguments{
  \item{ftn}{ the function.  }
  \item{x0}{ is the initial guess at the fixed point. }
  \item{tol}{ distance of successive iterations at which algorithm terminates. }
  \item{max.iter}{ maximum number of iterations. }
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

\seealso{ \code{\link{newtonraphson}}, \code{\link{bisection}}}

\examples{
ftn1 <- function(x) return(exp(exp(-x)))
fixedpoint(ftn1, 2, tol = 1e-6)
}

\keyword{ optimize }

