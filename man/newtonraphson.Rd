\name{newtonraphson}
\alias{newtonraphson}

\title{ A function of the Newton-Raphson algorithm. }

\description{
Applies the Newton-Raphson algorithm to find x such that ftn(x)[1] == 0.
}

\usage{
newtonraphson(ftn, x0, tol = 1e-09, max.iter = 100)
}

\arguments{
  \item{ftn}{ the function.  }
  \item{x0}{ is the initial guess at the fixed point. }
  \item{tol}{ distance of successive iterations at which algorithm terminates. }
  \item{max.iter}{ maximum number of iterations. }
}

\value{
Returns the value of x at which ftn(x)[1] == 0.  If the function fails to
converge within \kbd{max.iter} iterations, returns \kbd{NULL}.
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{ \code{\link{fixedpoint}}, \code{\link{bisection}}}

\examples{
ftn4 <- function(x) {
  # returns function value and its derivative at x
  fx <- log(x) - exp(-x)
  dfx <- 1/x + exp(-x)
  return(c(fx, dfx))
}
newtonraphson(ftn4, 2, 1e-6)
}

\keyword{ optimize }

