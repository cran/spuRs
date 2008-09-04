\name{bisection}
\alias{bisection}

\title{ A function of the bisection algorithm. }

\description{
Applies the bisection algorithm to find x such that ftn(x) == x.
}

\usage{
bisection(ftn, x.l, x.r, tol = 1e-09)
}

\arguments{
  \item{ftn}{ the function.  }
  \item{x.l}{ is the lower starting point. }
  \item{x.r}{ is the upper starting point. }
  \item{tol}{ distance of successive iterations at which algorithm
    terminates. }
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

\seealso{ \code{\link{newtonraphson}}, \code{\link{fixedpoint}}}

\examples{
ftn5 <- function(x) return(log(x)-exp(-x))
bisection(ftn5, 1, 2, tol = 1e-6)
}

\keyword{ optimize }

