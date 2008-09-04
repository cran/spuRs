\name{prime}
\alias{prime}

\title{ Function to assess whether or not an integer is prime. }
\description{
  An inefficient, brute-force algorithm to assess whether or not an integer is prime. 
}
\usage{
prime(n)
}

\arguments{
  \item{n}{ The integer. }
}
\details{
The function assumes that \code{n} is a positive integer.
}

\value{
The function returns a logical object that is TRUE if the integer is prime.
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009. An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link{primesieve}}}

\examples{
prime(10)
prime(7)
}
\keyword{ manip }

