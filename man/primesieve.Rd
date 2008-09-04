\name{primesieve}
\alias{primesieve}

\title{ Function to identify all the primes in a vector of positive integers. }
\description{
This function uses the Sieve of Eratosthenes to find all the primes less
than or equal to a given integer.
}

\usage{
primesieve(sieved, unsieved)
}


\arguments{
  \item{sieved}{ Identified primes (empty vector for initialization) }
  \item{unsieved}{ Candidate integers }
}

\details{
The function assumes that \kbd{unsieved} is a vector of positive integers.
}

\value{
Returns a vector of primes sieved (selected) from the input vector.

}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009. An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link{prime}}}

\examples{
primesieve(c(), 2:200)

}

\keyword{ manip }

