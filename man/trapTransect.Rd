\name{trapTransect}

\alias{trapTransect}

\title{ Function to construct an object representing a transect of seedtraps. }

\description{ This function constructs a trapTransect object given a
vector of trap distances from the parent plant, a vector of trap seed
counts corresponding to the trap distances, and a single trap area.  }

\usage{ trapTransect(distances, seed.counts, trap.area = 0.0001) }

\arguments{
  \item{distances}{A vector of trap distances from the parent plant.  }
  \item{seed.counts}{A vector of seed counts in each trap.}
  \item{trap.area}{The surface area of each trap.}
}

\details{
This function is a constructor.
}

\value{
A trapTransect object, which is a list comprising three objects:
  \item{distances }{A vector of trap distances from the parent plant.}
  \item{seed.counts }{A vector of seed counts in each trap.}
  \item{trap.area }{The surface area of each trap.}
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009. An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link{mean.trapTransect}},  \code{\link{print.trapTransect}}}

\examples{
s1 <- trapTransect(distances = 1:4, seed.counts = c(4, 3, 2, 0))
s1
mean(s1)
}

\keyword{ data }
