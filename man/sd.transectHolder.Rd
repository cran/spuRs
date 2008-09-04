\name{sd.transectHolder}

\alias{sd.transectHolder}

\title{ Function to compute the sd dispersal distance along a transect
  of seed traps.}

\description{ This function computes the standard deviation of the
dispersal distances along a transect of seed traps.  }

\usage{
sd.transectHolder(transectHolder)
}

\arguments{
  \item{transectHolder}{an object representing a transect of seed traps. }
}

\value{ The standard deviation of the seed dispersal distances is returned.  }

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link{transectHolder}} }

\examples{

transect.1 <- trapTransect(distances = 1:4,
                        seed.counts = c(4, 3, 2, 0))
transect.2 <- trapTransect(distances = 1:3,
                        seed.counts = c(3, 2, 1))
transect.3 <- trapTransect(distances=(1:5)/2,
                        seed.counts = c(3, 4, 2, 3, 1))
allTraps <- transectHolder(transect.1, transect.2, transect.3,
                           family="Weibull")

mean(allTraps)

sd.transectHolder(allTraps)

}

\keyword{ univar }

