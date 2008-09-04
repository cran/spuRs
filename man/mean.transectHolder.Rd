\name{mean.transectHolder}

\alias{mean.transectHolder}

\title{ Function to compute the mean dispersal distance along a transect
  of seed traps.}

\description{ This function computes the mean dispersal distance along a
transect of seed traps.  }

\usage{
## S3 method for class transectHolder 
\method{mean}{transectHolder}(x, \dots)
}

\arguments{
  \item{x}{an object representing a transect of seed traps. }
  \item{\dots}{further arguments passed to or from other methods.}
}

\value{ The mean seed dispersal distance is returned.  }

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

}

\keyword{ univar }

