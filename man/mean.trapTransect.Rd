\name{mean.trapTransect}

\alias{mean.trapTransect}

\title{ Function to compute the mean dispersal distance along a transect
  of seed traps.}

\description{ This function computes the mean dispersal distance along a
transect of seed traps.  }

\usage{
## S3 method for class trapTransect 
\method{mean}{trapTransect}(x, \dots)
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

\seealso{\code{\link{trapTransect}} }

\examples{

s1 <- trapTransect(distances = 1:4, seed.count = c(4, 3, 2, 0))
mean(s1)

}

\keyword{ univar }

