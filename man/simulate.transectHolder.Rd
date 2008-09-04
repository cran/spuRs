\name{simulate.transectHolder}
\alias{simulate.transectHolder}

\title{Function to simulate a modelled seed rain from a transectHolder }

\description{
  This function simulates a two-dimensional seed rain
  according to the model stored in a transectHolder object.  The angle
  of the seed location from the parent plant is uniformly distributed on
  [0, 2 pi).
}

\usage{
## S3 method for class transectHolder 
\method{simulate}{transectHolder}(object, nsim=1, seed=NULL, \dots)
}

\arguments{
  \item{object}{ the transectHolder object for simulation }
  \item{nsim}{ the number of seeds to simulate. }
  \item{seed}{ if not NULL, set the seed to this value before simulation.}
  \item{\dots}{ additional optional arguments (ignored here).}
}

\value{
  A dataframe with \code{n} rows with the following components:
  \item{distances }{seed distances to parent plant}
  \item{angles }{seed angles to parent plant, in radians}
  \item{x }{x-location of seed}
  \item{y }{y-location of seed}
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009. An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link{transectHolder}}}

\examples{
transect.1 <- trapTransect(distances = 1:4,
                        seed.counts = c(4, 3, 2, 0))
transect.2 <- trapTransect(distances = 1:3,
                        seed.counts = c(3, 2, 1))
transect.3 <- trapTransect(distances=(1:5)/2,
                        seed.counts = c(3, 4, 2, 3, 1))
allTraps <- transectHolder(transect.1, transect.2, transect.3,
                           family="Weibull")
allTraps
simulate(allTraps, nsim=5, seed=123)
}

\keyword{ distribution }

