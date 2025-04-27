\name{fitDistances}
\alias{fitDistances}

\title{ Function to fit a model to seed transect distance/count data. }

\description{
This function uses maximum likelihood to fit a nominated probability
density function to the data of a seedtrap transect holder.
}

\usage{
fitDistances(x, family)
}

\arguments{
  \item{x}{ an object of class transectHolder }
  \item{family}{ the nominated distribution, which must be one of those
    distributions that can be fit by \kbd{fitdistr} of the MASS package. }
}

\value{
  The function returns the parameter estimates for the nominated family.
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link[MASS]{fitdistr}}, \code{\link{trapTransect}}}

\examples{
library(MASS)
s1 <- trapTransect(distances = 1:4, seed.counts = c(4, 3, 2, 0))
allTraps <- transectHolder(s1, family="Weibull")
fitDistances(allTraps, "exponential")
}

\keyword{ manip }

