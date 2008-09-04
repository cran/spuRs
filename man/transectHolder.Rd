\name{transectHolder}
\alias{transectHolder}

\title{ Function to construct an object representing a collection of
  trapTransect objects.
}

\description{
   This function constructs a transectHolder object given a collection
   of \kbd{trapTransect} objects and a nominated probability density
   function to fit to the seed count profile.
}

\usage{
transectHolder(..., family = "exponential")
}

\arguments{
  \item{\dots}{ one or more \kbd{trapTransect} objects }
  \item{family}{ the probability density function to fit to the distance
  count profiles. }
}

\details{
  This function is a constructor.

  The nominated distribution, which must be one of those distributions
    that can be fit by \kbd{fitdistr} of the MASS package.  }

\value{
  A transectHolder object, which is a list comprising
  \item{transects }{a list one or more trapTransect objects,}
  \item{family }{the name of the distribution to which the transect data
  has been fit,}
  \item{parameters }{the estimated parameters for that distribution,}
  \item{rng }{the corresponding random number generator for simulations.}
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009. An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link{trapTransect}} }

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
}

\keyword{ data }
