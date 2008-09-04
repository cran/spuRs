\name{print.transectHolder}

\alias{print.transectHolder}

\title{ Function to print a transectHolder object usefullly.}

\description{ This function prints the details of a transectHolder object.
}

\usage{
## S3 method for class transectHolder 
\method{print}{transectHolder}(x, \dots)
}

\arguments{
  \item{x}{An object representing a transect of seed traps. }
  \item{\dots}{further arguments passed to or from other methods.}  
}

\details{ The print function simply uses \kbd{str} on the transectHolder
object.  }

\value{
This function is called for its side-effect, which is to print the
object informatively.
}

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
allTraps

}

\keyword{ print }

