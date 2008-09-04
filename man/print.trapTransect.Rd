\name{print.trapTransect}

\alias{print.trapTransect}

\title{ Function to print a trapTransect object usefullly.}

\description{ This function prints the details of a trapTransect object.
}

\usage{
## S3 method for class trapTransect 
\method{print}{trapTransect}(x, \dots)
}

\arguments{
  \item{x}{An object representing a transect of seed traps. }
  \item{\dots}{further arguments passed to or from other methods.}  
}

\details{ The print function simply uses \kbd{str} on the trapTransect
object.  }

\value{
This function is called for its side-effect, which is to print the
object informatively.
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{\code{\link{trapTransect}} }

\examples{

s1 <- trapTransect(distances = 1:4, seed.count = c(4, 3, 2, 0))
s1

}

\keyword{ print }

