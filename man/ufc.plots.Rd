\name{ufc.plots}
\alias{ufc.plots}
\docType{data}
\title{ Upper Flat Creek forest cruise plot data }
\description{
  These are a subset of the plot measurement data from the
  Upper Flat Creek unit of the University of Idaho Experimental
  Forest, which was measured in 1991. 
}
\usage{data(ufc.plots)}
\format{
  A data frame with 144 observations on the following 6 variables.
  \describe{
    \item{plot}{plot label}
    \item{north.n}{northerly plot count}
    \item{east.n}{easterly plot count}
    \item{north}{northerly coordinate}
    \item{east}{easterly coordinate}
    \item{vol.m3.ha}{total above-ground merchantable volume, in
      cubic metres per hectare.}
  }
}
\source{
     The data are provided courtesy of Harold Osborne and Ross
     Appelgren of the University of Idaho Experimental Forest. 
}
\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009. An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}
\seealso{ \code{\link{ufc}} }
\examples{
data(ufc.plots)
}
\keyword{datasets}
