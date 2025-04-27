\name{vol.m3}
\alias{vol.m3}
\title{ Function to compute the volume of a tree bole assuming a
  particular shape. }
\description{
 This function computes the volume of a tree bole given its basal
  diameter and length, assuming that the bole is a frustum of a
  geometric solid.
}
\usage{
vol.m3(dbh.cm, height.m, multiplier = 0.5)
}
\arguments{
  \item{dbh.cm}{ basal diameter in cm. }
  \item{height.m}{ height in m. }
  \item{multiplier}{ shape, expressed as a multiplier. }
}
\details{
  Commonly-used shapes are:
  \describe{
    \item{1/3 }{conoid}
    \item{1/2 }{second-degree parabaloid}
    \item{1 }{cylinder}
    }
}
\value{
  The volume is returned, in units of cubic metres.
}

\examples{
vol.m3(30, 30)
vol.m3(30, 30, 1)
}

\keyword{ manip }
