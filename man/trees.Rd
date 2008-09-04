\name{trees}

\alias{trees}

\docType{data}

\title{ von Guttenberg Norway spruce tree measurement data }

\description{
These are a subset of the von Guttenberg data, a set of measurements on
Norway spruce (\emph{Picea abies} [L.] Karst) in several different
locations and site categories.
}

\usage{data(trees)}

\format{
  A data frame with 1200 observations on the following 3 variables.
  \describe{
    \item{ID}{A factor identifying the tree by location, site,
      and tree number.}
    \item{Age}{The age at which the tree was measured.}
    \item{Vol}{The bole volume of the tree, in cubic dm.}
  }
}

\source{
These data were kindly provided by Professor Boris Zeide, University of
Arkanasa, Monticello, AK, USA, and are further documented in Zeide (1993).
}

\references{
  A.R. von Guttenberg.  1915.  Growth and yield of spruce in Hochgebirge.
  Franz Deuticke, Wien. (In German)

  B. Zeide, 1993.  Analysis of growth equations.  \emph{Forest Science}
  \bold{39} 594--616.
  
}

\examples{
data(trees)
}

\keyword{datasets}
