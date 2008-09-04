\name{treeg}

\alias{treeg}

\docType{data}

\title{ Grand fir tree growth data from northern and central Idaho, USA.}

\description{
A sample of 66 grand fir (\emph{Abies grandis}) trees was selected from
national forests around northern and central Idaho.  The trees were
selected to be dominant in their environment, with no visible evidence
of crown damage, forks, broken tops, etc.  For each tree the habitat
type and the national forest from which it came were recorded.  We have
data from nine national forests and six different habitat types.
}

\usage{data(treeg)}

\format{
  A data frame with 542 observations on the following 6 variables.
  \describe{
    \item{tree.ID}{Tree number.}
    \item{forest}{National forest number.}
    \item{habitat}{Habitat code (see Details).}
    \item{dbh.in}{Bole diameter at 1.37 m, in inches}
    \item{height.ft}{Tree height, in feet.}
    \item{age}{Age at which measurement was taken.}
  }
}

\details{

For each tree the height, diameter and age were measured (age is
measured using tree rings), then the tree was split lengthways, which
allows you to determine the height and diameter of the tree at any
age.  In this instance height and diameter were recorded for the age the
tree was felled and then at ten year periods going back in time.  The
diameter of the tree was measured at a height of 1.37 m (4'6''), which
is called \emph{breast height} in forestry.  The height refers to the
height of the main trunk only.

The habitats corresponding to codes 1 through 5 are: Ts/Pach; Ts/Op;
Th/Pach; AG/Pach and PA/Pach.  These codes refer to the climax tree
species, which is the most shade-tolerant species that can grow on the
site, and the dominant understorey plant, respectively.  Ts refers to
\emph{Thuja plicata} and \emph{Tsuga heterophylla}, Th refers to
just \emph{Thuja plicata}, AG is \emph{Abies grandis}, PA is
\emph{Picea engelmanii} and \emph{Abies lasiocarpa}, Pach is
\emph{Pachistima myrsinites}, and Op is the nasty \emph{Oplopanaz
horridurn}.  Grand fir is considered a major climax species for AG/Pach,
a major seral species for Th/Pach and PA/Pach, and a minor seral species
for Ts/Pach and Ts/Op.  Loosely speaking, a community is \emph{seral}
if there is evidence that at least some of the species are temporary,
and \emph{climax} if the community is self-regenerating (Daubenmire,
1952).

}

\source{ These data were kindly supplied by Dr Al Stage, Principal
Mensurationist (retired), USDA Forest Service Foresct Sciences
Laboratory, Moscow, ID, USA.  }

\references{
  
  R. Daubenmire, 1952. Forest Vegetation of Northern Idaho and Adjacent
  Washington, and Its Bearing on Concepts of Vegetation Classification,
  \emph{Ecological Monographs} \bold{22}, 301--330.

  A. R. Stage, 1963. A mathematical approach to polymorphic site index
  curves for grand fir. \emph{Forest Science} \bold{9}, 167--180.
}

\examples{
data(treeg)
}

\keyword{datasets}
