\name{CMCSimulation}
\alias{CMCSimulation}

\title{ A function to simulate a continuous time Markov chain. }

\description{
This function simulates a continuous time finite state space Markov chain with known rate matrix Q, state space 0,1,..,n and initial state i for the time period (0,T).  If plotflag is TRUE it also produces a plot.
}

\usage{
CMCSimulation(Q,i,Tend,plotflag = FALSE)
}

\arguments{
  \item{Q}{ the rate matrix.  }
  \item{i}{ the initial state. }
  \item{Tend}{ the end of the simulation period (0,T). }
  \item{plotflag}{flag indicating if plot needed}
}
\details{
We assume that \kbd{Q} is well defined rate matrix.
}

\value{
Returns the matrix (statehist,timehist) containing the realisation of the chain for the specified period.  The function also produces a plot of the realisation. \
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{ \code{\link{MCSimulation}}}

\examples{
Q <- matrix(c(-24,24,0, 2,-14,12, 0,4,-4), 
            nrow = 3, ncol = 3, byrow = TRUE)
CMCSimulation(Q,0,1)
}

\keyword{ Markov chain simulation }

