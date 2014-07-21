\name{MCSimulation}
\alias{MCSimulation}

\title{ A function to simulate a discrete time Markov chain. }

\description{
This function simulates a discrete time Markov chain with transition matrix P, state space 0,1,..,n and 
and initial state i for nsteps transitions.
}

\usage{
MCSimulation(P,i,nsteps)
}

\arguments{
  \item{P}{ the transition matrix.  }
  \item{i}{ the initial state. }
  \item{nsteps}{ the number of transitions to be simulated. }
}
\details{
We assume that \kbd{P} is well defined transition matrix with rows summing to 1.
}

\value{
Returns the vector statehist containing the realisation of the chain for nsteps transitions.\
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{ \code{\link{MCEstimation}}, \code{\link{CMCSimulation}}}

\examples{
P <- matrix(c(0.5,0.5,0,0,0.7,0.1,0.2,0,0,0.1,0.1,0.8,0,0,0.7,0.3),
            nrow = 4, ncol = 4, byrow = TRUE)
MCSimulation(P, 0, 250)
}

\keyword{ Markov chain simulation }

