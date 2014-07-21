\name{MCEstimation}
\alias{MCEstimation}

\title{ A function to estimate the transition matrix for a discrete time Markov chain. }

\description{
This function estimates the transition matrix for a discrete time Markov chain with state space 0,1,..,n given a realisation. The chain has n+1 states.
}

\usage{
MCEstimation(statehist,n)
}

\arguments{
  \item{statehist}{ the realisation of the chain.  }
  \item{n}{ the highest numbered state. }
}
\details{
We assume that the state space is 0,1,2...,n.  n is assumed known as it cannot be reliably infered from the realisation.
}

\value{
Returns the empirical transition matrix obtained by calculating the observed frequencies of actual transitions in the realisation.\
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}

\seealso{ \code{\link{MCSimulation}}}

\examples{
P <- matrix(c(0.5,0.5,0,0,0.7,0.1,0.2,0,0,0.1,0.1,0.8,0,0,0.7,0.3),
            nrow = 4, ncol = 4, byrow = TRUE)
statehist<-MCSimulation(P, 0, 3000)
MCEstimation(statehist, 3)
}

\keyword{ Markov chain estimation }

