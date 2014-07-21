\name{booking_clerkMC}
\alias{booking_clerkMC}

\title{ A function to simulate the harassed booking clerk Markov chain. }

\description{
Simulates the harassed booking clerk Markov chain with given arrival and service rates up to t.end. The state space is (C(t),X(t),Y(t)), where C(t) represents the status of the clerk, X(t) the number of people waiting, and Y(t) the number of calls waiting. C(t) is 0 if clerk is idle, 1 if clerk is serving a person and 2 if clerk is serving a call.
}

\usage{
booking_clerkMC(personArrRate, 
                callArrRate,
                personServRate,
                callServRate,
                t.end)
}

\arguments{
  \item{personArrRate}{ the person arrival rate.  }
  \item{callArrRate}{ the call arrival rate. }
  \item{personServRate}{ the person service rate. }
  \item{callServRate}{ the call service rate. }
  \item{t.end}{ the time of the time period to be simulated i.e. (0,t.end). }
}
\details{
We assume that all given rates are finite and positive.
}

\value{
Returns the matrix (t.hist, state.hist) containing the realisation of the chain.
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}


\examples{
booking_clerkMC(3,6,5,8,1)
}

\keyword{ Markov chain simulation }

