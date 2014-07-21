\name{RK4adapt}
\alias{RK4adapt}

\title{ A function which uses the Fourth order Runge-Kutta method with adaptive step size to solve a system of ODE's. }

\description{
This function simulates a discrete time Markov chain with transition matrix P, state space 0,1,..,n and 
and initial state i for nsteps transitions.
}

\usage{
RK4adapt(dydt, t0, y0, t1, h0 = 1, tol = 1e-10, ...)
}

\arguments{
  \item{dydt}{ a function giving the gradient of y(t).}
  \item{t0}{ initial value of t.}
  \item{y0}{initial value of y(t).}
  \item{t1}{system solved up to time t1.}
  \item{h0}{initial step size}
  \item{tol}{ tolerance for adapting step size.}
  \item{...}{pass arguments to function dydt.}
}
\details{
We assume that \kbd{P} is well defined transition matrix with rows summing to 1.
}

\value{
Returns a list with elements t, a vector giving times, and y, a matrix whose rows give the solution at successive times.
}

\references{
  Jones, O.D., R. Maillardet, and A.P. Robinson.  2009.  An Introduction
  to Scientific Programming and Simulation, Using R.  Chapman And Hall/CRC.
}


\examples{
LV <- function(t=NULL, y, a, b, g, e, K=Inf)
  c(a*y[1]*(1 - y[1]/K) - b*y[1]*y[2], g*b*y[1]*y[2] - e*y[2])

xy <- RK4adapt(LV, 0, c(100, 50), 200, 1, tol=1e-3, 
               a=0.05, K=Inf, b=0.0002, g=0.8, e=0.03)

par(mfrow = c(2,1))
plot(xy$y[,1], xy$y[,2], type='p', 
     xlab='prey', ylab='pred', main='RK4, adaptive h')
plot(xy$t, xy$y[,1], type='p', xlab='time', 
     ylab='prey circles pred triangles', main='RK4, adaptive h')
points(xy$t, xy$y[,2], pch=2)
par(mfrow=c(1,1))
}

\keyword{Numerical solution of system of ODE's' }

