\name{vecbinsolv}
\alias{vecbinsolv}
\title{Solve systems of nonlinear equations based on a monotonic function}
\description{
Solve a nonlinear equation or a vector of nonlinear equations 
based on an increasing function in a specified interval
}
\usage{
vecbinsolv(zf, fun, tlo, thi, nits = 30, \dots )
}
\arguments{
  \item{zf}{the right hand side of the equation(s) to be solved}
  \item{fun}{an increasing function of a scalar argument, or a vector of such functions}
  \item{tlo}{lower limit of interval over which the solution is sought}
  \item{thi}{upper limit of interval over which the solution is sought}
  \item{nits}{number of binary subdivisions carried out}
  \item{\dots}{additional arguments to the function \code{fun}}
}
\details{
If \code{fun} is a scalar monotone function,
the routine finds a vector
\code{t} the same length as \code{zf} such that, 
component-wise, \eqn{\code{fun(t) = zf}}{fun(t) = zf}, 
where this is possible within the interval \eqn{\code{(tlo,thi)}}{(tlo, thi)}. 
The relevant value returned is the nearer extreme to the solution if there 
is no solution in the specified range for any particular component of \code{zf}.
The routine will also work if \code{fun} is a vector of monotone functions, 
allowing different functions to be considered for different components.  
The interval over which the search is conducted has to be the same for each 
component.

The accuracy of the solution is determined by the number of binary subdivisions;
if \eqn{\code{nits=30}}{nits=30} then the solution(s) will be accurate to about 9 orders of 
magnitude less than the length of the original interval \eqn{\code{(tlo, thi)}}{(tlo,thi)}.  
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\keyword{internal}% at least one, from doc/KEYWORDS
