\name{laplace.threshzero}
\alias{laplace.threshzero}
\title{Objective function for Laplace threshold}
\description{
The objective function that has to be zeroed 
to find the posterior median threshold when the Laplace prior is used
}
\usage{
laplace.threshzero(x, w, a = 0.5)
}
\arguments{
  \item{x}{vector of putative threshold values}
  \item{w}{vector of prior probabilities that parameter is nonzero (can be scalar)}
  \item{a}{scale parameter of the Laplace part of the prior}
}
\details{
The function is called by the routine \code{\link{tfromw}} which finds the threshold for particular weight(s) \code{w}.
}
\value{
vector of objective function values
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\note{The routine assumes that \eqn{\code{a} \leq 20}{a <= 20} and truncates \eqn{\code{a}}{a} to 20 if any larger value is supplied.}
\seealso{\code{\link{cauchy.threshzero}},
\code{\link{tfromw}}
}
\keyword{internal}
