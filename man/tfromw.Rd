\name{tfromw}
\alias{tfromw}
\title{Find threshold from mixing weight}
\description{
Given a weight or vector of weights (i.e. prior probabilities that the parameter is nonzero), 
find the corresponding
threshold(s) under the specified prior.
}
\usage{
tfromw(w, prior = "laplace", bayesfac = FALSE, a = 0.5)
}
\arguments{
  \item{w}{prior weight or vector of weights}
  \item{prior}{specification of prior to be used; can be \eqn{\code{"cauchy"}}{"cauchy"} or 
\eqn{\code{"laplace"}}{"laplace"}}
  \item{bayesfac}{specifies whether Bayes factor threshold should be used instead of posterior median threshold}
  \item{a}{scale factor if Laplace prior is used.  Ignored if Cauchy prior is used.}
}
\details{
The Bayes factor method uses a threshold such that the posterior probability of zero is 
exactly half if the data value is equal to the threshold.  
If \code{bayesfac} is set to \eqn{\code{FALSE}}{FALSE} (the default) 
then the threshold is that of the posterior median function given the data value.

The routine carries out a binary search over an appropriate function, using the routine 
\code{\link{vecbinsolv}}.  
The function to be zeroed is \code{\link{beta.laplace}} or \code{\link{beta.cauchy}} 
if the Bayes factor threshold is to be found, and \code{\link{laplace.threshzero}} or 
\code{\link{cauchy.threshzero}} for the posterior median threshold.
}

\value{
The value or vector of values of the estimated threshold(s).
}
\references{See \code{\link{ebayesthresh}} and \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}
\seealso{\code{\link{wfromx}},\code{\link{tfromx}},\code{\link{wandafromx}}}
\examples{
tfromw(c(0.05, 0.1)) 

tfromw(c(0.05, 0.1), prior="cauchy", bayesfac=TRUE)

}
\keyword{nonparametric}
