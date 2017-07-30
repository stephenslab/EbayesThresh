\name{tfromw}
\alias{tfromw}
\alias{laplace.threshzero}
\alias{cauchy.threshzero}
\title{Find threshold from mixing weight}

\description{Given a single value or a vector of weights (i.e. prior
probabilities that the parameter is nonzero) and sampling standard
deviations (sd equals 1 for Cauchy prior), find the corresponding
threshold(s) under the specified prior.}
\usage{
  tfromw(w, s = 1, prior = "laplace", bayesfac = FALSE, a = 0.5)

  laplace.threshzero(x, s = 1, w = 0.5, a = 0.5)

  cauchy.threshzero(z, w)
}
\arguments{
  \item{w}{Prior weight or vector of weights.}
  
  \item{s}{A single value or a vector of standard deviations if the
  Laplace prior is used. If \code{w} is a vector, must have the same
  length as \code{w}. Ignored if Cauchy prior is used.}

  \item{prior}{Specification of prior to be used; can be
    \code{"cauchy"} or \code{"laplace"}.}
  
  \item{bayesfac}{Specifies whether Bayes factor threshold should be
    used instead of posterior median threshold.}
  
  \item{a}{Scale factor if Laplace prior is used. Ignored if Cauchy
    prior is used.}
}
\details{
The Bayes factor method uses a threshold such that the posterior
probability of zero is exactly half if the data value is equal to the
threshold. If \code{bayesfac} is set to \code{FALSE} (the default) then
the threshold is that of the posterior median function given the data
value.

The routine carries out a binary search over each component of an
appropriate vector function, using the routine \code{\link{vecbinsolv}}.

For the posterior median threshold, the function to be zeroed is
\code{laplace.threshzero} or \code{cauchy.threshzero}.

For the Bayes factor threshold, the corresponding functions are
\code{\link{beta.laplace}} or \code{\link{beta.cauchy}}.
}
\value{The value or vector of values of the estimated threshold(s).}
\references{See \code{\link{ebayesthresh}} and
  \url{http://www.bernardsilverman.com}} 
\author{Bernard Silverman}
\seealso{\code{\link{wfromx}}, \code{\link{tfromx}},
  \code{\link{wandafromx}}}
\examples{
tfromw(c(0.05, 0.1), s = 1) 
tfromw(c(0.05, 0.1), prior = "cauchy", bayesfac = TRUE)
}
\keyword{nonparametric}
