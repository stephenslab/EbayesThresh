\name{ebayesthresh.wavelet}
\alias{ebayesthresh.wavelet}
\alias{ebayesthresh.wavelet.dwt}
\alias{ebayesthresh.wavelet.wd}
\alias{ebayesthresh.wavelet.splus}
\title{
Empirical Bayes thresholding on the levels of a wavelet transform
}
\description{
Apply an Empirical Bayes thresholding approach level by level to the detail coefficients in a wavelet transform.
}
\usage{
ebayesthresh.wavelet(xtr, vscale = "independent", smooth.levels = Inf, 
      prior = "laplace", a = 0.5, bayesfac = FALSE, 
      threshrule = "median")
}
\arguments{
  \item{xtr}{%
	The wavelet transform of a vector of data. 
	The transform is obtained using one of the wavelet transform routines in \R or in S+WAVELETS. 
	Any choice of wavelet, boundary condition, etc provided by these routines can be used. 
	}
  \item{vscale}{%
	Controls the scale used at different levels of the transform. 
	If \code{vscale} is a scalar quantity, then it will be assumed that the wavelet coefficients 
	at every level have this standard deviation. 
	If \code{vscale = "independent"}, the standard deviation will be estimated from the highest level 
	of the wavelet transform and will then be used for all levels processed. 
	If \code{vscale="level"}, then the standard deviation will be estimated separately for each level processed, 
	allowing standard deviation that is level-dependent.
}
  \item{smooth.levels}{%
	the number of levels to be processed, if less than the number of 
	levels of detail calculated by the wavelet transform.
}
  \item{prior}{%
	specification of prior to be used for the coefficients at each level,
	conditional on their mean being nonzero; 
	can be \code{cauchy} or \code{laplace}
	} 
  \item{a}{%
	scale factor if Laplace prior is used.  
	Ignored if Cauchy prior is used. 
	If, on entry, \code{a=NA} and \code{prior="laplace"}, then the scale 
	parameter will also be estimated at each level by marginal maximum likelihood. 
	If \code{a} is not specified then the default value 0.5 will be used.
	}
  \item{bayesfac}{%
	if \code{bayesfac=TRUE}, then whenever a threshold is explicitly 
	calculated, the Bayes factor threshold will be used
	}
  \item{threshrule}{%
	specifies the thresholding rule to be applied to the coefficients.
	Possible values are \code{median} (use the posterior median); 
	\code{mean} (use the posterior mean); 
	\code{hard} (carry out hard thresholding); 
	\code{soft} (carry out soft thresholding); 
}
}
\details{
The routine \code{ebayesthresh.wavelet} can process a wavelet transform obtained using the routine 
\code{wd} in the WaveThresh \R package, the routines \code{dwt} or \code{modwt} in the waveslim \R package,
or one of the routines (either dwt or nd.dwt) in S+WAVELETS. 
Note that the wavelet transform must be calculated before the routine ebayesthresh.wavelet is called; the choice of wavelet,
boundary conditions, decimated vs nondecimated wavelet, and so on, are made when the wavelet transform is calculated.
Apart from some housekeeping to estimate the standard deviation if necessary, and to determine the number of levels 
to be processed, the main part of the routine is a call, for each level, to the smoothing routine \code{\link{ebayesthresh}}.
The basic notion of processing each level of detail coefficients is easily transferred to transforms 
constructed using other wavelet software. 
Similarly, it is straightforward to modify the routine to give 
other details of the wavelet transform, if necessary using the option 
\code{verbose = TRUE} in the calls to \code{\link{ebayesthresh}}.

The main routine \code{ebayesthresh.wavelet} calls the relevant one of the routines \code{ebayesthresh.wavelet.wd} (for a transform obtained from WaveThresh), \code{ebayesthresh.wavelet.dwt} (for transforms obtained from either \code{dwt} or \code{modwt} in waveslim) or \code{ebayesthresh.wavelet.splus} (for transforms obtained from S+WAVELETS.  
}
\value{
The wavelet transform (in the same format as that supplied to the routine) of the values of the estimated regression function underlying the original data.
}
\references{
Johnstone, I. M. and Silverman, B. W. (2005)  
Empirical Bayes selection of wavelet thresholds.
\emph{Annals of Statistics}, \bold{33?}, to appear.

See also the other references given for \code{\link{ebayesthresh}} and at \url{http://www.bernardsilverman.com}}
\author{Bernard Silverman}}
\seealso{\code{\link{ebayesthresh}}}
\examples{
}
\keyword{nonparametric}% at least one, from doc/KEYWORDS
