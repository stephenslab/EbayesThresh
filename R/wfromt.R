"wfromt" <-
function(tt, s, prior = "laplace", a = 0.5)
{
# find the weight that has posterior median threshold tt, 
#
	pr <- substring(prior, 1, 1)
	if(pr == "l"){
	  tma <- tt/s - s*a
	  wi <- 1/abs(tma)
	  wi[tma > -35] <- pnorm(tma[tma > -35])/dnorm(tma[tma > -35])
		wi <- a * s * wi - beta.laplace(tt, s, a)
	}
	if(pr == "c") {
		dnz <- dnorm(tt)
		wi <- 1 + (pnorm(tt) - tt * dnz - 1/2)/(sqrt(pi/2) * dnz * tt^2
			)
		wi[!is.finite(wi)] <- 1
	}
	return(1/wi)
}