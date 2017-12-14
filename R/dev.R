# contains some functions useful for testing/development

# compute posterior mean under Laplace prior, from equation 2.8 in thesis
# conditional on non-zero
pmeancond = function(x,s,a){
  l=lambda(x,s,a)
  l *(x-s^2*a) + (1-l)*(x+s^2*a)
}

pmeancond_etrunc = function(x,s,a){
  l=lambda(x,s,a)
  l * truncnorm::etruncnorm(0,Inf,x-s^2*a,s) + (1-l)*truncnorm::etruncnorm(-Inf,0,x+s^2*a,s)
  
#  l * ashr:::my_etruncnorm(0,Inf,x-s^2*a,s) + (1-l)*ashr:::my_etruncnorm(-Inf,0,x+s^2*a,s)
}


