#Function CorrCVine generates a random correlation matrix using C-Vine#
#Referebce: Lewandowski et al (2009) Generating random correlation matrices based on vines and extended onion method#

CorrCVine = function(d, r.low, r.up){ 
	P = matrix(data=0, nrow=d, ncol=d)		#Matrix P stores partial correlation#
	S = diag(x=1, nrow=d)					#Matrix S stores correlation#
  	if(d==1){
  		return(S)
  	}
  	if(d==2){ 
  		rho = runif(1,r.low,r.up)
    	S = matrix(c(1,rho,rho,1),2,2)
    	return(S) 
  	}
  	for(i in 2:d){
  		P[1,i] = runif(1, r.low, r.up)
  		S[1,i] = P[1,i]
  		S[i,1] = P[i,1]
  	}
  	for(k in 2:(d-1)){
  		for(i in (k+1):d){
  			P[k,i] = runif(1, r.low, r.up)
  			p = P[k,i]
  			for(l in (k-1):1){
  				p = p*sqrt((1-P[l,i]^2)*(1-P[l,k]^2)) + P[l,i]*P[l,k]
  			}
  			S[k,i] = p
  			S[i,k] = p
  		}
  	}
  	return(S)
}

