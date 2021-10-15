library(plyr)
library(metafor)
library(doParallel)
library(RhpcBLASctl)

blas_set_num_threads(1)

registerDoParallel(cores=24)

iteration = 3000

sd.e = seq(from=0.2, to=2, by=0.2)
mu.log.sigma = seq(from=-2, to=0, by=1)
var.log.sigma = seq(from=0.5, to=2, by=0.1)
mu.rep = seq(from=5, to=25, by=5)
no.study = 50


parm = expand.grid(sd.e=sd.e, mu.log.sigma=mu.log.sigma, var.log.sigma=var.log.sigma, mu.rep=mu.rep)
parm$sd.rep = -6.012 + 1.748*parm$mu.rep
parm$size.rep = parm$mu.rep^2/(parm$sd.rep^2-parm$mu.rep)
scenario = dim(parm)[1]

parm.est = array(dim=c(iteration, 53))

set.seed(10)


result = foreach(k = 1:2400) %dopar% {
	library(metafor)
	library(plyr)
	for(i in 1:iteration){
		effect.size = array(dim=c(no.study,3))
		mu = 10
		alpha = 1
		no.rep = 2+rnbinom(n=no.study, size=parm$size.rep[k], mu=parm$mu.rep[k])
		sigma = exp(rnorm(n=no.study, mean=parm$mu.log.sigma[k], sd=sqrt(parm$var.log.sigma[k])))
		for(m in 1:no.study){
			log.e = rnorm(n=1, mean=0, sd=parm$sd.e[k])
			alpha.m = alpha * exp(log.e)
			yc = mu * rlnorm(n=no.rep[m],meanlog=-log(sigma[m]^2+1)/2, sdlog=sqrt(log(sigma[m]^2+1)))
			yt = mu * alpha.m * rlnorm(n=no.rep[m], meanlog=-log(sigma[m]^2+1)/2, sdlog=sqrt(log(sigma[m]^2+1)))
			effect.size[m,1] = log(mean(yt)/mean(yc))
			effect.size[m,2] = var(yt)/no.rep[m]/mean(yt)^2 + var(yc)/no.rep[m]/mean(yc)^2
			
			log.RR = array()
			for(n in 1:5000){
				yc = mu * rlnorm(n=no.rep[m],meanlog=-log(sigma[m]^2+1)/2, sdlog=sqrt(log(sigma[m]^2+1)))
				yt = mu * alpha.m * rlnorm(n=no.rep[m], meanlog=-log(sigma[m]^2+1)/2, sdlog=sqrt(log(sigma[m]^2+1)))
				log.RR[n] = log(mean(yt)/mean(yc))
			}
			effect.size[m,3] = var(log.RR)
		}
		
		effect.size = data.frame(effect.size)
		names(effect.size) = c("ES","ES.var","ES.var.true")
		
		mod1 = try(rma(yi=ES, vi=ES.var, test="knha", data=effect.size), silent=T)
		mod2 = try(rma(yi=ES, vi=ES.var.true, test="knha", data=effect.size), silent=T)
		parm.est[i,1] = as.numeric(try(mod1$b, silent=T))
		parm.est[i,2] = as.numeric(try(mod2$b, silent=T))
		mod3 = try(lm(ES~1, data=effect.size), silent=T)
		parm.est[i,3] = as.numeric(try(coef(mod3), silent=T))

						
		#Record true and estimated within-study error variance#
		parm.est[i,4:53] = effect.size[,3]
		
	}
	parm.est
}



save.image(file="VarSigma_I2_RE.RData")
