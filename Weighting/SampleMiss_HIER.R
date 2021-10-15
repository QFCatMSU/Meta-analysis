library(plyr)
library(metafor)
library(nlme)
library(RhpcBLASctl)

blas_set_num_threads(1)

iteration = 5000

#Levels of experimental factors#
sd.e = seq(from=0.2, to=1, by=0.2)
missing = c(0:5)

#Parameters for normal distribution to draw the log of among-replicate error term#
mu.log.sigma = 0
var.log.sigma = 2

#Negative binomial distribution parameters for number of replicates#
mu.rep = 25
sd.rep = -6.012 + 1.748*mu.rep
size.rep = mu.rep^2/(sd.rep^2-mu.rep)

no.study = 50

parm = expand.grid(sd.e=sd.e, missing=missing)
scenario = dim(parm)[1]

parm.est = array(dim=c(scenario, iteration, 52))


set.seed(10)
t1 = proc.time()

for(k in 1:scenario){
	for(i in 1:iteration){
		effect.size = array(dim=c(no.study,2))
		mu = 10
		alpha = 1
		no.rep = 2 + rnbinom(n=no.study, size=size.rep, mu=mu.rep)
		sigma = exp(rnorm(n=no.study, mean=mu.log.sigma, sd=sqrt(var.log.sigma)))
		group.eff = rep(rnorm(n=10, mean=0, sd=parm$sd.e[k]/sqrt(2)), each=5)
		for(m in 1:no.study){
			log.e = rnorm(n=1, mean=0, sd=parm$sd.e[k]/sqrt(2))
			alpha.m = alpha * exp(log.e) * exp(group.eff[m])
			yc = mu * rlnorm(n=no.rep[m], meanlog=-log(sigma[m]^2+1)/2, sdlog=sqrt(log(sigma[m]^2+1)))
			yt = mu * alpha.m * rlnorm(n=no.rep[m], meanlog=-log(sigma[m]^2+1)/2, sdlog=sqrt(log(sigma[m]^2+1)))
			effect.size[m,1] = log(mean(yt)/mean(yc))
			effect.size[m,2] = var(yt)/no.rep[m]/mean(yt)^2 + var(yc)/no.rep[m]/mean(yc)^2
		}
		effect.size = data.frame(effect.size)
		names(effect.size) = c("ES","ES.var")
		effect.size$group = rep(c(1:10), each=5)
		effect.size$study = 1:no.study
		
		
		if(parm$missing[k]==0){
			effect.size.reduced = effect.size
		}
		if(parm$missing[k]!=0){
			group.miss = sample(x=1:10, size=parm$missing[k])
			effect.size.reduced = effect.size[-which(effect.size$group %in% group.miss),]
		}
		
		mod1 = try(rma.mv(yi=ES, V=ES.var, random=list(~1|group, ~1|study), data=effect.size.reduced), silent=T)
		parm.est[k,i,1] = as.numeric(try(mod1$b, silent=T))
		
		mod2 = try(lme(ES~1, random=~1|group, data=effect.size), silent=T)
		parm.est[k,i,2] = as.numeric(try(fixef(mod2), silent=T))
		
		parm.est[k,i,3:52] = effect.size$ES.var
		
	}
	
}

t2 = proc.time()
t2 - t1

save.image(file="SampleMiss_HIER.RData")