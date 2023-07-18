library(tidyverse)
library(metafor)
library(doParallel)
library(RhpcBLASctl)

#Set up parallel computing and single core use for numeric routine#
blas_set_num_threads(1)
registerDoParallel(cores = 102)

#Number of iterations in the simulation#
iteration <- 5000

#Levels of experimental factors#
sd.e <- c(0.006, 0.008, 0.01, 0.03, 0.05, 0.07, 0.1, 0.3, 0.5)
mu.log.sigma <- seq(from = -2, to = 0, by = 1)
var.log.sigma <- seq(from = 0.4, to = 1, by = 0.1)
mu.rep <- seq(from = 5, to = 25, by = 10)
missing <- seq(from = 0, to = 25, by = 5)

parm = expand.grid(sd.e = sd.e, mu.log.sigma = mu.log.sigma, 
                   var.log.sigma = var.log.sigma, mu.rep = mu.rep, missing = missing)
parm$sd.rep <- -6.012 + 1.748 * parm$mu.rep
parm$size.rep <- parm$mu.rep^2 / (parm$sd.rep^2 - parm$mu.rep)
scenario = dim(parm)[1]

no.study <- 50

set.seed(10)

result <- foreach(k = 1:scenario) %dopar% {
	library(metafor)
	library(tidyverse)
	parm.est <- array(dim = c(iteration, 52))
	for(i in 1:iteration){
		effect.size <- array(dim = c(no.study, 2))
		mu <- 10
		alpha <- 1
		no.rep <- 2 + rnbinom(n = no.study, size = parm$size.rep[k], mu = parm$mu.rep[k])
		sigma <- exp(rnorm(n = no.study, mean = parm$mu.log.sigma[k], sd = sqrt(parm$var.log.sigma[k])))
        for(m in 1:no.study){
			log.e <- rnorm(n=1, mean=0, sd=parm$sd.e[k])
			alpha.m <- alpha * exp(log.e)
			yc <- mu * rlnorm(n = no.rep[m],meanlog = -log(sigma[m]^2 + 1) / 2, sdlog = sqrt(log(sigma[m]^2 + 1)))
			yt <- mu * alpha.m * 
			      rlnorm(n = no.rep[m], meanlog = -log(sigma[m]^2 + 1) / 2, sdlog = sqrt(log(sigma[m]^2 + 1)))
			effect.size[m,1] <- log(mean(yt)/mean(yc))
			effect.size[m,2] <- var(yt)/no.rep[m]/mean(yt)^2 + var(yc)/no.rep[m]/mean(yc)^2			
		}
		
		effect.size <- data.frame(effect.size)
		names(effect.size) <- c("ES", "ES.var")
		if(parm$missing[k] == 0){
			mod <- try(rma(yi = ES, vi = ES.var, test = "knha", data = effect.size), silent = T)
		    parm.est[i, 1] <- try(mod$b, silent = T) %>% as.numeric()
		    parm.est[i, 2] <- try(mod$b, silent = T) %>% as.numeric()
		
		}
		if(parm$missing[k] != 0){
			which.miss <- sample(x = 1:no.study, size = parm$missing[k])
			effect.size.reduced <- effect.size[-which.miss,]
			mean.ES.var <- mean(effect.size.reduced$ES.var)
			effect.size.imputed <- 
			  effect.size %>% 
			  mutate(ES.var = replace(x = ES.var, list = which.miss, value = mean.ES.var))
		
		    #Complete case analysis#
		    mod1 <- try(rma(yi = ES, vi = ES.var, test = "knha", data = effect.size.reduced), silent = T)
		    parm.est[i,1] <- try(mod1$b, silent = T) %>% as.numeric()
		
		    #Imputation using mean within-study error variance#
		    mod2 <- try(rma(yi = ES, vi = ES.var, test = "knha", data = effect.size.imputed), silent = T)
		    parm.est[i, 2] <- try(mod2$b, silent = T) %>% as.numeric()
		
		}
		
		parm.est[i,3:52] <- effect.size$ES.var
		
	}
	
	parm.est
}

save.image(file = "missing_random_impute_mean.RData")