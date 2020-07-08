library(plyr)
library(MASS)
library(metafor)
library(robumeta)
library(Matrix)

rep.low = 3
rep.up = 20
iteration = 3000

no.paper = c(20, 10, 5)
sd.e = c(0.1,0.5,1)
size = c(0.3339891,0.4522312,0.6287481)

parm = expand.grid(size=size, sd.e=sd.e, no.paper=no.paper)
parm$mu = rep(c(0.5,4.5,14.5),times=9)
scenario = dim(parm)[1]

print(scenario)

parm.est = array(dim=c(scenario,iteration,6))
parm.low = array(dim=c(scenario,iteration,6))
parm.up = array(dim=c(scenario,iteration,6))
tau2.paper = array(dim=c(scenario,iteration,2))

set.seed(5)


for(k in 1:scenario){
	for(a in 1:iteration){
		no.study = 1 + rnbinom(n=parm$no.paper[k], size=parm$size[k], mu=parm$mu[k])
		raw.data = data.frame()
	for(i in 1:parm$no.paper[k]){
		mu = 10
		alpha.t = 1.5
		log.e = rnorm(n=no.study[i], mean=0, sd=parm$sd.e[k])
		for(j in 1:no.study[i]){
			alpha = alpha.t * exp(log.e[j])
			no.rep = runif(1,rep.low,rep.up)
			sd.log.epsilon = runif(1,0.1,0.3)
			yc = mu * exp(rnorm(n=no.rep, mean=0, sd=sd.log.epsilon))
			yt = mu * alpha * exp(rnorm(n=no.rep, mean=0, sd=sd.log.epsilon))
			data.temp = data.frame(yc,yt, paper=rep(i,no.rep), study=rep(j, no.rep))
			raw.data = rbind(raw.data, data.temp)
		}
	}

	effect.size = ddply(raw.data, c("paper","study"), summarise, log.ratio=log(mean(yt)/mean(yc)), var.log.ratio=var(yt)/	length(yt)/mean(yt)^2+var(yc)/length(yc)/mean(yc)^2)
	
	#Fit model that ignores paper effects#
	mod1 = try(rma(yi=log.ratio, vi=var.log.ratio, mods=~1, method="REML", data=effect.size, test="knha"), silent=T)
	parm.est[k,a,1] = as.numeric(try(mod1$b, silent=T))
	parm.low[k,a,1] = as.numeric(try(mod1$ci.lb, silent=T))
	parm.up[k,a,1] = as.numeric(try(mod1$ci.ub, silent=T))

	#Calculate mean effect size from each paper#
	effect.size.paper = ddply(effect.size, c("paper"), summarise, log.ratio.p=rma(yi=log.ratio,vi=var.log.ratio, method="FE")$b, var.log.ratio.p=(rma(yi=log.ratio,vi=var.log.ratio, method="FE")$se)^2)
	mod2 = try(rma(yi=log.ratio.p, vi=var.log.ratio.p, mods=~1, method="REML", data=effect.size.paper, test="knha"), silent=T)
	parm.est[k,a,2] = as.numeric(try(mod2$b, silent=T))
	parm.low[k,a,2] = as.numeric(try(mod2$ci.lb, silent=T))
	parm.up[k,a,2] = as.numeric(try(mod2$ci.ub, silent=T))

	#Randomly choose one effect size from each paper#
	effect.size.random = ddply(effect.size, c("paper"), summarise, study.no=sample(study,size=1), log.ratio=log.ratio[study.no], var.log.ratio=var.log.ratio[study.no])
	mod3 = try(rma(yi=log.ratio, vi=var.log.ratio, mods=~1, method="REML", data=effect.size.random, test="knha"), silent=T)
	parm.est[k,a,3] = as.numeric(try(mod3$b, silent=T))
	parm.low[k,a,3] = as.numeric(try(mod3$ci.lb, silent=T))
	parm.up[k,a,3] = as.numeric(try(mod3$ci.ub, silent=T))
	
	#Model with a simple paper effect#
	effect.size$study.unique = paste0(effect.size$paper,"-",effect.size$study)
	mod4 = try(rma.mv(yi=log.ratio, V=var.log.ratio, mods=~1, random=list(~1|paper,~1|study.unique), data=effect.size, test="t"), silent=T)
	parm.est[k,a,4] = as.numeric(try(mod4$b, silent=T))
	parm.low[k,a,4] = as.numeric(try(mod4$ci.lb, silent=T))
	parm.up[k,a,4] = as.numeric(try(mod4$ci.ub, silent=T))
	#Store the variance component estimates, first element is paper, second element is study#
	tau2.paper[k,a,1] = as.numeric(try(mod4$sigma2[1], silent=T))	
	tau2.paper[k,a,2] = as.numeric(try(mod4$sigma2[2], silent=T))
	
	#Robust variance estimates method by Hedges#
	mod5 = try(robu(formula=log.ratio~1, data=effect.size, studynum=paper, var.eff.size=var.log.ratio), silent=T)
	parm.est[k,a,5] = as.numeric(try(mod5$reg_table$b.r, silent=T)) 
	parm.low[k,a,5] = as.numeric(try(mod5$reg_table$CI.L, silent=T))
	parm.up[k,a,5] = as.numeric(try(mod5$reg_table$CI.U, silent=T))
	
	
	#Robust variance estimates method by Hedges with hierarchical weights#
	mod6 = try(robu(formula=log.ratio~1, data=effect.size, studynum=paper, var.eff.size=var.log.ratio, modelweights="HIER"), silent=T)
	parm.est[k,a,6] = as.numeric(try(mod6$reg_table$b.r, silent=T)) 
	parm.low[k,a,6] = as.numeric(try(mod6$reg_table$CI.L, silent=T))
	parm.up[k,a,6] = as.numeric(try(mod6$reg_table$CI.U, silent=T))
	}
	print(k)

}

save.image(file="Independent.RData")


