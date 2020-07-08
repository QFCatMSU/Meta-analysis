#Plot for independent studies#
load("Independent.RData")

#Calculate error rate and confidence interval of error rate#
error = array(dim=c(scenario, 6))
error.ci = array(dim=c(scenario, 6))
cover = (parm.low<log(1.5)) + (parm.up>log(1.5))

for(i in 1:scenario){
	error[i,] = 1-colSums(cover[i,,]==2, na.rm=T)/iteration
	error.ci[i,] = qnorm(0.975)*sqrt(error[i,]*(1-error[i,])/iteration)
}

error = error[,1:5]
error.ci = error.ci[,1:5]

#Plot error rate. Each panel is a combination of number of papers and studies per paper#
quartz(w=8, h=5)
par(mfrow=c(3,3), mar=c(0,0,0,0), oma=c(4,4,2,2))
for(i in 1:length(no.paper)){
	for(j in 1:length(size)){
		error.plot = as.vector(t(error[parm$no.paper==no.paper[i] & parm$size==size[j],]))
		error.ci.plot = as.vector(t(error.ci[parm$no.paper==no.paper[i] & parm$size==size[j],]))
		plot(error.plot~c(1:5,7:11,13:17), ylim=c(0.01,0.09), xlim=c(0.5,17.5), pch=19, axes=F)
		box()
		arrows(x0=c(1:5,7:11,13:17), y0=error.plot-error.ci.plot, y1=error.plot+error.ci.plot, length=0)
		abline(h=0.05, lty=2, col="grey")
		abline(v=c(6,12), lty=2, col="grey")
		mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
		mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
		mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
		if(i==3){axis(1, at=c(1:5,7:11,13:17), tck=-0.03, labels=rep(c("1","2","3","4","5"),times=3), cex.axis=0.85)}
		if(j==1){axis(2, at=c(0.02,0.05,0.08), tck=-0.03)}
	}
	
}
#Label the axes#
mtext("Methods", side=1, outer=T, line=2.7, font=2, cex=1)
mtext("Type I error rate", side=2, outer=T, line=2.7, font=2, cex=1)

#Label number of paper and number of study per paper#
mtext(text="20 papers", side=4, adj=0.88 , outer=T, line=0.2, cex=0.8)
mtext(text="10 papers", side=4, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="5 papers", side=4, adj=0.12, outer=T, line=0.2, cex=0.8)

mtext(text="1.5 studies/paper", side=3, adj=0.1 , outer=T, line=0.2, cex=0.8)
mtext(text="5.5 studies/paper", side=3, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="15.5 studies/paper", side=3, adj=0.9, outer=T, line=0.2, cex=0.8)



load("EqualCorr.RData")

#Calculate error rate and confidence interval of error rate#
error = array(dim=c(scenario, 6))
error.ci = array(dim=c(scenario, 6))
cover = (parm.low<log(1.5)) + (parm.up>log(1.5))

for(i in 1:scenario){
	error[i,] = 1-colSums(cover[i,,]==2, na.rm=T)/iteration
	error.ci[i,] = qnorm(0.975)*sqrt(error[i,]*(1-error[i,])/iteration)
}

error = error[,1:5]
error.ci = error.ci[,1:5]

#Plot error rate. Each panel is a combination of number of papers and studies per paper#
quartz(w=8, h=5)
par(mfrow=c(3,3), mar=c(0,0,0,0), oma=c(4,4,2,2))
for(i in 1:length(no.paper)){
	for(j in 1:length(size)){
		error1 = as.vector(t(error[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.1,]))
		error.ci1 = as.vector(t(error.ci[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.1,]))
		error2 = as.vector(t(error[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.5,]))
		error.ci2 = as.vector(t(error.ci[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.5,]))
		error3 = as.vector(t(error[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.9,]))
		error.ci3 = as.vector(t(error.ci[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.9,]))
		plot(error1~c(1:5,7:11,13:17), ylim=c(0.01,0.11), xlim=c(0.5,17.5), pch=19, axes=F)
		points(error2~c(1:5,7:11,13:17), pch=19, col="orange")
		points(error3~c(1:5,7:11,13:17), pch=19, col="red")
		box()
		arrows(x0=c(1:5,7:11,13:17), y0=error1-error.ci1, y1=error1+error.ci1, length=0)
		arrows(x0=c(1:5,7:11,13:17), y0=error2-error.ci2, y1=error2+error.ci2, length=0)
		arrows(x0=c(1:5,7:11,13:17), y0=error3-error.ci3, y1=error3+error.ci3, length=0)
		abline(h=0.05, lty=2, col="grey")
		abline(v=c(6,12), lty=2, col="grey")
		mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
		mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
		mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
		if(i==3){axis(1, at=c(1:5,7:11,13:17), tck=-0.03, labels=rep(c("1","2","3","4","5"),times=3), cex.axis=0.85)}
		if(j==1){axis(2, at=c(0.02,0.05,0.08), tck=-0.03)}
	}
	
}
#Label the axes#
mtext("Methods", side=1, outer=T, line=2.7, font=2, cex=1)
mtext("Type I error rate", side=2, outer=T, line=2.7, font=2, cex=1)

#Label number of paper and number of study per paper#
mtext(text="20 papers", side=4, adj=0.88 , outer=T, line=0.2, cex=0.8)
mtext(text="10 papers", side=4, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="5 papers", side=4, adj=0.12, outer=T, line=0.2, cex=0.8)

mtext(text="1.5 studies/paper", side=3, adj=0.1 , outer=T, line=0.2, cex=0.8)
mtext(text="5.5 studies/paper", side=3, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="15.5 studies/paper", side=3, adj=0.9, outer=T, line=0.2, cex=0.8)



load("UnequalCorr.RData")

#Calculate error rate and confidence interval of error rate#
error = array(dim=c(scenario, 6))
error.ci = array(dim=c(scenario, 6))
cover = (parm.low<log(1.5)) + (parm.up>log(1.5))

for(i in 1:scenario){
	error[i,] = 1-colSums(cover[i,,]==2, na.rm=T)/iteration
	error.ci[i,] = qnorm(0.975)*sqrt(error[i,]*(1-error[i,])/iteration)
}

error = error[,c(1:4,6)]
error.ci = error.ci[,c(1:4,6)]

#Plot error rate. Each panel is a combination of number of papers and studies per paper#
quartz(w=8, h=5)
par(mfrow=c(3,3), mar=c(0,0,0,0), oma=c(4,4,2,2))
for(i in 1:length(no.paper)){
	for(j in 1:length(size)){
		error1 = as.vector(t(error[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.1,]))
		error.ci1 = as.vector(t(error.ci[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.1,]))
		error2 = as.vector(t(error[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.6,]))
		error.ci2 = as.vector(t(error.ci[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.6,]))

		plot(error1~c(1:5,7:11,13:17), ylim=c(0.01,0.15), xlim=c(0.5,17.5), pch=19, axes=F)
		points(error2~c(1:5,7:11,13:17), pch=19, col="orange")
		box()
		arrows(x0=c(1:5,7:11,13:17), y0=error1-error.ci1, y1=error1+error.ci1, length=0)
		arrows(x0=c(1:5,7:11,13:17), y0=error2-error.ci2, y1=error2+error.ci2, length=0)
		abline(h=0.05, lty=2, col="grey")
		abline(v=c(6,12), lty=2, col="grey")
		mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
		mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
		mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
		if(i==3){axis(1, at=c(1:5,7:11,13:17), tck=-0.03, labels=rep(c("1","2","3","4","5"),times=3), cex.axis=0.85)}
		if(j==1){axis(2, at=c(0.02,0.05,0.08), tck=-0.03)}
	}
	
}
#Label the axes#
mtext("Methods", side=1, outer=T, line=2.7, font=2, cex=1)
mtext("Type I error rate", side=2, outer=T, line=2.7, font=2, cex=1)

#Label number of paper and number of study per paper#
mtext(text="20 papers", side=4, adj=0.88 , outer=T, line=0.2, cex=0.8)
mtext(text="10 papers", side=4, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="5 papers", side=4, adj=0.12, outer=T, line=0.2, cex=0.8)

mtext(text="1.5 studies/paper", side=3, adj=0.1 , outer=T, line=0.2, cex=0.8)
mtext(text="5.5 studies/paper", side=3, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="15.5 studies/paper", side=3, adj=0.9, outer=T, line=0.2, cex=0.8)


