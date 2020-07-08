#Plot for independent studies#
load("Independent.RData")

#Calculate error rate and confidence interval of error rate#
sd = array(dim=c(scenario, 6))

for(i in 1:scenario){
	sd[i,] = apply(parm.est[i,,], 2, sd, na.rm=T)
}

sd = sd[,c(1:5)]

#Plot error rate. Each panel is a combination of number of papers and studies per paper#
quartz(w=8, h=5)
par(mfrow=c(3,3), mar=c(0,0,0,0), oma=c(4,4,2,2))
for(i in 1:length(no.paper)){
	for(j in 1:length(size)){
		sd.plot = as.vector(t(sd[parm$no.paper==no.paper[i] & parm$size==size[j],]))
		plot(sd.plot~c(1:5,7:11,13:17), ylim=c(-0.05,0.65), xlim=c(0.5,17.5), pch=19, axes=F)
		box()
		abline(v=c(6,12), lty=2, col="grey")
		mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
		mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
		mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
		if(i==3){axis(1, at=c(1:5,7:11,13:17), tck=-0.03, labels=rep(c("1","2","3","4","5"),times=3), cex.axis=0.85)}
		if(j==1){axis(2, at=c(0.0,0.3,0.6), tck=-0.03)}
	}
	
}
#Label the axes#
mtext("Methods", side=1, outer=T, line=2.7, font=2, cex=1)
mtext("Standard Deviation", side=2, outer=T, line=2.7, font=2, cex=1)

#Label number of paper and number of study per paper#
mtext(text="20 papers", side=4, adj=0.88 , outer=T, line=0.2, cex=0.8)
mtext(text="10 papers", side=4, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="5 papers", side=4, adj=0.12, outer=T, line=0.2, cex=0.8)

mtext(text="1.5 studies/paper", side=3, adj=0.1 , outer=T, line=0.2, cex=0.8)
mtext(text="5.5 studies/paper", side=3, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="15.5 studies/paper", side=3, adj=0.9, outer=T, line=0.2, cex=0.8)


#Equally correlated studies#
load("EqualCorr.RData")
sd = array(dim=c(scenario, 6))
for(i in 1:scenario){
	sd[i,] = apply(parm.est[i,,], 2, sd, na.rm=T)
}
sd = sd[,1:5]


quartz(w=8, h=5)
par(mfrow=c(3,3), mar=c(0,0,0,0), oma=c(4,4,2,2))
for(i in 1:length(no.paper)){
	for(j in 1:length(size)){
		sd1 = as.vector(t(sd[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.1,]))
		sd2 = as.vector(t(sd[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.5,]))
		sd3 = as.vector(t(sd[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.9,]))
		plot(sd1~c(1:5,7:11,13:17), ylim=c(-0.05,0.65), xlim=c(0.5,17.5), pch=19, axes=F, type="o")
		points(sd2~c(1:5,7:11,13:17), pch=19, col="orange", type="o")
		points(sd3~c(1:5,7:11,13:17), pch=19, col="red", type="o")
		box()
		abline(v=c(6,12), lty=2, col="grey")
		mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
		mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
		mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
		if(i==3){axis(1, at=c(1:5,7:11,13:17), tck=-0.03, labels=rep(c("1","2","3","4","5"),times=3), cex.axis=0.85)}
		if(j==1){axis(2, at=c(0.0,0.3,0.6), tck=-0.03)}
	}
	
}
#Label the axes#
mtext("Methods", side=1, outer=T, line=2.7, font=2, cex=1)
mtext("Standard Deviation", side=2, outer=T, line=2.7, font=2, cex=1)

#Label number of paper and number of study per paper#
mtext(text="20 papers", side=4, adj=0.88 , outer=T, line=0.2, cex=0.8)
mtext(text="10 papers", side=4, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="5 papers", side=4, adj=0.12, outer=T, line=0.2, cex=0.8)

mtext(text="1.5 studies/paper", side=3, adj=0.1 , outer=T, line=0.2, cex=0.8)
mtext(text="5.5 studies/paper", side=3, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="15.5 studies/paper", side=3, adj=0.9, outer=T, line=0.2, cex=0.8)




#Equally correlated studies#
load("UnequalCorr.RData")
sd = array(dim=c(scenario, 6))
for(i in 1:scenario){
	sd[i,] = apply(parm.est[i,,], 2, sd, na.rm=T)
}
sd = sd[,1:5]


quartz(w=8, h=5)
par(mfrow=c(3,3), mar=c(0,0,0,0), oma=c(4,4,2,2))
for(i in 1:length(no.paper)){
	for(j in 1:length(size)){
		sd1 = as.vector(t(sd[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.1,]))
		sd2 = as.vector(t(sd[parm$no.paper==no.paper[i] & parm$size==size[j] & parm$rho.e==0.6,]))
		plot(sd1~c(1:5,7:11,13:17), ylim=c(-0.05,0.65), xlim=c(0.5,17.5), pch=19, axes=F, type="o")
		points(sd2~c(1:5,7:11,13:17), pch=19, col="red", type="o")
		box()
		abline(v=c(6,12), lty=2, col="grey")
		mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
		mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
		mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
		if(i==3){axis(1, at=c(1:5,7:11,13:17), tck=-0.03, labels=rep(c("1","2","3","4","5"),times=3), cex.axis=0.85)}
		if(j==1){axis(2, at=c(0.0,0.3,0.6), tck=-0.03)}
	}
	
}
#Label the axes#
mtext("Methods", side=1, outer=T, line=2.7, font=2, cex=1)
mtext("Standard Deviation", side=2, outer=T, line=2.7, font=2, cex=1)

#Label number of paper and number of study per paper#
mtext(text="20 papers", side=4, adj=0.88 , outer=T, line=0.2, cex=0.8)
mtext(text="10 papers", side=4, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="5 papers", side=4, adj=0.12, outer=T, line=0.2, cex=0.8)

mtext(text="1.5 studies/paper", side=3, adj=0.1 , outer=T, line=0.2, cex=0.8)
mtext(text="5.5 studies/paper", side=3, adj=0.5, outer=T, line=0.2, cex=0.8)
mtext(text="15.5 studies/paper", side=3, adj=0.9, outer=T, line=0.2, cex=0.8)

