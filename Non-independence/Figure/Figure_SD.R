#Define the layout of multiple panel figure using layout matrix#
layout.mat = rbind(c(1:3),c(1:3),c(1:3),c(4:6),c(4:6),c(4:6),c(7:9),c(7:9),c(7:9),c(10:12),c(10:12),c(10:12),c(19:21),c(13:15),c(13:15),c(13:15),c(16:18),c(16:18),c(16:18))

#Set up plotting device and figure margins#
quartz(w=9, h=10)
layout(mat=layout.mat)
par(mar=c(0,0,0,0), oma=c(4,4,3,2))


#######################
##Independent studies##
#######################
load("IIDStudy.RData")

#Calculate RMSE, error rate, and the confidence interval of error rate#
sd = array(dim=c(scenario,5))
for(i in 1:scenario){
	sd[i,] = apply(parm.est[i,,], 2, sd, na.rm=T)
}

#Find which sample (row in rmse and error rate) corresponds to each paper size#
#Each size is plotted in a separate panel#
size = array(dim=c(6,scenario/3))
size[1,] = with(parm, which(mu==0.5))
size[2,] = with(parm, which(mu==4.5))
size[3,] = with(parm, which(mu==14.5))

paper.mean = c(1.5,5.5,15.5)
paper.sd = c(1.11,7.02,18.68)

for(i in 1:3){
	plot(sd[size[i,1],]~c(1:5), xlim=c(0.5,6*(scenario/3-1)+5.5), ylim=range(-0.05,0.4), pch=19, axes=F)
	box()
	if(i == 1){axis(2, at=c(0,0.15, 0.3), tck=-0.03)}
	#axis(1, at=c(1:5,7:11,13:17), tck=-0.03, labels=rep(c("1","2","3","4","5"), times=3), cex.axis=0.8)
	#mtext(letters[i], side=3, line=-1.3, adj=0.02, font=2)
	for(j in 2:length(size[i,])){
		points(sd[size[i,j],]~c(((j-1)*6+1):((j-1)*6+5)), pch=19)
		abline(v=6*(j-1), lty=2, col="grey")
	}
	mtext(paste0("Mean:",paper.mean[i]),side=1,adj=0.95,line=-2.3, cex=0.75)
	mtext(paste0("SD:",paper.sd[i]),side=1, adj=0.95,line=-1.3, cex=0.75)
	mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
	mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
	mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
	mtext(expression(rho==0), side=1, adj=0.5, cex=0.8, line=-1.3)
	
}


#######################
##Equally correlated studies##
#######################
load("EqualStudy.RData")

#Calculate standard deviation of estimates#
sd = array(dim=c(scenario,5))
for(i in 1:scenario){
	sd[i,] = apply(parm.est[i,,], 2, sd, na.rm=T)
}


#Find which sample corresponds to each paper size#
size = array(dim=c(9,3))
size[1,] = with(parm, which(mu==0.5 & rho.e==0.1))
size[2,] = with(parm, which(mu==4.5 & rho.e==0.1))
size[3,] = with(parm, which(mu==14.5 & rho.e==0.1))
size[4,] = with(parm, which(mu==0.5 & rho.e==0.5))
size[5,] = with(parm, which(mu==4.5 & rho.e==0.5))
size[6,] = with(parm, which(mu==14.5 & rho.e==0.5))
size[7,] = with(parm, which(mu==0.5 & rho.e==0.9))
size[8,] = with(parm, which(mu==4.5 & rho.e==0.9))
size[9,] = with(parm, which(mu==14.5 & rho.e==0.9))



paper.mean = rep(c(1.5,5.5,15.5), times=3)
paper.sd = rep(c(1.11,7.02,18.68), times=3)
for(i in 1:9){
	plot(sd[size[i,1],]~c(1:5), xlim=c(0.5,17.5), ylim=range(-0.05,0.4), pch=19, axes=F)
	box()
	if(i %in% c(1,4,7)){axis(2, at=c(0,0.15, 0.3), tck=-0.03)}
	if(i %in% c(7,8,9)){axis(1, at=c(1:5,7:11,13:17), tck=-0.03, labels=rep(c("1","2","3","4","5"), times=3), cex.axis=0.8)}
	#mtext(letters[i], side=3, line=-1.3, adj=0.02, font=2)
	for(j in 2:length(size[i,])){
		points(sd[size[i,j],]~c(((j-1)*6+1):((j-1)*6+5)), pch=19)
		abline(v=6*(j-1), lty=2, col="grey")
	}
	mtext(paste0("Mean:",paper.mean[i]),side=1,adj=0.95,line=-2.3, cex=0.75)
	mtext(paste0("SD:",paper.sd[i]),side=1, adj=0.95,line=-1.3, cex=0.75)
	mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
	mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
	mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
	if(i %in% c(1,2,3)){mtext(expression(rho==0.1), side=1, adj=0.5, cex=0.8, line=-1.3)}
	if(i %in% c(4,5,6)){mtext(expression(rho==0.5), side=1, adj=0.5, cex=0.8, line=-1.3)}
	if(i %in% c(7,8,9)){mtext(expression(rho==0.9), side=1, adj=0.5, cex=0.8, line=-1.3)}
	
}


##################################
## Unequally correlated studies ##
##################################
#Load results from unequally correlated study with tau varying from paper to paper#
load("UnequalStudy_VaryTau.RData")
sd.varytau = array(dim=c(scenario,5))
for(i in 1:scenario){
	sd.varytau[i,] = apply(parm.est[i,,], 2, sd, na.rm=T)
}


#Local results for unequally correlated study with constant tau#
load("UnequalStudy.RData")

#Calculate RMSE, error rate, and the confidence interval of error rate#
sd = array(dim=c(scenario,5))

for(i in 1:scenario){
	sd[i,] = apply(parm.est[i,,], 2, sd, na.rm=T)
}


#Find which sample corresponds to each paper size#
size = array(dim=c(6,3))
size[1,] = with(parm, which(mu==0.5 & rho.e==0.1))
size[2,] = with(parm, which(mu==4.5 & rho.e==0.1))
size[3,] = with(parm, which(mu==14.5 & rho.e==0.1))
size[4,] = with(parm, which(mu==0.5 & rho.e==0.6))
size[5,] = with(parm, which(mu==4.5 & rho.e==0.6))
size[6,] = with(parm, which(mu==14.5 & rho.e==0.6))

paper.mean = rep(c(1.5,5.5,15.5), times=2)
paper.sd = rep(c(1.11,7.02,18.68), times=2)

for(i in 1:6){
	plot(sd[size[i,1],]~c(1:5), xlim=c(0.5,23.5), ylim=range(-0.05,0.44), pch=19, axes=F)
	box()
	if(i %in% c(1,4)){axis(2, at=c(0,0.2, 0.4), tck=-0.03)}
	if(i %in% c(4,5,6)){axis(1, at=c(1:5,7:11,13:17,19:23), tck=-0.03, labels=rep(c("1","2","3","4","5"), times=4), cex.axis=0.7)}
	#mtext(letters[i], side=3, line=-1.3, adj=0.02, font=2)
	for(j in 2:length(size[i,])){
		points(sd[size[i,j],]~c(((j-1)*6+1):((j-1)*6+5)), pch=19)
		abline(v=6*(j-1), lty=2, col="grey")
	}
	points(sd.varytau[i,]~c(19:23), pch=19)
	abline(v=18, lty=2, col="grey")
	mtext(paste0("Mean:",paper.mean[i]),side=1,adj=0.95,line=-2.3, cex=0.75)
	mtext(paste0("SD:",paper.sd[i]),side=1, adj=0.95,line=-1.3, cex=0.75)
	mtext(expression(tau==0.1), side=3, adj=0.06, cex=0.8, line=-1.4)
	mtext(expression(tau==0.5), side=3, adj=0.35, cex=0.8, line=-1.4)
	mtext(expression(tau==1), side=3, adj=0.64, cex=0.8, line=-1.4)
	mtext(expression(tau==0.1-1), side=3, adj=0.98, cex=0.77, line=-1.4)
	if(i %in% c(1,2,3)){mtext(expression(rho==0.1-0.4), side=1, adj=0.5, cex=0.8, line=-1.3)}
	if(i %in% c(4,5,6)){mtext(expression(rho==0.6-0.9), side=1, adj=0.5, cex=0.8, line=-1.3)}
	
}

#Label each section of figures with the type of non-independence#
mtext("Experiment 1: equally correlated studies", outer=T, line=0.5, font=2, adj=1)
mtext("Experiment 2: unequally correlated studies", outer=T, line=-46.8, font=2, adj=1)

#Label axes#
mtext("Standard deviation", side=2, outer=T, line=2.5, font=2)
mtext("Meta-analysis methods", side=1, outer=T, line=2.5, font=2)
