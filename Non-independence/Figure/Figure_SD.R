#Define the layout of multiple panel figure using layout matrix#
layout.mat = rbind(c(1:2),c(1:2),c(1:2),c(11:12),c(3:4),c(3:4),c(3:4),c(5:6),c(5:6),c(5:6),c(13:14),c(7:8),c(7:8),c(7:8),c(9:10),c(9:10),c(9:10))

method.col = c(rgb(0,0,0),rgb(0.9,0.6,0),rgb(0.8,0.4,0),rgb(0,0.45,0.7),rgb(0.35,0.7,0.9))

#Set up plotting device and figure margins#
quartz(w=6.5, h=9)
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

for(i in c(1,3)){
	plot(sd[size[i,1],]~c(1:5), xlim=c(0.5,6*(scenario/3-1)+5.5), ylim=range(-0.06,0.4), pch=19, axes=F, col=method.col)
	box()
	if(i == 1){axis(2, at=c(0,0.15, 0.3), tck=-0.03)}
	for(j in 2:length(size[i,])){
		points(sd[size[i,j],]~c(((j-1)*6+1):((j-1)*6+5)), pch=19,col=method.col)
		abline(v=6*(j-1), lty=2, col="grey")
	}
	mtext(paste0("Mean:",paper.mean[i]),side=1,adj=0.95,line=-2.3, cex=0.75)
	mtext(paste0("SD:",paper.sd[i]),side=1, adj=0.95,line=-1.3, cex=0.75)
	mtext(expression(tau==0.1), side=3, adj=0.1, cex=0.8, line=-1.4)
	mtext(expression(tau==0.5), side=3, adj=0.5, cex=0.8, line=-1.4)
	mtext(expression(tau==1), side=3, adj=0.9, cex=0.8, line=-1.4)
	mtext(expression(rho==0), side=1, adj=0.5, cex=0.8, line=-1.3)
	#mtext(paste0("(",LETTERS[i],")"), side=1, adj=0.02, line=-1.35, cex=0.75)
	
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
for(i in c(1,3,7,9)){
	plot(sd[size[i,1],]~c(1:5), xlim=c(0.5,17.5), ylim=range(-0.06,0.4), pch=19, axes=F, col=method.col)
	box()
	if(i %in% c(1,4,7)){axis(2, at=c(0,0.15, 0.3), tck=-0.03)}
	for(j in 2:length(size[i,])){
		points(sd[size[i,j],]~c(((j-1)*6+1):((j-1)*6+5)), pch=19, col=method.col)
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
	#mtext(paste0("(",LETTERS[3+i],")"), side=1, adj=0.02, line=-1.35, cex=0.75)
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

for(i in c(1,3,4,6)){
	plot(sd[size[i,1],]~c(1:5), xlim=c(0.5,23.5), ylim=range(-0.06,0.44), pch=19, axes=F, col=method.col)
	box()
	if(i %in% c(1,4)){axis(2, at=c(0,0.2, 0.4), tck=-0.03)}
	for(j in 2:length(size[i,])){
		points(sd[size[i,j],]~c(((j-1)*6+1):((j-1)*6+5)), pch=19, col=method.col)
		abline(v=6*(j-1), lty=2, col="grey")
	}
	points(sd.varytau[i,]~c(19:23), pch=19, col=method.col)
	abline(v=18, lty=2, col="grey")
	mtext(paste0("Mean:",paper.mean[i]),side=1,adj=0.95,line=-2.3, cex=0.75)
	mtext(paste0("SD:",paper.sd[i]),side=1, adj=0.95,line=-1.3, cex=0.75)
	mtext(expression(tau==0.1), side=3, adj=0.06, cex=0.8, line=-1.4)
	mtext(expression(tau==0.5), side=3, adj=0.35, cex=0.8, line=-1.4)
	mtext(expression(tau==1), side=3, adj=0.64, cex=0.8, line=-1.4)
	mtext(expression(tau==0.1-1), side=3, adj=0.98, cex=0.77, line=-1.4)
	if(i %in% c(1,2,3)){mtext(expression(rho==0.1-0.4), side=1, adj=0.5, cex=0.8, line=-1.3)}
	if(i %in% c(4,5,6)){mtext(expression(rho==0.6-0.9), side=1, adj=0.5, cex=0.8, line=-1.3)}
	#mtext(paste0("(",LETTERS[12+i],")"), side=1, adj=0.02, line=-1.35, cex=0.75)
}

#Label each section of figures with the type of non-independence#
mtext("Experiment 1: independence", outer=T, line=0.5, font=2, adj=1)
mtext("Experiment 1: equal correlation", outer=T, line=-14, font=2, adj=1)
mtext("Experiment 2: unequal correlation", outer=T, line=-39, font=2, adj=1)

#Label axes#
mtext("Standard deviation", side=2, outer=T, line=2.5, font=2)

mtext("Methods:", outer=T, line=0.5, font=2, adj=0, cex=0.95)
legend("topleft", legend=c("1","2","3","4","5"), pch=19, xpd=NA, inset=c(-0.75,-4.88), bty="n", horiz=T, cex=1.35, col=method.col)