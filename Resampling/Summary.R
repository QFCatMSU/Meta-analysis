library(boot)

load("Compile.RData")

#Kolmogorov-Smirnov test for uniform distribution#
ks.iter = 3000
ks.p = array()
ks.p.paper = array()
sig.prop = array()
sig.prop.paper = array()

#Bootstrap and test uniform distribution for p-values for group comparison#
for(i in 1:ks.iter){
	c = sample(x=1:20, size=20, replace=T)
	p.test = apply(X=p.all[,c], MARGIN=2, FUN=sample, size=1, replace=T)
	p.paper.test = apply(X=p.paper.all[,c], MARGIN=2, FUN=sample, size=1, replace=T)
	ks.p[i] = ks.test(x=p.test, y="punif")$p.value
	ks.p.paper[i] = ks.test(x=p.paper.test, y="punif")$p.value	
	sig.prop[i] = length(which(p.test<=0.05))/length(p.test)
	sig.prop.paper[i] = length(which(p.paper.test<=0.05))/length(p.test)
}


quartz(w=7,h=3.5)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(p.hist, xlab="p-value",main="", ylim=c(0,7))
plot(p.paper.hist, xlab="p-value",main="Adding paper effect", ylim=c(0,7))


#P-value from KS test for uniform distribution#
#Expect uniform distribution of p-value from KS test, thus 5% signficant deviation from uniform#
quartz(w=7,h=3.5)
par(mfrow=c(1,2), mar=c(4,4,1,1), cex=0.85)
hist(ks.p, breaks=seq(0,1,0.05), ylim=c(0,2500), main="Original method", xlab="P-value testing uniform distribution")
hist(ks.p.paper, breaks=seq(0,1,0.05), ylim=c(0,2500), main="With a random paper effect", xlab="P-value testing uniform distribution")

prop.test(x=length(which(ks.p<=0.05)), n=ks.iter)
prop.test(x=length(which(ks.p.paper<=0.05)), n=ks.iter)

#Proportion of significant result from 20 papers#
#Expect 5% significant#
quartz(w=7,h=3.5)
par(mfrow=c(1,2), mar=c(4,4,1,1), cex=0.85)
hist(sig.prop, ylim=c(0,800), main="Original method", xlab="% significant")
hist(sig.prop.paper, ylim=c(0,800), main="With a random paper effect", xlab="% significant")

SigPropCalc = function(data, idx){
	col = sample(x=dim(data)[2], replace=T)
	p.sample = apply(data[,col], MARGIN=2, FUN=sample, size=1, replace=T)
	length(which(p.sample<=0.05))/length(p.sample)
}

mean(sig.prop)
mean(sig.prop.paper)

boot.p = boot(data=p.all, statistic=SigPropCalc, R=3000)
boot.ci(boot.p, type="perc")
boot.p.paper = boot(data=p.paper.all, statistic=SigPropCalc, R=4000)
boot.ci(boot.p.paper, type="all")

#Contrast MA with or without accounting for non-independence#
accounted = c("272.R", "282.R", "303.R", "628.R", "424.R")
not.accounted = c("53.R", "2.R", "82.R", "120.R", "511.R", "695.R", "1365.R", "1307.R", "1180.R", "1185.R", "674.R", "639.R",  "585.R", "759.R", "952.R")
i.accounted = which(code.all %in% accounted)
i.not.accounted = which(code.all %in% not.accounted)

#Histogram of p-value for group comparison (for illustration purpose)#
quartz(w=4,h=4)
par(mfrow=c(1,1), mar=c(4,4,1,1))
for(i in 1:9){
	hist(p.all[i,], breaks=seq(0,1,0.05), col="grey", border=NA, main="", xlab="p-value", ylim=c(0,10))
	hist(p.all[i,i.accounted], breaks=seq(0,1,0.05), col="dodgerblue", border=NA, add=T)
	if(i==1){legend("topleft", legend=c("Account for paper effect","Not account for paper effect"), fill=c("dodgerblue","grey"), bty="n", border=NA) }
}

#Plot histogram of p-value on average#
quartz(w=7,h=3.5)
par(mfrow=c(1,2),mar=c(4,4,1,1), cex=0.9)

p.account.hist = hist(p.all[,i.accounted], plot=F, breaks=seq(0,1,0.05))
p.hist = hist(p.all, plot=F, breaks=seq(0,1,0.05))
p.hist$counts = p.hist$counts/iteration
p.account.hist$counts = p.account.hist$counts/iteration
plot(p.hist, border=NA, col="grey", xlab="p-value", ylim=c(0,7), main="")
plot(p.account.hist, border=NA, col="dodgerblue", add=T)
legend("topright", legend=c("Account for paper effect","Not account for paper effect"), fill=c("dodgerblue","grey"), bty="n", border=NA)

p.paper.hist = hist(p.paper.all, plot=F, breaks=seq(0,1,0.05))
p.paper.hist$counts = p.paper.hist$counts/iteration
plot(p.paper.hist, border=NA, col="grey", ylim=c(0,7), main="Adding paper effect", xlab="p-value")








ks.p1 = array()
ks.p2 = array()
sig.prop1 = array()
sig.prop2 = array()

for(i in 1:ks.iter){
	c1 = sample(x=i.accounted, replace=T)
	c2 = sample(x=i.not.accounted, replace=T)
	p.accounted = apply(X=p.all[,c1],MARGIN=2, FUN=sample, size=1, replace=T)
	p.not.accounted = apply(X=p.all[,c2], MARGIN=2, FUN=sample, size=1, replace=T)
	ks.p1[i] = ks.test(x=p.accounted, y="punif")$p.value
	ks.p2[i] = ks.test(x=p.not.accounted, y="punif")$p.value
	sig.prop1[i] = length(which(p.accounted <= 0.05))/length(p.accounted)
	sig.prop2[i] = length(which(p.not.accounted <= 0.05))/length(p.not.accounted)
}

mean(sig.prop1)
mean(sig.prop2)

boot.p1 = boot(data=p.all[,i.accounted], statistic=SigPropCalc, R=5000)
boot.ci(boot.p1, type="norm")
boot.p2 = boot(data=p.all[,i.not.accounted], statistic=SigPropCalc, R=8000)
boot.ci(boot.p2, type="norm")

quartz(w=7,h=3.5)
par(mfrow=c(1,2), mar=c(4,4,1,1), cex=0.85)
hist(ks.p1, breaks=seq(0,1,0.05), main="Account for", ylim=c(0,2500), xlab="p-value testing uniform distribution")
hist(ks.p2, breaks=seq(0,1,0.05), main="Not account for", ylim=c(0,2500), xlab="p-value testing uniform distribution")

quartz(w=7,h=3.5)
par(mfrow=c(1,2), mar=c(4,4,1,1), cex=0.85)
hist(sig.prop1, breaks=seq(0,1,0.05), main="Account for", ylim=c(0,2500), xlab="% significant")
hist(sig.prop2, breaks=seq(0,1,0.05), main="Not account for", ylim=c(0,2500), xlab="% significant")



#P-values for the original comparison with and without adding a random paper effect#
quartz(w=8,h=4)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(p.original.paper.all~p.original.all, xlim=c(0,1), ylim=c(0,1), xlab="Original p-values", ylab="P-values with a paper effect", pch=19)
points(p.original.paper.all[i.accounted]~p.original.all[i.accounted], col="dodgerblue", pch=19)
abline(0,1, lty=2, col="grey")
abline(h=0.05, lty=2, col="grey")
abline(v=0.05, lty=2, col="grey")
legend(x=0.07,y=1.05, legend=c("Account for paper effect","Not account for paper effect"), pch=19, col=c("dodgerblue", "grey"), bty="n", cex=0.85)
plot(p.original.paper.all~p.original.all, xlim=c(0,0.05), ylim=c(0,0.05), xlab="Original p-values", ylab="P-values with a paper effect", pch=19)
points(p.original.paper.all[i.accounted]~p.original.all[i.accounted], col="dodgerblue", pch=19)
abline(0,1, lty=2, col="grey")


#Proportion significant for each paper vs number of studies/paper#
prop.sig.each = colSums(p.all<=0.05, na.rm=T)/dim(p.all)[1]
prop.sig.paper.each = colSums(p.paper.all<=0.05, na.rm=T)/dim(p.all)[1]
study.per.paper = c(2.7, 6.01, 6.7, 3.06, 17.63, 2.02, 3.06, 7.17, 3.63, 2.57, 6.54, 5.66, 3.83, 6.33, 8.42, 2.11, 3.57, 3.54, 3.31, 10.48)
paper.num = c("1180", "1185", "120", "1307", "1365", "2", "272", "282", "303", "424", "511", "53", "585", "628", "639", "674", "695", "759", "82", "952")


quartz(w=4.5,h=4.5)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(prop.sig.each[i.not.accounted]~study.per.paper[i.not.accounted], ylab="Proportion significant", xlab="Study/paper", pch=19, ylim=c(0,1), col="grey")
points(prop.sig.each[i.accounted]~study.per.paper[i.accounted], col="dodgerblue", pch=19)
legend("topright", legend=c("Account for paper effect","Not account for paper effect"), pch=19, col=c("dodgerblue", "grey"), bty="n", cex=0.85)


