#Distribution of number of studies per paper in meta-analysis#
data = read.csv(file="StudyNo_Distribution.csv", header=T)

mu.study = array()
var.study = array()
size.study = array()
#Negative binomial distribution fitted for each meta-analysis data#
quartz(w=8,h=8)
par(mfrow=c(4,4), mar=c(2,2,1,1), oma=c(3,3,1,1))
for(i in 2:dim(data)[2]){
	study.num.raw = rep(data$No_studies, data[,i])
	mu.study[i] = mean(study.num.raw)-1
	var.study[i] = var(study.num.raw)
	size.study[i] = mu.study[i]^2/(var.study[i]-mu.study[i])
	empirical.prob = data[,i]/sum(data[,i])
	plot(empirical.prob~data$No_studies, type="h", lwd=5, col="gray")
	fitted.prob = dnbinom(x=0:80, size=size.study[i], mu=mu.study[i])
	lines(fitted.prob~c(1:81), pch=19, col="darkgreen", lwd=2)
	mtext(text=paste0("Mean: ",round(mu.study[i]+1,1)),side=3, line=-1.5, cex=0.85, adj=0.95)
	mtext(text=paste0("SD: ", round(sqrt(var.study[i]),1)), side=3, line=-2.7, cex=0.85, adj=0.95)
	
}
mtext("Number of studies per paper", outer=T, side=1, line=1)
mtext("Probability", side=2, outer=T, line=1)

#Distribution of number of papers combining all meta-analysis#
count.total = rowSums(data[,-1])
study.num.raw.all = rep(data$No_studies, count.total)

#Using the mean and size parameterization of negative binomial distribution#
#variance=mean+mean^2/size#
mu.study.all = mean(study.num.raw.all)-1
var.study.all = var(study.num.raw.all)
size.study.all = mu.study.all^2/(var.study.all-mu.study.all)

#Number of papers per meta-analysis#
mean(colSums(data)[-1])
median(colSums(data)[-1])

#Plot empirical and fitted distribution of number of studies per paper#
empirical.prob = count.total/sum(count.total)
plot(empirical.prob~data$No_studies, xlab="Number of studies per paper", ylab="Probability", type="h", lwd=5, ylim=c(0,0.45), col="gray")
fitted.prob = dnbinom(x=0:80, size=size.study.all, mu=mu.study.all)
lines(fitted.prob~c(1:81), pch=19, col="red", lwd=2)
mtext(text=paste0("Mean: ",round(mu.study.all+1,1)),side=3, line=-1.5, cex=0.85, adj=0.95)
mtext(text=paste0("Var: ", round(var.study.all,1)), side=3, line=-2.7, cex=0.85, adj=0.95)
#legend("topright", legend=c("Observed","fitted"), lty=c(1,1), lwd=c(5,5), col=c("gray","darkgreen"), bty="n")

#Relationship between mean and variance of number of studies per paper#

#Mean and variance of number of studies used in simulation#
mod = lm(I(log(var.study))~I(log(mu.study)))
mod.pred = predict(mod, level=0.95, interval="p", newdata=data.frame(mu.study=c(0.5,4.5,14.5)))
var.pred = exp(c(mod.pred[,1]))
mu.pred = c(0.5,4.5,14.5)
size.pred = mu.pred^2/(var.pred-mu.pred)

#Print the mean and standard deviation of number of studies per paper based on linear regression#
mu.pred
sqrt(var.pred)
size.pred


quartz(w=4,h=4)
par(mfrow=c(1,1), mar=c(4,4,1,1), oma=c(0,0,0,0))
plot(I(mu.study), I(sqrt(var.study)), xlab="Mean", ylab="Standard deviation",log="xy", ylim=c(0.4,27), pch=19)
pred.ci = predict(mod, level=0.95, interval="p", newdata=data.frame(mu.study=seq(0.1,16,0.1)))
#lines(sqrt(exp(pred.ci[,3]))~seq(0.1,16,0.1), lty=2, col="grey", lwd=1.5)
#lines(sqrt(exp(pred.ci[,2]))~seq(.1,16,0.1), lty=2, col="grey", lwd=1.5)
lines(sqrt(exp(pred.ci[,1]))~seq(.1,16,0.1), lty=1, col="grey", lwd=1.5)
