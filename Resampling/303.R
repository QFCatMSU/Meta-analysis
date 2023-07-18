library(readxl)
library(metafor)

#Data file is 303_raw2.xlsx for GR activity data#
data = as.data.frame(read_excel(path="303.xlsx"))

#Calculate srandardized mean difference and its variance#
data = escalc(measure="SMD", m1i=Mt, m2i=Mc, n1i=Nt, n2i=Nc, sd1i=SDt, sd2i=SDc, data=data, vtype="LS")

calc.v = function(x){
	v = matrix(1/x$Nc[1]+outer(x$yi,x$yi,"*")/(2*x$Ni[1]), nrow=nrow(x), ncol=nrow(x))
	diag(v) = x$vi
	v
}

Vgr = bldiag(lapply(split(data, data$ID), calc.v))

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(ID, Approach))

#Reproduce the most significant comparison in the original meta-analysis#
#Comparison: RR across organisms group#
#The authors did not perform group comparison, thus try to match heterogeneity test#
mod = rma.mv(yi=yi, V=Vgr, mods=~factor(Approach), random=list(~Approach|ID), struct="UN", data=data, method="REML")
p.original = mod$QMp
p.original.paper = p.original


n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma.mv(yi=yi, V=Vgr, mods=~factor(Approach)+factor(group), random=list(~Approach|ID), struct="UN", data=data.i, btt=c(3))
	p[i] = mod.i$QMp
	p.paper[i] = p[i]
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
