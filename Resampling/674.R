library(metafor)

data = read.csv("674.csv", header=T)

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Reference, Type))
data$study= 1:dim(data)[1]

data$RR = with(data, log(PPAx/OAx))
data$RR.var = with(data, PPAvar/PPAn/PPAx^2 + OAvar/OAn/OAx^2)
data = data[data$Biological.efect == "Density",]

mod = rma(yi=RR, vi=RR.var, mods=~factor(Type), data=data)
mod.p = rma.mv(yi=RR, V=RR.var, mods=~factor(Type), random=list(~1|Reference,~1|study), data=data)
p.original = mod$QMp
p.original = mod.p$QMp


n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=RR, vi=RR.var, mods=~factor(Type)+factor(group), data=data.i, btt=c(4))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=RR, V=RR.var, mods=~factor(Type)+factor(group), random=list(~1|Reference,~1|study), data=data.i, btt=c(4))
	p.paper[i] = mod.i.paper$QMp
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
