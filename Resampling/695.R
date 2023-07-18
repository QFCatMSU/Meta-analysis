library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="695.xlsx"))

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Paper, Organism))
data$study = 1:dim(data)[1]

#Reproduce the most significant comparison in the original meta-analysis#
#Emergence vs Plant source in table 1#
mod = rma(yi=C, sei=se.C, mods=~factor(Organism), data=data)
mod.p = rma.mv(yi=C, V=se.C^2, mods=~factor(Organism), random=list(~1|Paper,~1|study), data=data)
p.original = mod$QMp
p.original.paper = mod.p$QMp



n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=C, sei=se.C, mods=~factor(Organism)+factor(group), data=data.i, btt=c(4))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=C, V=se.C^2, mods=~factor(Organism)+factor(group), random=list(~1|Paper,~1|study), data=data.i, btt=c(4))
	p.paper[i] = mod.i.paper$QMp
	
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
