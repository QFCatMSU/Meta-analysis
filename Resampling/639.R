library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="639.xlsx"))

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Paper, Activity))
data$study = 1:dim(data)[1]

#Reproduce the most significant comparison in the original meta-analysis#
#Emergence vs Plant source in table 1#
mod = rma(yi=z, vi=var.z, mods=~factor(Activity), method="DL", data=data)
p.original = mod$QMp

mod.p = rma.mv(yi=z, V=var.z, mods=~factor(Activity), random=list(~1|study,~1|Paper), data=data)
p.original.paper = mod.p$QMp


n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=z, vi=var.z, mods=~factor(Activity)+factor(group), method="DL", data=data.i, btt=c(6))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=z, V=var.z, mods=~factor(Activity)+factor(group), random=list(~1|study,~1|Paper), data=data.i, btt=c(6))
	p.paper[i] = mod.i.paper$QMp
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
