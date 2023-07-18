library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="759.xlsx"))

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Paper, Predator))
data$study = 1:dim(data)[1]

#Reproduce the most significant comparison in the original meta-analysis#
#Emergence vs Plant source in table 1#

#Original model#
mod = rma(yi=d, vi=var.d, mods=~factor(Predator), method="DL",data=data) 
#Add simple random paper effect to the original model#
mod.p = rma.mv(yi=d, V=var.d, mods=~factor(Predator), random=list(~1|Paper,~1|study), data=data)
p.original = mod$QMp
p.original.paper = mod.p$QMp

#Perform the resampling analysis#
n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=d, vi=var.d, mods=~factor(Predator)+factor(group), data=data.i, method="DL",btt=c(7))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=d, V=var.d, mods=~factor(Predator)+factor(group), random=list(~1|Paper,~1|study), data=data.i,btt=c(7))
	p.paper[i] = mod.i.paper$QMp
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
