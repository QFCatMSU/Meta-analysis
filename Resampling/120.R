library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="120.xlsx"))

#Delete empty rows#
data = with(data, data[!is.na(Cm) & !is.na(Pyrolysis_temp),])

#Calculate log response ratio and its variance#
data$RR = with(data, log(Tm/Cm))
data$var.RR = with(data, Csd^2/Cn/Cm^2+Tsd^2/Tn/Tm^2)

#Create numeric ID for each paper#
data$Ref_ID = as.numeric(factor(data$References))
data$study = 1:dim(data)[1]

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Ref_ID, Pyrolysis_temp))

#Reproduce the most significant comparison in the original meta-analysis#
#Comparison: RR of bacterium across categories of pyrolysis temperature#
mod = rma(yi=RR, vi=var.RR, mods=~factor(Pyrolysis_temp), data=data, method="DL")
mod.p = rma.mv(yi=RR, V=var.RR, mods=~factor(Pyrolysis_temp), random=list(~1|study,~1|Ref_ID), data=data)
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
	mod.i = rma(yi=RR, vi=var.RR, mods=~factor(Pyrolysis_temp)+factor(group), data=data.i, method="DL", btt=c(4))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=RR, V=var.RR, mods=~factor(Pyrolysis_temp)+factor(group), random=list(~1|study,~1|Ref_ID), btt=c(4), data=data.i)
	p.paper[i] = mod.i.paper$QMp
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
