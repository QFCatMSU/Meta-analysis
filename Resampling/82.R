library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="82.xlsx"))

#Create numeric ID for each paper based on author and publication year#
data$Ref_ID = as.numeric(factor(paste0(data$Author,data$Year)))

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Ref_ID, System))
data$study = 1:dim(data)[1]

#Reproduce the most significant comparison in the original meta-analysis#
#Comparison: AOB response ratio across cropping system#
mod = rma(yi=RR_AOB, vi=v_AOB, mods=~factor(System), data=data, method="HE")
mod.p = rma.mv(yi=RR_AOB, V=v_AOB, mods=~factor(System), random=list(~1|Ref_ID,~1|study), data=data)
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
	mod.i = rma(yi=RR_AOB, vi=v_AOB, mods=~factor(System)+factor(group), data=data.i, method="HE", btt=c(3))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=RR_AOB, V=v_AOB, mods=~factor(System)+factor(group), random=list(~1|Ref_ID,~1|study), data=data.i, btt=c(3))
	p.paper[i] = mod.i.paper$QMp
	
	
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
