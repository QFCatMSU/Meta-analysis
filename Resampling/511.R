library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="511.xlsx"))

#Create numeric ID for each paper based on author and year#
data$Ref_ID = with(data, paste0(Author,Year))

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Ref_ID, Plant))

#Reproduce the most significant comparison in the original meta-analysis#
#Emergence vs Plant source in table 1#
mod = rma(yi=d, vi=var.d, mods=~factor(Plant), method="FE", data=data)
mod.p = rma.mv(yi=d, V=var.d, mods=~factor(Plant), random=list(~1|Ref_ID), data=data)
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
	mod.i = try(rma(yi=d, vi=var.d, mods=~factor(Plant)+factor(group), method="FE", data=data.i, btt=c(12)),silent=T)
	p[i] = as.numeric(try(mod.i$QMp, silent=T))
	mod.i.paper = try(rma.mv(yi=d, V=var.d, mods=~factor(Plant)+factor(group), random=list(~1|Ref_ID), data=data.i, btt=c(12)), silent=T)
	p.paper[i] = as.numeric(try(mod.i.paper$QMp, silent=T))
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
