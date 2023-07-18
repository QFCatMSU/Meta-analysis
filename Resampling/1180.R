library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="1180.xlsx"))

data$block = with(data, paste0(Reference,trophic))
data$study = 1:dim(data)[1]


mod = rma(yi=d, vi=var.d, mods=~factor(trophic), data=data, method="REML")
mod.p = rma.mv(yi=d, V=var.d, mods=~factor(trophic), random=list(~1|Reference,~1|study), data=data)
p.original = mod$QMp
p.original.paper = mod.p$QMp

n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=d, vi=var.d, mods=~factor(trophic)+factor(group), data=data.i, btt=c(7))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=d, V=var.d, mods=~factor(trophic)+factor(group), random=list(~1|Reference,~1|study), data=data.i, btt=c(7))
	p.paper[i] = mod.i.paper$QMp
}

length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))