library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="952.xlsx"))

data$block = with(data, paste0(study,competition))
data = data[data$code==1,]

data = escalc(measure="ROM", m1i=yCnF, m2i=nCnF, sd1i=yCnF_sd, sd2i=nCnF_sd, n1i=N, n2i=N, data=data)
data = data[!is.na(data$yi),]

data$comp_trt = with(data, paste0(competition,location))

mod = rma.mv(yi=yi, V=vi, mods=~factor(comp_trt), random=list(~1|id, ~1|focal), data=data, method="REML")
mod.p = rma.mv(yi=yi, V=vi, mods=~factor(comp_trt), random=list(~1|focal, ~1|id,~1|study), data=data)
p.original = mod$QMp
p.original.paper = mod.p$QMp

n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 100}
p = array(dim=iteration)
p.paper = array(dim=iteration)


for(i in 1:iteration){
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma.mv(yi=yi, V=vi, mods=~factor(comp_trt)+factor(group), random=list(~1|focal, ~1|id), data=data.i, btt=c(4))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=yi, V=vi, mods=~factor(comp_trt)+factor(group), random=list(~1|focal, ~1|id,~1|study), data=data.i, btt=c(4))
	p.paper[i] = mod.i.paper$QMp
	
}

length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))