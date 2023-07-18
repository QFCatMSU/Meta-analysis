library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="1365.xlsx"))
data$block = with(data, paste0(date,author,category))

data$paper = with(data, paste0(date,author))
data$study = 1:dim(data)[1]

mod = rma(yi=y, vi=v, mods=~concentration+duration+factor(category), data=data, btt=c(4,5))
mod.p = rma.mv(yi=y, V=v, mods=~concentration+duration+factor(category), data=data, random=list(~1|paper, ~1|study), btt=c(4,5))
p.original = mod$QMp
p.original.paper = mod.p$QMp

n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=y, vi=v, mods=~concentration+duration+factor(category)+factor(group), data=data.i, btt=c(6))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=y, V=v, mods=~concentration+duration+factor(category)+factor(group), data=data.i, btt=c(6), random=list(~1|paper, ~1|study))
	p.paper[i] = mod.i.paper$QMp
	
}

length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))