library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="1307.xlsx"))
data$richess.category = data$richness
data$richness.category[data$richness != "Single"] = "Mixture"
data$richness.category[data$richness == "Single"] = "Single"
data$richness.category[is.na(data$richness)] = "Mixture"
data$block = with(data, paste0(author, year, richness.category))

data$paper = with(data, paste0(author,year))
data$study = 1:dim(data)[1]


mod = rma(yi=lnR, vi=vn, mods=~factor(richness.category), data=data, method="DL")
mod.p = rma.mv(yi=lnR, V=vn, mods=~factor(richness.category), random=list(~1|paper, ~1|study), data=data, method="ML")
p.original = mod$QMp
p.original.paper = mod.p$QMp

n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = try(rma(yi=lnR, vi=vn, mods=~factor(richness.category)+factor(group), data=data.i, btt=c(3)),silent=T)
	p[i] = as.numeric(try(mod.i$QMp,silent=T))
	mod.i.paper = try(rma.mv(yi=lnR, V=vn, mods=~factor(richness.category)+factor(group), random=list(~1|paper, ~1|study), data=data.i, method="ML"),silent=T)
	p.paper[i] = as.numeric(try(mod.i.paper$QMp,silent=T))
}

length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))