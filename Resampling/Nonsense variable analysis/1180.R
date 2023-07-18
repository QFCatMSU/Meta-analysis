library(readxl)
library(metafor)
library(tidyverse)

data = as.data.frame(read_excel(path="1180.xlsx"))

data$block = with(data, paste0(Reference,trophic))
data$study = 1:dim(data)[1]


mod = rma(yi=d, vi=var.d, mods=~factor(trophic), data=data, method="REML")
mod.p = rma.mv(yi=d, V=var.d, mods=~factor(trophic), random=list(~1|Reference,~1|study), data=data)
p.original = mod$QMp
p.original.paper = mod.p$QMp


#Nonsese variable analysis#
p.ns = vector()
p.paper.ns = vector()

#Four nonsese variable#
#Odd or even year; Odd or even number of letters in author name#
#First letter of name before or after M; Percentage of vowels lower or higher than 33%#
year.parity = as.factor(as.numeric(data$Year) %% 2 == 0)
name.parity = as.factor(str_count(data$Author) %% 2 == 0)
name.rank = as.factor(substr(data$Author,1,1) %in% LETTERS[1:13])
name.vowels = as.factor(str_count(data$Author, "[AEIOUaeiou]")/str_length(data$Author) < 0.33)

mod.ns1 = rma(yi=d, vi=var.d, mods=~year.parity+factor(trophic), data=data, method="REML", btt=2)
mod.ns2 = rma(yi=d, vi=var.d, mods=~name.parity+factor(trophic), data=data, method="REML", btt=2)
mod.ns3 = rma(yi=d, vi=var.d, mods=~name.rank+factor(trophic), data=data, method="REML", btt=2)
mod.ns4 = rma(yi=d, vi=var.d, mods=~name.vowels+factor(trophic), data=data, method="REML", btt=2)

p.ns = c(mod.ns1$QMp, mod.ns2$QMp, mod.ns3$QMp, mod.ns4$QMp)

mod.p.ns1 = rma.mv(yi=d, V=var.d, mods=~year.parity+factor(trophic), random=list(~1|Reference,~1|study), data=data, btt=2)
mod.p.ns2 = rma.mv(yi=d, V=var.d, mods=~name.parity+factor(trophic), random=list(~1|Reference,~1|study), data=data, btt=2)
mod.p.ns3 = rma.mv(yi=d, V=var.d, mods=~name.rank+factor(trophic), random=list(~1|Reference,~1|study), data=data, btt=2)
mod.p.ns4 = rma.mv(yi=d, V=var.d, mods=~name.vowels+factor(trophic), random=list(~1|Reference,~1|study), data=data, btt=2)

p.paper.ns = c(mod.p.ns1$QMp, mod.p.ns2$QMp, mod.p.ns3$QMp, mod.p.ns4$QMp)



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