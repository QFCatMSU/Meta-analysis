library(readxl)
library(metafor)
library(tidyverse)

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

mod.ns1 = rma(yi=z, vi=var.z, mods=~year.parity+factor(Activity), method="DL", data=data, btt=2)
mod.ns2 = rma(yi=z, vi=var.z, mods=~name.parity+factor(Activity), method="DL", data=data, btt=2)
mod.ns3 = rma(yi=z, vi=var.z, mods=~name.rank+factor(Activity), method="DL", data=data, btt=2)
mod.ns4 = rma(yi=z, vi=var.z, mods=~name.vowels+factor(Activity), method="DL", data=data, btt=2)
p.ns = c(mod.ns1$QMp, mod.ns2$QMp, mod.ns3$QMp, mod.ns4$QMp)

mod.p.ns1 = rma.mv(yi=z, V=var.z, mods=~year.parity+factor(Activity), random=list(~1|study,~1|Paper), data=data, btt=2)
mod.p.ns2 = rma.mv(yi=z, V=var.z, mods=~name.parity+factor(Activity), random=list(~1|study,~1|Paper), data=data, btt=2)
mod.p.ns3 = rma.mv(yi=z, V=var.z, mods=~name.rank+factor(Activity), random=list(~1|study,~1|Paper), data=data,btt=2)
mod.p.ns4 = rma.mv(yi=z, V=var.z, mods=~name.vowels+factor(Activity), random=list(~1|study,~1|Paper), data=data,btt=2)

p.paper.ns = c(mod.p.ns1$QMp, mod.p.ns2$QMp, mod.p.ns3$QMp, mod.p.ns4$QMp)






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
