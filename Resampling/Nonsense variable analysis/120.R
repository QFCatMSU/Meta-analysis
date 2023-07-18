library(readxl)
library(metafor)
library(tidyverse)

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

mod.ns1 = rma(yi=RR, vi=var.RR, mods=~year.parity+factor(Pyrolysis_temp), data=data, method="DL", btt=2)
mod.ns2 = rma(yi=RR, vi=var.RR, mods=~name.parity+factor(Pyrolysis_temp), data=data, method="DL", btt=2)
mod.ns3 = rma(yi=RR, vi=var.RR, mods=~name.rank+factor(Pyrolysis_temp), data=data, method="DL", btt=2)
mod.ns4 = rma(yi=RR, vi=var.RR, mods=~name.vowels+factor(Pyrolysis_temp), data=data, method="DL", btt=2)
p.ns = c(mod.ns1$QMp, mod.ns2$QMp, mod.ns3$QMp, mod.ns4$QMp)

mod.p.ns1 = rma.mv(yi=RR, V=var.RR, mods=~year.parity+factor(Pyrolysis_temp), random=list(~1|study,~1|Ref_ID), data=data, btt=2)
mod.p.ns2 = rma.mv(yi=RR, V=var.RR, mods=~name.parity+factor(Pyrolysis_temp), random=list(~1|study,~1|Ref_ID), data=data, btt=2)
mod.p.ns3 = rma.mv(yi=RR, V=var.RR, mods=~name.rank+factor(Pyrolysis_temp), random=list(~1|study,~1|Ref_ID), data=data, btt=2)
mod.p.ns4 = rma.mv(yi=RR, V=var.RR, mods=~name.vowels+factor(Pyrolysis_temp), random=list(~1|study,~1|Ref_ID), data=data, btt=2)

p.paper.ns = c(mod.p.ns1$QMp, mod.p.ns2$QMp, mod.p.ns3$QMp, mod.p.ns4$QMp)





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
