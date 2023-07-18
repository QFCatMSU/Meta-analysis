library(metafor)
library(tidyverse)

data = read.csv("628.csv", header=T)


#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(StudyID, PoolType))

data = escalc(measure="ROM", m1i=MeanTreatment, m2i=MeanControl, sd1i=SDTreatment, sd2i=SDControl, n1i=Ntreatment, n2i=Ncontrol, data=data)

#Reproduce the most significant comparison in the original meta-analysis#
#Emergence vs Plant source in table 1#
mod = rma.mv(yi=yi, V=vi, mods=~factor(PoolType), random=list(~1|StudyID,~1|StudyID/Location), method="REML", data=data)
p.original = mod$QMp
p.original.paper = p.original


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

mod.ns1 = rma.mv(yi=yi, V=vi, mods=~year.parity+factor(PoolType), random=list(~1|StudyID,~1|StudyID/Location), method="REML", data=data, btt=2)
mod.ns2 = rma.mv(yi=yi, V=vi, mods=~name.parity+factor(PoolType), random=list(~1|StudyID,~1|StudyID/Location), method="REML", data=data, btt=2)
mod.ns3 = rma.mv(yi=yi, V=vi, mods=~name.rank+factor(PoolType), random=list(~1|StudyID,~1|StudyID/Location), method="REML", data=data, btt=2)
mod.ns4 = rma.mv(yi=yi, V=vi, mods=~name.vowels+factor(PoolType), random=list(~1|StudyID,~1|StudyID/Location), method="REML", data=data, btt=2)
p.ns = c(mod.ns1$QMp, mod.ns2$QMp, mod.ns3$QMp, mod.ns4$QMp)
p.paper.ns = p.ns


n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2,sample(c(1,2), size=n.block-2, replace=T)), replace=F))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = try(rma.mv(yi=yi, V=vi, mods=~factor(PoolType)+factor(group), random=list(~1|StudyID,~1|StudyID/Location), method="REML", data=data.i, btt=c(3)), silent=T)
	p[i] = as.numeric(try(mod.i$QMp, silent=T))
	p.paper[i] = p[i]
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
