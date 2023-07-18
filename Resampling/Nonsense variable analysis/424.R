library(metafor)
library(tidyverse)


data = read.csv("424.csv", header=T)

#Create a ID for each effect size to use as a random effect in rma.mv#
data$Study.ID = rep(1:dim(data)[1])
#Subset of data containing only survival senescence rate#
data = with(data, data[response=="survival",])



#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(author_year, class))

#Reproduce the most significant comparison in the original meta-analysis#
#Comparison: Fisher transformed correlation coefficient over class (bird vs animal)#
mod = rma.mv(yi=Zr_directed, V=var, random=list(~1|author_year,~1|model_code,~1|Study.ID), mods=~factor(class), data=data)
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

mod.ns1 = rma.mv(yi=Zr_directed, V=var, random=list(~1|author_year,~1|model_code,~1|Study.ID), mods=~year.parity+factor(class), data=data, btt=2)
mod.ns2 = rma.mv(yi=Zr_directed, V=var, random=list(~1|author_year,~1|model_code,~1|Study.ID), mods=~name.parity+factor(class), data=data, btt=2)
mod.ns3 = rma.mv(yi=Zr_directed, V=var, random=list(~1|author_year,~1|model_code,~1|Study.ID), mods=~name.rank+factor(class), data=data, btt=2)
mod.ns4 = rma.mv(yi=Zr_directed, V=var, random=list(~1|author_year,~1|model_code,~1|Study.ID), mods=~name.vowels+factor(class), data=data, btt=2)
p.ns = c(mod.ns1$QMp, mod.ns2$QMp, mod.ns3$QMp, mod.ns4$QMp)
p.paper.ns = p.ns


n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2,sample(c(1,2), size=n.block-2, replace=T))))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = try(rma.mv(yi=Zr_directed, V=var, mods=~factor(group)+factor(class), random=list(~1|author_year,~1|model_code,~1|Study.ID), data=data.i, btt=c(2)))
	p[i] = as.numeric(try(mod.i$QMp))
	p.paper[i] = p[i]
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
