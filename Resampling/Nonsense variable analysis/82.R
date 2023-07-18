library(readxl)
library(metafor)
library(tidyverse)

data = as.data.frame(read_excel(path="82.xlsx"))

#Create numeric ID for each paper based on author and publication year#
data$Ref_ID = as.numeric(factor(paste0(data$Author,data$Year)))

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Ref_ID, System))
data$study = 1:dim(data)[1]

#Reproduce the most significant comparison in the original meta-analysis#
#Comparison: AOB response ratio across cropping system#
mod = rma(yi=RR_AOB, vi=v_AOB, mods=~factor(System), data=data, method="HE")
mod.p = rma.mv(yi=RR_AOB, V=v_AOB, mods=~factor(System), random=list(~1|Ref_ID,~1|study), data=data)
p.original = mod$QMp
p.original.paper = mod.p$QMp

#Nonsese variable analysis#
p.ns = vector()
p.paper.ns = vector()

#Four nonsese variable#
#Odd or even year; Odd or even number of letters in author name#
#First letter of name before or after M; Percentage of vowels lower or higher than 33%#
year.parity = as.factor(as.numeric(data$Year1) %% 2 == 0)
name.parity = as.factor(str_count(data$Author1) %% 2 == 0)
name.rank = as.factor(substr(data$Author1,1,1) %in% LETTERS[1:13])
name.vowels = as.factor(str_count(data$Author1, "[AEIOUaeiou]")/str_length(data$Author1) < 0.33)

mod.ns1 = rma(yi=RR_AOB, vi=v_AOB, mods=~year.parity+factor(System), data=data, method="HE", btt=2)
mod.ns2 = rma(yi=RR_AOB, vi=v_AOB, mods=~name.parity+factor(System), data=data, method="HE", btt=2)
mod.ns3 = rma(yi=RR_AOB, vi=v_AOB, mods=~name.rank+factor(System), data=data, method="HE", btt=2)
mod.ns4 = rma(yi=RR_AOB, vi=v_AOB, mods=~name.vowels+factor(System), data=data, method="HE", btt=2)
p.ns = c(mod.ns1$QMp, mod.ns2$QMp, mod.ns3$QMp, mod.ns4$QMp)

mod.p.ns1 = rma.mv(yi=RR_AOB, V=v_AOB, mods=~year.parity+factor(System), random=list(~1|Ref_ID,~1|study), data=data, btt=2)
mod.p.ns2 = rma.mv(yi=RR_AOB, V=v_AOB, mods=~name.parity+factor(System), random=list(~1|Ref_ID,~1|study), data=data, btt=2)
mod.p.ns3 = rma.mv(yi=RR_AOB, V=v_AOB, mods=~name.rank+factor(System), random=list(~1|Ref_ID,~1|study), data=data, btt=2)
mod.p.ns4 = rma.mv(yi=RR_AOB, V=v_AOB, mods=~name.vowels+factor(System), random=list(~1|Ref_ID,~1|study), data=data, btt=2)

p.paper.ns = c(mod.p.ns1$QMp, mod.p.ns2$QMp, mod.p.ns3$QMp, mod.p.ns4$QMp)



n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=RR_AOB, vi=v_AOB, mods=~factor(System)+factor(group), data=data.i, method="HE", btt=c(3))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=RR_AOB, V=v_AOB, mods=~factor(System)+factor(group), random=list(~1|Ref_ID,~1|study), data=data.i, btt=c(3))
	p.paper[i] = mod.i.paper$QMp
	
	
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
