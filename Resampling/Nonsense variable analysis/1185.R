library(readxl)
library(metafor)
library(tidyverse)

data = read.csv("1185.csv", header=T)
data = data[data$Scavenger.effect == "n" & data$Metric != "species richness",]

data$block = with(data, paste0(Study_ID, Gear.type))
data$study = 1:dim(data)[1]
data$RR = 1
data$RR.var = 1

for(i in 1:dim(data)[1]){
	if(data$Method[i] == "BA"){
		data$RR[i] = with(data, log(Mean_AftImpt[i]/Mean_BefImpt[i]))
		data$RR.var[i] = with(data, SD_AftImpt[i]^2/Mean_AftImpt[i]^2/n_AftImpt[i] + SD_BefImpt[i]^2/Mean_BefImpt[i]^2/n_BefImpt[i])
	}
	
	if(data$Method[i] == "CI"){
		data$RR[i] = with(data, log(Mean_AftImpt[i]/Mean_AftCtrl[i]))
		data$RR.var[i] = with(data, SD_AftImpt[i]^2/Mean_AftImpt[i]^2/n_AftImpt[i] + SD_AftCtrl[i]^2/Mean_AftCtrl[i]^2/n_AftCtrl[i])
	}
	
	if(data$Method[i] == "BACI"){
		data$RR[i] = with(data, log(Mean_AftImpt[i]/Mean_AftCtrl[i]/Mean_BefImpt[i]*Mean_BefCtrl[i]))
		data$RR.var[i] = with(data, SD_AftImpt[i]^2/Mean_AftImpt[i]^2/n_AftImpt[i] + SD_AftCtrl[i]^2/Mean_AftCtrl[i]^2/n_AftCtrl[i] + SD_BefImpt[i]^2/Mean_BefImpt[i]^2/n_BefImpt[i] + SD_BefCtrl[i]^2/Mean_BefCtrl[i]^2/n_BefCtrl[i])
	}
}



mod = rma(yi=I(RR/f), vi=RR.var, mods=~Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, method="REML")
mod.p = rma.mv(yi=RR/f, V=RR.var, mods=~Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, random=list(~1|Study_ID,~1|study))
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

mod.ns1 = rma(yi=I(RR/f), vi=RR.var, mods=~year.parity+Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, method="REML", btt=2)
mod.ns2 = rma(yi=I(RR/f), vi=RR.var, mods=~name.parity+Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, method="REML", btt=2)
mod.ns3 = rma(yi=I(RR/f), vi=RR.var, mods=~name.rank+Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, method="REML", btt=2)
mod.ns4 = rma(yi=I(RR/f), vi=RR.var, mods=~name.vowels+Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, method="REML", btt=2)

p.ns = c(mod.ns1$QMp, mod.ns2$QMp, mod.ns3$QMp, mod.ns4$QMp)

mod.p.ns1 = rma.mv(yi=RR/f, V=RR.var, mods=~year.parity+Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, random=list(~1|Study_ID,~1|study), btt=2)
mod.p.ns2 = rma.mv(yi=RR/f, V=RR.var, mods=~name.parity+Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, random=list(~1|Study_ID,~1|study), btt=2)
mod.p.ns3 = rma.mv(yi=RR/f, V=RR.var, mods=~name.rank+Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, random=list(~1|Study_ID,~1|study), btt=2)
mod.p.ns4 = rma.mv(yi=RR/f, V=RR.var, mods=~name.vowels+Gear.type+Gear.type*I(log(Time+1,base=2)), data=data, random=list(~1|Study_ID,~1|study), btt=2)

p.paper.ns = c(mod.p.ns1$QMp, mod.p.ns2$QMp, mod.p.ns3$QMp, mod.p.ns4$QMp)



n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=RR/f, vi=RR.var, mods=~Gear.type+Gear.type*I(log(Time+1,base=2))+factor(group), data=data.i, btt=c(8))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=RR/f, V=RR.var, mods=~Gear.type+Gear.type*I(log(Time+1,base=2))+factor(group), data=data.i, btt=c(8), random=list(~1|Study_ID,~1|study))
	p.paper[i] = mod.i.paper$QMp
}

length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))