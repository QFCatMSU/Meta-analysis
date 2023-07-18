library(readxl)
library(metafor)

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