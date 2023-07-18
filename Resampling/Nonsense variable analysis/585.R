library(metafor)
library(plyr)
library(tidyverse)

data = read.csv("585.csv", header=T)
names(data)[1] = "Site"

#Calculate response ratio, sensitivity, and their variances based on microbial C#
data$RR1 = with(data, log(T.M1/C.M1))
data$var.RR1 = with(data, C.SD1^2/N1/C.M1^2+T.SD1^2/N1/T.M1^2)
data$sens1 = with(data, RR1/abs(Precip.pct/100))
data$var.sens1 = with(data, var.RR1/(Precip.pct/100)^2)
#Calculate response ratio, sensitivity, and their variances based on microbial N#
data$RR2 = with(data, log(T.M2/C.M2))
data$var.RR2 = with(data, C.SD2^2/N2/C.M2^2+T.SD2^2/N2/T.M2^2)
data$sens2 = with(data, RR2/abs(Precip.pct/100))
data$var.sens2 = with(data, var.RR2/(Precip.pct/100)^2)
#Calculate response ratio, sensitivity, and their variances based on total PLFA#
data$RR3 = with(data, log(T.M3/C.M3))
data$var.RR3 = with(data, C.SD3^2/N3/C.M3^2+T.SD3^2/N3/T.M3^2)
data$sens3 = with(data, RR3/abs(Precip.pct/100))
data$var.sens3 = with(data, var.RR3/(Precip.pct/100)^2)

#Use microbial C first, N second, and total PFLA third for sensitivity#
data$sens = data$sens1
data$sens[is.na(data$sens1)] = data$sens2[is.na(data$sens1)]
data$sens[is.na(data$sens1) & is.na(data$sens2)] = data$sens3[is.na(data$sens1) & is.na(data$sens2)]
data$var.sens = data$var.sens1
data$var.sens[is.na(data$var.sens1)] = data$var.sens2[is.na(data$var.sens1)]
data$var.sens[is.na(data$var.sens1) & is.na(data$var.sens2)] = data$var.sens3[is.na(data$var.sens1) & is.na(data$var.sens2)]


#Eliminate rows without clay percent or MAP or effect size#
data = with(data, data[!is.na(sens) & !is.na(MAP) & !is.na(Clay),])
#Define soil texture: fine or coarse using 10% clay as threshold#
data$texture = ifelse(data$Clay>10, "Fine", "Coarse")

#Comparison: coarse vs fine in arid (MAP<800) in IPPT experiment#
data = with(data, data[substr(Case.ID, 1,4)=="IPPT" & MAP<800,])

#Weighted mean sensitivity for each case study#
data.mean = plyr::ddply(data, c("Site"), summarise, texture=texture[1], mean.sens=weighted.mean(x=sens, w=1/var.sens), mean.sens.var=1/sum(1/var.sens), Year=Year[1], Author=Author[1])

#Reproduce most significant comparison#
mod = rma(yi=mean.sens, vi=mean.sens.var, mods=~factor(texture), data=data.mean, method="FE")
p.original = mod$QMp
p.original.paper = p.original


#Nonsese variable analysis#
p.ns = vector()
p.paper.ns = vector()

#Four nonsese variable#
#Odd or even year; Odd or even number of letters in author name#
#First letter of name before or after M; Percentage of vowels lower or higher than 33%#
year.parity = as.factor(as.numeric(data.mean$Year) %% 2 == 0)
name.parity = as.factor(str_count(data.mean$Author) %% 2 == 0)
name.rank = as.factor(substr(data.mean$Author,1,1) %in% LETTERS[1:13])
name.vowels = as.factor(str_count(data.mean$Author, "[AEIOUaeiou]")/str_length(data.mean$Author) < 0.33)

mod.ns1 = rma(yi=mean.sens, vi=mean.sens.var, mods=~year.parity+factor(texture), data=data.mean, method="FE", btt=2)
mod.ns2 = rma(yi=mean.sens, vi=mean.sens.var, mods=~name.parity+factor(texture), data=data.mean, method="FE", btt=2)
mod.ns3 = rma(yi=mean.sens, vi=mean.sens.var, mods=~name.rank+factor(texture), data=data.mean, method="FE", btt=2)
mod.ns4 = rma(yi=mean.sens, vi=mean.sens.var, mods=~name.vowels+factor(texture), data=data.mean, method="FE", btt=2)
p.ns = c(mod.ns1$QMp, mod.ns2$QMp, mod.ns3$QMp, mod.ns4$QMp)
p.paper.ns = p.ns



#Define block of data that belong to the same paper and same group under comparison#
data.mean$block = with(data.mean, paste0(Site, texture))


n.block = length(unique(data.mean$block))
if(!("iteration"%in%ls())){iteration = 100}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data.mean$block), group=sample(c(1,2,sample(c(1,2), size=n.block-2, replace=T))))
	data.i = merge(x=data.mean, y=block.group, by=c("block"))
	mod.i = rma(yi=mean.sens, vi=mean.sens.var, mods=~factor(group)+factor(texture), data=data.i, btt=c(2), method="FE")
	p[i] = mod.i$QMp
	p.paper[i] = p[i]
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
