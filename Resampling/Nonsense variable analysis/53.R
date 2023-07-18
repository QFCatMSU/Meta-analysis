library(readxl)
library(nlme)
library(tidyverse)

data = as.data.frame(read_excel(path="53.xlsx"))
data = with(data, data[!is.na(treatment.mean) & !is.na(control.mean) & !is.na(treatment.n) & !is.na(control.n),])
data$RR = with(data, log(treatment.mean/control.mean))

#Authors use sample size based weight due to variance not available in many papers#
study.per.paper = data.frame(table(data$ID))
names(study.per.paper) = c("ID","study.number")
data = merge(x=data, y=study.per.paper, by="ID")

#Authors state that they only used sample size based on weight#
#The paper they cite for detailed methods also down weigh each paper by the number of studies in it#
data$weight = with(data, (treatment.n*control.n/(treatment.n+control.n)))
data$block = with(data, paste0(ID, manipulation.type))

#Reproduce the most significant comparison in the original meta-analysis#
#Comparison: RR of CO2 flux across treatment type (rain addition or removal) based on table 1#
#The paper uses fixed effect model with sample size based weight#
mod = lm(RR~manipulation.type, weights=weight, data=data)
mod.p = lme(RR~manipulation.type, random=~1|ID, weights=~I(1/weight), data=data)
p.original = summary(mod)$coefficients[2,4]
p.original.paper = summary(mod.p)$tTable[2,5]

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

mod.ns1 = lm(RR~year.parity + manipulation.type, weights=weight, data=data)
mod.ns2 = lm(RR~name.parity + manipulation.type, weights=weight, data=data)
mod.ns3 = lm(RR~name.rank + manipulation.type, weights=weight, data=data)
mod.ns4 = lm(RR~name.vowels + manipulation.type, weights=weight, data=data)
p.ns = c(summary(mod.ns1)$coefficients[2,4], summary(mod.ns2)$coefficients[2,4], summary(mod.ns3)$coefficients[2,4], summary(mod.ns4)$coefficients[2,4])

mod.p.ns1 = lme(RR~year.parity + manipulation.type, random=~1|ID, weights=~I(1/weight), data=data)
mod.p.ns2 = lme(RR~name.parity + manipulation.type, random=~1|ID, weights=~I(1/weight), data=data)
mod.p.ns3 = lme(RR~name.rank + manipulation.type, random=~1|ID, weights=~I(1/weight), data=data)
mod.p.ns4 = lme(RR~name.vowels + manipulation.type, random=~1|ID, weights=~I(1/weight), data=data)

p.paper.ns = c(summary(mod.p.ns1)$tTable[2,5], summary(mod.p.ns2)$tTable[2,5], summary(mod.p.ns3)$tTable[2,5], summary(mod.p.ns4)$tTable[2,5])


n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = lm(RR~manipulation.type+factor(group), weights=weight, data=data.i)
	Q.i = summary(mod.i)$coefficients[3,3]^2
	p[i] = pchisq(q=Q.i, df=1, lower.tail=F)
	mod.i.paper = lme(RR~manipulation.type+factor(group), random=~1|ID, weights=~I(1/weight), data=data.i)
	p.paper[i] = summary(mod.i.paper)$tTable[3,5]
	
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
