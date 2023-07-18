library(readxl)
library(lme4)
library(tidyverse)

data = as.data.frame(read_excel(path="272.xlsx"))

#Create numeric ID for each paper#
data$Ref_ID = as.numeric(factor(data$Citation))
data$study = 1:dim(data)[1]

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(Ref_ID, Invader_type))

#Reproduce the most significant comparison in the original meta-analysis#
#Comparison: RR of bacterium across categories of pyrolysis temperature#
mod1 = lmer(RR~factor(Habitat_type)+factor(Invader_type)+factor(Carbon_pool)+(1|Citation), REML=F, data, weights=1/var.RR)
mod2 = lmer(RR~factor(Habitat_type)+factor(Carbon_pool)+(1|Citation), REML=F, data, weights=1/var.RR)
p.original = anova(mod1, mod2)[2,8]
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

mod.ns1 = lmer(RR~year.parity+factor(Habitat_type)+factor(Invader_type)+factor(Carbon_pool)+(1|Citation), REML=F, data, weights=1/var.RR)
mod.ns2 = lmer(RR~name.parity+factor(Habitat_type)+factor(Invader_type)+factor(Carbon_pool)+(1|Citation), REML=F, data, weights=1/var.RR)
mod.ns3 = lmer(RR~name.rank+factor(Habitat_type)+factor(Invader_type)+factor(Carbon_pool)+(1|Citation), REML=F, data, weights=1/var.RR)
mod.ns4 = lmer(RR~name.vowels+factor(Habitat_type)+factor(Invader_type)+factor(Carbon_pool)+(1|Citation), REML=F, data, weights=1/var.RR)
p.ns = c(anova(mod.ns1,mod1)[2,8], anova(mod.ns2,mod1)[2,8], anova(mod.ns3,mod1)[2,8], anova(mod.ns4,mod1)[2,8])

p.paper.ns = p.ns





n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod1.i = lmer(RR~factor(Habitat_type)+factor(Invader_type)+factor(Carbon_pool)+factor(group)+(1|Citation), REML=F, data.i, weights=1/var.RR)
	mod2.i = lmer(RR~factor(Habitat_type)+factor(Carbon_pool)+factor(Invader_type)+(1|Citation), REML=F, data.i, weights=1/var.RR)
anova(mod1, mod2)
	p[i] = anova(mod1.i, mod2.i)[2,8]
	p.paper[i] = p[i]
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
