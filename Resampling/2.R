library(readxl)
library(metafor)

data = as.data.frame(read_excel(path="2.xlsx"))

#There are 109 effect sizes for pasture, matching table 1#
#After eliminating studies without reporting sample size, there are 95 left#
data = with(data, data[!is.na(mean_restore) & !is.na(mean_degrade) & !is.na(SD_restore) & !is.na(SD_degrade) & disturbance_type=="Pastagem" & !is.na(n_restore) & !is.na(n_degrade),])

#Hedges'g calculation based on Borenstein 2009 book as cited by the author#
data$g = with(data, (mean_restore-mean_degrade)/sqrt(((n_restore-1)*SD_restore^2+(n_degrade-1)*SD_degrade^2)/(n_restore+n_degrade-2))*(1-3/(4*(n_restore+n_degrade-2)-1)))
data$var.g = with(data, (1-3/(4*(n_restore+n_degrade-2)-1))^2*((n_restore+n_degrade)/(n_restore*n_degrade)+g^2/2/(n_restore+n_degrade)))

#Create numeric ID for paper#
data$ID = as.numeric(factor(data$reference))
data$study = 1:dim(data)[1]

#Define block of data that belong to the same paper and same group under comparison#
data$block = with(data, paste0(ID, Ecosystem_service_indicator))

#Reproduce the most significant comparison in the original meta-analysis#
#Comparison: Hedges' g in difference ecosystem service type in pasture#
mod = rma(yi=g, vi=var.g, mods=~factor(Ecosystem_service_indicator), data=data)
mod.p = rma.mv(yi=g, V=var.g, mods=~factor(Ecosystem_service_indicator), random=list(~1|ID, ~1|study), data=data)
p.original = mod$QMp
p.original.paper = mod.p$QMp

n.block = length(unique(data$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	#Assign each block randomly to one of the two groups#
	block.group = data.frame(block=unique(data$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=data, y=block.group, by=c("block"))
	mod.i = rma(yi=g, vi=var.g, mods=~factor(Ecosystem_service_indicator)+factor(group), data=data.i, btt=c(4))
	p[i] = mod.i$QMp
	mod.i.paper = rma.mv(yi=g, V=var.g, mods=~factor(Ecosystem_service_indicator)+factor(group), random=list(~1|ID,~1|study), data=data.i, btt=c(4))
	p.paper[i] = mod.i.paper$QMp
}

#Proportion of significant results#
length(which(p<=0.05))/length(p)
prop.test(x=length(which(p<=0.05)), n=length(p))
