library(metafor)

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
