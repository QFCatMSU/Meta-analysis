library(metafor)

#The code below is based on email communication with the authors and their Rmd document#
data = read.csv("282.csv", header=T)

#Calculate Hedges d based on unbiased estimator of the pooled sd #
es = escalc(measure="SMDH", data=data, m1i=M.treatment, sd1i=SD.treatment, n1i=N.treatment, m2i=M.control, sd2i=SD.control, n2i=N.control, append=T, vtype="UB")

#Change sign of d in certain category so they measure performance in the same direction as others#
es$yi[which(es$Insect.response.type %in% c("developmental_time", "mortality", "juvenile_development"))] = -1*es$yi[which(es$Insect.response.type %in% c("developmental_time", "mortality", "juvenile_development"))]

#Drop outliers and lines where effect size cannot be calculated#
es = droplevels(es[es$yi>-100,])
es = es[-which(is.na(es$yi)==T),]

#Sort the dataset to adjust variance-covariance calculation based on shared control#
es = es[order(es$Mult.comp),]

#Calculate sum of sample size to determine if a study used shared control#
es$N.uncorrected = unlist(lapply(split(es, es$Mult.comp), function(x) rep(sum(x$N.treatment)+sum(x$N.control), each=nrow(x))))
es$Ni = unlist(lapply(split(es, es$Mult.comp), function(x) rep(sum(x$N.treatment)+x$N.control[1], each=nrow(x))))

#Re-calculate variance covariance when having shared control#
es$vi2 = ifelse(es$Ni==es$N.uncorrected, es$vi, ((1/es$N.treatment) + (1/es$N.control) + (es$yi)^2 / (2*es$Ni)))
es$vi = es$vi2
es = es[,-which(names(es) %in% c("vi2"))]

#Define function that calculates covariance matrix when effect sizes share common control#
calc.v = function(x) {
   v = matrix(1/x$N.control[1] + outer(x$yi, x$yi, "*")/(2*x$Ni[1]), nrow=nrow(x), ncol=nrow(x))
   diag(v) = x$vi
   v
}
es = es[order(es$Mult.comp),]
V = bldiag(lapply(split(es, es$Mult.comp), calc.v))

#Subset of data with both attackers being herbivore#
es$First.attacker.pooled = as.factor(ifelse(es$Inductor.organism == "herbivore", "herbivore", "pathogen"))
es$Second.attacker.pooled = as.factor(ifelse(es$Organism == "herbivore", "herbivore", "pathogen"))
x = which(es$Second.attacker.pooled == "herbivore" & es$First.attacker.pooled=="herbivore")
m.scale <- rma.mv(yi ~ Pathway * Type.of.interaction, V = V[x,x], random = ~factor(Case.ID)|factor(Study.ID), data=  droplevels(es[x,]), btt=c(2:4))
p.original = m.scale$QMp
p.original.paper = p.original



es$block = with(es, paste0(Study.ID, Type.of.interaction))
n.block = length(unique(es$block))
if(!("iteration"%in%ls())){iteration = 1000}
p = array(dim=iteration)
p.paper = array(dim=iteration)

for(i in 1:iteration){
	block.group = data.frame(block=unique(es$block), group=sample(c(1,2), size=n.block, replace=T))
	data.i = merge(x=es, y=block.group, by=c("block"))
	mod.i = rma.mv(yi ~ Pathway * Type.of.interaction + factor(group), V = V[x,x], random = ~factor(Case.ID)|factor(Study.ID), data=  droplevels(data.i[x,]), btt=6)
	p[i] = mod.i$QMp
	p.paper[i] = p[i]
}