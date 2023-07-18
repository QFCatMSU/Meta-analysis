library(RhpcBLASctl)

#Set the algebra routine to use one core#
blas_set_num_threads(1)

#File names of script for each paper. Scipt for paper is name as number.R#
code.all = list.files(pattern="[[:digit:]].R$")

iteration = 1

#p.all store p-value for comparing random groups#
#p.paper/all store p-value for comparing random groups when the model contains a random paper effect#
p.all = array(dim=c(iteration, length(code.all)))
p.paper.all = array(dim=c(iteration, length(code.all)))
#p.original.all stores p-values for the original group comparison#
#p.original.paper.all stores p-value for the orginal group comparison after adding random paper effect #
p.original.all = array()
p.original.paper.all = array()
p.ns.all = array(dim=c(length(code.all),4))
p.paper.ns.all = array(dim=c(length(code.all),4))

set.seed(5)
for(r in 1:length(code.all)){
	source(code.all[r])
	p.all[,r] = p
	p.paper.all[,r] = p.paper
	p.original.all[r] = p.original
	p.original.paper.all[r] = p.original.paper
	p.ns.all[r,] = p.ns
	p.paper.ns.all[r,] = p.paper.ns
	print(r)
}

save.image(file="Compile.RData")