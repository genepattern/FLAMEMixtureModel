runMixtureModel <- function(dist, g,dim2cluster,seed=123456,mode.estimation="F",step=0.5) {
	datafiles<-dir("./",pattern=".preprocessed.txt")
	num.samples <- length(datafiles)
	for (id in 1:num.samples) {
		cat(paste("file",id,datafiles[id]),'\n')
		callme(datafiles,id,g,dim2cluster,dist=dist,ncov = 3,seed = seed,mode.estimation=mode.estimation,step=step)
	}
}