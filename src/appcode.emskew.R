# Written for FACS Research Group
# By Sam Wang (kwang@maths.uq.edu.au)
# On 5 Sept, 2007.


callme<-function(datafiles,id,g=c(3:5),dim2cluster,dist="mvt",ncov=3, seed=123456, mode.estimation = "F",step = 0.5) {
	datafile<-read.table(datafiles[id],header = T, sep = "\t")
	dat<- subset(datafile, select = dim2cluster)
	dat <- as.matrix(dat)
	channel.names = colnames(dat)
	name<-paste(unlist(strsplit(datafiles[id],"\\.preprocessed."))[1])
	OS <- Sys.info()[["sysname"]]
	if (OS == "Windows") {
		exe <- 'emskew.dll'
	} else {
		exe <- 'emskew.so'
	}
	if(!is.loaded(exe))
	dyn.load(exe)
		
	for(h in g) {
		print(paste("start fitting", h, sep = ' '))
		ndist<-switch(dist,mvn=1,mvt=2,msn=3,mst=4)
		ptm <- proc.time()
		obj<-EmSkew(dat,h,ndist,ncov,seed=seed)
		obj$runtime <- proc.time() - ptm
		# output results
		
		if (obj$error == 0) {
		
		if (ndist == 3 | ndist == 4) {
			if (mode.estimation == "T") {	
				obb <-  EmSkewMOD(ndist,obj$mu,obj$sigma,step=step,obj$delta,obj$dof)
				obj$mod <- obb$modpts
			}
			if (mode.estimation == "F") { obj$mod = obj$mu }
		}
		if (ndist == 1 | ndist == 2) {
		obj$mod = obj$mu
		}
		
		#obj$ICL <- getICL(dat,h,dist,ncov=3,obj$pro,obj$mu,obj$sigma,obj$dof,obj$delta,obj$clust)
		obj$SWR <- getSWR(dat,h,obj$sigma, obj$clust, obj$tao)
 
		p<-ncol(dat)
		#obj$Maha<-mahalonobis(p,h,obj$mu,obj$sigma)

		# 1. write the result to a ret file, use next fucntion to read out
		print("write .ret")
		dput(obj,paste(name,dist,h,"ret",sep='.'))

		# 2. write the result to a txt file
		print("write membership.txt")
		write.table(cbind(dat,obj$clust), sep = "\t",paste(name,dist,h,"membership.txt",sep='.'),col.names=c(channel.names,"cluster"),row.names=F)
		} else {
		dput(obj, paste(name,dist,h,"error",sep='.'))
		}
		}

}