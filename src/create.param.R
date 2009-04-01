makeParamFiles <- function(retfiles, dim, dist) {

	for (i in 1:length(retfiles)) {
		print(i)
		gfile <- dget(retfiles[i])
		g = length(gfile$pro)
		out <- c()
		
		for (k in 1:g) {
			tmp.out <- matrix(nrow = dim, ncol = (5+dim))
			tmp.out[1,1] <- gfile$pro[k]
			tmp.out[,2] <- gfile$mu[,k]
			tmp.out[1,3] <- gfile$dof[k]
			tmp.out[,4:(3+dim)] <- gfile$sigma[,,k]
			tmp.out[,(4+dim)] <- gfile$delta[,k]	
			tmp.out[,(5+dim)] <- gfile$mod[,k]
			out<-rbind(out,tmp.out)
		}
	names <- c("props", "mus", "df", paste("Var", 1:dim, sep = ""), "alpha","mod")
	fname <- paste(strsplit(retfiles[i],"\\.ret")[[1]][1],"parameters.txt",sep='.')
	write.table(out, sep = "\t", quote = F, file = fname, row.names = F, col.names = names) 
	}

}
