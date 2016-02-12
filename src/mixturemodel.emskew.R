#LoadPackages <- function(package.names) {
#source("http://bioconductor.org/biocLite.R")
#for (i in 1:length(package.names)) {
#	package.name = package.names[i]
#	installed <- installed.packages()[,1]
#	if (!any(installed == package.name)) {
#		#install.packages(package.name, repos = "http://cran.r-project.org")
#		biocLite(package.name)
#	}
#}
#}

cleanup <- function()
{
    files <- list.files(all.files=TRUE)
    for (i in 1:length(files))
    {
         if(regexpr(paste(".zip","$",sep=""), tolower(files[[i]]))[[1]] == -1
            && tolower(files[[i]]) != "stderr.txt" && tolower(files[[i]]) != "cmd.out"
            && tolower(files[[i]]) != "stdout.txt"
            && tolower(files[[i]]) != ".epilogue.pbs"
            && tolower(files[[i]]) != "command.pbs"
            && tolower(files[[i]]) != ".command.pbs"
            && tolower(files[[i]]) != ".epilogue.sh"
)
        {
            file.remove(files[[i]])
        }
    }
}

parseCmdLine <- function(...)
{
    suppressMessages(mixtureModel(...))
}

mixtureModel <- function(
libdir, #full pathname to where FLAME program files are kept
preprocessedData, #full pathname to preprocessed data in .zip
g.min=2,
g.max=20,
density, #{normal, t, skewn, skewt}
channels.to.cluster,
seed =123456, #random seed
mode.estimation = "F",
step = 0.5,
output.prefix #<studyname_dataname>
){

zip.ext <- regexpr(paste(".zip","$",sep=""), tolower(preprocessedData))
if(zip.ext[[1]] == -1)
{
    stop("Input file must be of type zip ")
}

source(paste(libdir,"common.R",sep='/'))
source(paste(libdir,"unzip.R",sep='/'))
source(paste(libdir,"appcode.emskew.R",sep='/'))
source(paste(libdir,"EmSkew.R",sep='/'))
source(paste(libdir,"run.mixture.model.R",sep='/'))
source(paste(libdir,"pairplot.clusters.R",sep='/'))
source(paste(libdir,"create.param.R",sep='/'))
source(paste(libdir,"plot.heatmap.R",sep='/'))
source(paste(libdir,"zip.R",sep='/'))

if(libdir!='')
{
    setLibPath(libdir)
	install.required.packages(libdir)
}

wkdir <- getwd()
setwd(libdir)
OS <- Sys.info()[["sysname"]]
	if (OS == "Windows") {
		exe = 'emskew.dll'
	} else {
		exe = 'emskew.so'
}
file.copy(exe, to = wkdir)
setwd(wkdir)

#mixtureModel.packages <- function() {
#	packages <- c("gplots")
#	LoadPackages(packages)
#}

#unzip preprocessed data
#preprocessedData <- unzip.file(preprocessedData, getwd())@extracted

on.exit(cleanup())

temp.dir <- paste(wkdir, "temp", sep="/")
dir.create(temp.dir)
unzip.file(preprocessedData, temp.dir)
temp.files <<- list.files(temp.dir)
unlink(temp.dir, recursive = TRUE)

unzip.file(preprocessedData, wkdir)
preprocessedData <- temp.files

g.min <- as.numeric(g.min)
g.max <- as.numeric(g.max)
g.range <- g.min:g.max

# remove trailing and leading spaces
channels.to.cluster <- gsub("^[ \t]+|[ \t]+$", "", channels.to.cluster)
                            
# remove spaces directly before or after a comma
channels.to.cluster <- gsub(" *, *", ",", channels.to.cluster)

channels.to.cluster <- strsplit(channels.to.cluster,',')[[1]]

seed <- as.integer(seed)

num.channels.to.cluster = length(channels.to.cluster)

if(Sys.getenv("R_LIBS") != '')
{
    setLibPath(c(Sys.getenv("R_LIBS"), .libPaths()))
}

#run mixture model
dist = switch(density, "normal" = "mvn", "t" = "mvt", "skewn" = "msn","skewt" = "mst")
runMixtureModel(dist=dist,
g=g.range,
dim2cluster=channels.to.cluster,
seed = seed,
mode.estimation = mode.estimation,
step = step)


###############################
### PLOT 2D FITTED CLUSTERS ###
###############################
concatfiles <- dir("./", pattern = "membership.txt")
pairplotClusters2D(concatfiles, dim = num.channels.to.cluster, g.max)

##########################
### CREATE PARAMETERES ###
##########################
makeParamFiles(retfiles = dir("./",pattern=".ret"),
dim = num.channels.to.cluster,
dist = dist)

####################
### PLOT HEATMAP ###
####################
memberfiles <- dir("./",pattern = ".membership")
paramfiles <- dir("./",pattern = ".parameters")
plotHeatmap(paramfiles = paramfiles,
concatfiles = memberfiles,
dist = dist)

#record fitting specs
specs <- c()
specs$g.range = g.range
specs$dist = dist
specs$dim = num.channels.to.cluster
dput(specs, "mixtureModelSpecs.ret")

#file.remove("emmix.dll")
#file.remove("emskew.dll")
#file.remove("emmix.so")


#zip up all files
zip.file(libdir = libdir,files = " *.locations.txt",outfile = paste(output.prefix,"MixtureModel.zip",sep='.'))
zip.file(libdir = libdir,files = " *.parameters.txt",outfile = paste(output.prefix,"MixtureModel.zip",sep='.'))
zip.file(libdir = libdir,files = " *.membership.txt",outfile = paste(output.prefix,"MixtureModel.zip",sep='.'))
zip.file(libdir = libdir,files = " *.png",outfile = paste(output.prefix,"MixtureModel.zip",sep='.'))
zip.file(libdir = libdir,files = " *.ret",outfile = paste(output.prefix,"MixtureModel.zip",sep='.'))
}

install.required.packages <- function(libdir)
{
    if(!is.package.installed(libdir, "gtools"))
	{
		install.package(libdir, "gtools_2.5.0.zip", "gtools_2.5.0.tgz", "gtools_2.5.0.tar.gz")
	}
    if(!is.package.installed(libdir, "gdata"))
	{
		install.package(libdir, "gdata_2.4.2.zip", "gdata_2.4.2.tgz", "gdata_2.4.2.tar.gz")
	}
    if(!is.package.installed(libdir, "gplots"))
	{
		install.package(libdir, "gplots_2.6.0.zip", "gplots_2.6.0.tgz", "gplots_2.6.0.tar.gz")
	}
}