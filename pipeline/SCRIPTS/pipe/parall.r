##########################################################################################
# PARALLEL COMPUTING
##########################################################################################

if (OS=="osx"|OS=="unix") { 
	require(doMC)
	registerDoMC(cores=detectCores()-1)
}
if (OS=="win") {
	require(doParallel)
	registerDoParallel(cl=makeCluster(detectCores()-1))
}

##########################################################################################
