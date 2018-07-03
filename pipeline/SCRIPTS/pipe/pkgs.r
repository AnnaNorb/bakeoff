##########################################################################################
# PACKAGES
##########################################################################################

install.packages(c("pROC","mvabund","randomForest","caret","e1071","gbm","dismo","yaImpute",
					"earth","devtools","glmnet","boral","gjam","spaMM","nlme","MASS","spaMM",
					"vegan","BayesComm","mvtnorm","parallel"))
require(devtools)
install_github("davharris/mistnet2")
install_github('BayesComm', 'goldingn')

if (OS=="osx"|OS=="unix") { 
	install.packages("doMC")
	install.packages(paste(WD,"MODELS/mvpart_pkg/mvpart_1.6-2.tar",sep=''), repos = NULL, type="source") 
	}
if (OS=="win") {
	install.packages("doParallel")
	install.packages(paste(WD,"MODELS/mvpart_pkg/mvpart_1.6-2.zip",sep=''), repos = NULL, type="source")
	}


##########################################################################################
