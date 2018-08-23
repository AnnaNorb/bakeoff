##########################################################################################
# FIT MODELS
##########################################################################################

if (MCMC2 == FALSE) {
	source(file.path(MD,"fit.glm.r"))		# ssGLM 1
	source(file.path(MD,"fit.glmmPQL.r"))	# ssGLM 2
	source(file.path(MD,"fit.gam.r"))		# GAM
	source(file.path(MD,"fit.mrt.r"))		# MRTs
	source(file.path(MD,"fit.rf.r"))		# RFs
	source(file.path(MD,"fit.brt.r"))		# BRT
	source(file.path(MD,"fit.svm.r"))		# SVM
	source(file.path(MD,"fit.gnn.r"))		# GNNs
	source(file.path(MD,"fit.mars.r"))		# MARS
	source(file.path(MD,"fit.mistnet.r"))  	# mistnet
	source(file.path(MD,"fit.manyglm.r"))	# MVABUND
	source(file.path(MD,"fit.sam.r"))		# SAMs
}
source(file.path(MD,"fit.gjam.r"))  	# GJAMS
source(file.path(MD,"fit.bc.r"))		# BayesComm
source(file.path(MD,"fit.boral.r"))  	# BORAL 

##########################################################################################
