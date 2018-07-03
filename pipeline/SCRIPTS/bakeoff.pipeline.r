##########################################################################################
# BAKEOFF SDM COMPARISON PIPELINE
##########################################################################################


# preliminaries
##########################################################################################
rm(list = ls(all=TRUE)); gc()	# clear workspace

pth <- "..." 	# write here the path to the location of the 'bakeoff' folder
#pth <- "~/OneDrive - University of Helsinki" 
#pth <- "D:/HY-data/NORBERG/OneDrive - University of Helsinki" 

SETT <- file.path(pth,"bakeoff","pipeline","SCRIPTS","settings.r")	# path to settings
source(SETT)	# run settings
setwd(WD)		# set working directory to the pipeline folder

source(file.path(SD,"pipe", "get_os.r"))		# identify your OS
source(file.path(SD,"pipe", "pkgs.r"))		# install required packages, 
source(file.path(SD,"pipe", "dirs.r"))		# create directories (if don't exist),
source(file.path(SD,"pipe", "parall.r"))		# and identify your OS and define settings 
											# for parallel computing

# model fitting & predictions
##########################################################################################

MCMC2 <- FALSE
sz <- 1
#sz <- 2
#sz <- 3

for (d in 1:length(Sets)) {

	set_no <- Sets[d]
	source(readdata)
	source(fitmodels)
	source(makepreds)
	
}

# performance measures (PM)
##########################################################################################

ENS<-list(NULL, unlist(mod_names),c("HMSC3", "GLM5", "MISTN1", "GNN1", "MARS1"))
PRV<-list(NA, 0.1)

source(modpreds)
source(pms)
source(pms_comb)
source(pms_tbl)
source(pms_plot)

# computation times (compile and plot)
##########################################################################################

source(compt_times)

##########################################################################################




