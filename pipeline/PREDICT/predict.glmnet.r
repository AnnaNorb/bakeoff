##########################################################################################
# UNIVARIATE GENERALIZED LINEAR MODELS WITH LASSO or ELASTICNET PENALTY PREDICTION
##########################################################################################
require(glmnet)

for (j in 1:3) {

	load(file=file.path(FD2,set_no,paste("glmnet1_",j,"_",dataN[sz],".RData",sep="")))

	nsp <- length(DD_v[[j]])
	nsites <- nrow(DD_v[[j]][[1]])

	glmnet1_probs <- foreach(i=1:nsp)  %dopar% { tryCatch({ predict(glmnet1[[i]], 
																	newx = as.matrix(DD_v[[j]][[i]][,-c(1:2,ncol(DD_v[[j]][[i]])-1,ncol(DD_t[[j]][[i]]))]),
																	type="response") 
														  },
	                                                      	error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

	glmnet1_probs <- foreach(i=1:nsp)  %dopar% { tryCatch({ glmnet1_probs[[i]][,sample(1:ncol(glmnet1_probs[[i]]), 100)] },
	                                                       	error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

	nulls <- unlist(lapply(glmnet1_probs,is.null))
	if (sum(nulls) != 0) {
		for (i in 1:sum(nulls)) {
			glmnet1_probs[nulls][i] <- list(matrix(mean(DD_t[[j]][nulls][[i]][,1]), ncol = 100, nrow = nsites))
		}
	}
	
	glmnet1_PA <- foreach(i=1:nsp)  %dopar% { rbinom(glmnet1_probs[[i]], 1, glmnet1_probs[[i]]) }

	save(glmnet1_PA, 
		 file = file.path(PD2, 
		 		set_no, 
		 		paste("glmnet1_PAs_", 
		 		j, 
		 		"_", 
		 		dataN[sz], 
		 		".RData",sep="")))

	rm(glmnet1)
	rm(glmnet1_probs)
	rm(glmnet1_PA)
	gc()

}
##########################################################################################
