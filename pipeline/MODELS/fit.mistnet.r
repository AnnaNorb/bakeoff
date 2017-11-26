##########################################################################################
# STOCHASTIC FEED-FORWARD NEURAL NETWORK
##########################################################################################

require(mistnet2)

##########################################################################################

for (j in 1:3) {

	X <- x_train[[j]][,-1]
	Xs <- cbind(X,s_train[[j]])
	Y <- y_train[[j]]


	if (j==1) { sT<-Sys.time() }

	mstnt <- mistnet(x=X, y=Y,
	    n_z = 2,
	    layers = list(
	        layer(
	            activator = sigmoid_activator,
	            n_nodes = ncol(X)+2+1,
	            weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
	        ),
	        layer(
	            activator = sigmoid_activator,
   		        n_nodes = ncol(X)+2+1,
        	    weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        	),
        	layer(
        	    activator = sigmoid_activator,
        	    n_nodes = ncol(Y),
           	 	weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        	)
    	),
   		error_distribution = make_distribution("BI",bd=1)
	)

	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}

save(mstnt, file=paste(FD,set_no,"/mstnt_",j,"_",dataN[sz],".RData",sep=""))
if (j==1) {
save(comTimes, file=paste(FD,set_no,"/comTimes_MISTN1_",dataN[sz],".RData",sep=""))
}


	if (j==1) { sT<-Sys.time() }

	mstnt2 <- mistnet(x=Xs, y=Y,
	    n_z = 2,
	    layers = list(
	        layer(
	            activator = sigmoid_activator,
	            n_nodes = ncol(Xs)+2+1,
	            weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
	        ),
	        layer(
	            activator = sigmoid_activator,
   		        n_nodes = ncol(Xs)+2+1,
        	    weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        	),
        	layer(
        	    activator = sigmoid_activator,
        	    n_nodes = ncol(Y),
           	 	weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        	)
    	),
   		error_distribution = make_distribution("BI",bd=1)
	)

	if (j==1) {
		eT<-Sys.time()
		comTimes$MISTN2<-eT-sT
		}

save(mstnt2, file=paste(FD,set_no,"/mstnt2_",j,"_",dataN[sz],".RData",sep=""))
if (j==1) {
save(comTimes, file=paste(FD,set_no,"/comTimes_MISTN2_",dataN[sz],".RData",sep=""))
}

}

##########################################################################################
