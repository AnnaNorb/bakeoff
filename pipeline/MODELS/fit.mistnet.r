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

		mstnt1 <- mistnet(x=X, y=Y,
			n_z = 1,
			layers = list(
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

	save(mstnt1, file=file.path(FD,set_no,paste("mstnt1_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
	save(comTimes, file=file.path(FD,set_no,paste("comTimes_MISTN1_",dataN[sz],".RData",sep="")))
	}

	rm(mstnt1)
	if (j==1) { rm(comTimes) }

		if (j==1) { sT<-Sys.time() }

		mstnt2 <- mistnet(x=X, y=Y,
			n_z = 2,
			layers = list(
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

	save(mstnt2, file=file.path(FD,set_no,paste("mstnt2_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
	save(comTimes, file=file.path(FD,set_no,paste("comTimes_MISTN2_",dataN[sz],".RData",sep="")))
	}

	rm(mstnt2)
	if (j==1) { rm(comTimes) }

	rm(X)
	rm(Y)

	gc()
}

##########################################################################################
