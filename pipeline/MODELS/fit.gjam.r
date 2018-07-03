##########################################################################################
# GENERALIZED ADDITIVE MODELS
##########################################################################################

require(gjam)

if (set_no=="birds" | set_no=="butterfly" | set_no=="plant") { 
	form<-as.formula(.~a+b+c+d+e+f+g+h+i+j)
}
if (set_no=="trees") { 
	form<-as.formula(.~a+b+c+d+e+f)
}
if (set_no=="vegetation") { 
	form<-as.formula(.~a+b+c+d+e+f+g+h)
}

if (MCMC2) {
	rl <- list(r = 2, N = 3)
	rl2 <- list(r = 2, N = 2)
	ml  	<- list(ng=100000, burnin=80000, typeNames='PA', holdoutN=0)
	ml_rl  <- list(ng=100000, burnin=80000, typeNames='PA', holdoutN=0, reductList=rl)
	ml_rl2  <- list(ng=100000, burnin=80000, typeNames='PA', holdoutN=0, reductList=rl2)
} else {
	rl <- list(r = 2, N = 3)
	rl2 <- list(r = 2, N = 2)
	ml  <- list(ng=50000, burnin=40000, typeNames='PA', holdoutN=0)
	ml_rl  	<- list(ng=50000, burnin=40000, typeNames='PA', holdoutN=0, reductList=rl)
	ml_rl2  <- list(ng=50000, burnin=40000, typeNames='PA', holdoutN=0, reductList=rl2)
}

##########################################################################################

for (j in 1:3) {

	no0sp<-colSums(y_train[[j]])!=0
	y_train_no0sp<-y_train[[j]][,no0sp]
	no0spNames<-colnames(y_train_no0sp)

	save(no0spNames, file=file.path(FD,set_no,paste("no0sp_GJAM_",j,"_",dataN[sz],".RData",sep="")))

	Xt <- x_train[[j]][,-1]
	colnames(Xt)<-letters[1:ncol(Xt)]
	ml$notStandard<-letters[1:ncol(Xt)]	
	
	if (j==1) { sT<-Sys.time() }
		gjam1 <- NULL
		tryCatch({ gjam1	<-	gjam(formula=form, xdata=as.data.frame(Xt), ydata=as.data.frame(y_train_no0sp), modelList=ml) }, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
		if (is.null(gjam1)) {
			tryCatch({ gjam1	<-	gjam(formula=form, xdata=as.data.frame(Xt), ydata=as.data.frame(y_train_no0sp), modelList=ml_rl) }, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
		}
		if (is.null(gjam1)) {
			tryCatch({ gjam1	<-	gjam(formula=form, xdata=as.data.frame(Xt), ydata=as.data.frame(y_train_no0sp), modelList=ml_rl2) }, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
		}
		if (is.null(gjam1)) {
			tryCatch({ gjam1	<-	gjam(formula=form, xdata=as.data.frame(Xt), ydata=as.data.frame(y_train_no0sp), modelList=ml, REDUCT=FALSE) }, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
		}
		
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
	}
	if (MCMC2) {
		save(gjam1, file=file.path(FD,set_no,paste("gjam1_",j,"_",dataN[sz],"_MCMC2.RData",sep="")))
		if (j==1) {
			save(comTimes, file=file.path(FD,set_no,paste("comTimes_GJAM1_",dataN[sz],"_MCMC2.RData",sep="")))
		}
	} else {
		save(gjam1, file=file.path(FD,set_no,paste("gjam1_",j,"_",dataN[sz],".RData",sep="")))
		if (j==1) {
			save(comTimes, file=file.path(FD,set_no,paste("comTimes_GJAM1_",dataN[sz],".RData",sep="")))
		}
	}

	rm(gjam1)
	if (j==1) { rm(comTimes) }
	gc()
}
##########################################################################################
