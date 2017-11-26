##########################################################################################
# GENERALIZED ADDITIVE MODELS
##########################################################################################

require(gjam)

ml  <- list(ng=30000, burnin=5000, typeNames='PA', holdoutN=0)

if (set_no=="birds" | set_no=="butterfly" | set_no=="plant") { 
	form<-as.formula(.~a+b+c+d+e+f+g+h+i+j)
	form_spat<-as.formula(.~a+b+c+d+e+f+g+h+i+j+k+l)
}
if (set_no=="trees") { 
	form<-as.formula(.~a+b+c+d+e+f)
	form_spat<-as.formula(.~a+b+c+d+e+f+g+h)
}
if (set_no=="vegetation") { 
	form<-as.formula(.~a+b+c+d+e+f+g+h)
	form_spat<-as.formula(.~a+b+c+d+e+f+g+h+i+j)
}


##########################################################################################

for (j in 1:3) {

	Xt <- x_train[[j]][,-1]
	colnames(Xt)<-letters[1:ncol(Xt)]
	Xts <- cbind(Xt,s_train[[j]])
	colnames(Xts)<-letters[1:ncol(Xts)]

	if (j==1) { sT<-Sys.time() }
	gjam	<-	gjam(formula=form, xdata=as.data.frame(Xt), ydata=as.data.frame(y_train[[j]]), modelList=ml)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(gjam, file=paste(FD,set_no,"/gjam_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GJAM1_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
	gjam2	<-	gjam(formula=form_spat, xdata=as.data.frame(Xts), ydata=as.data.frame(y_train[[j]]), modelList=ml)
	if (j==1) {
		eT<-Sys.time()
		comTimes$GJAM2<-eT-sT
		}
	save(gjam2, file=paste(FD,set_no,"/gjam2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GJAM2_",dataN[sz],".RData",sep=""))
	}
}
##########################################################################################
