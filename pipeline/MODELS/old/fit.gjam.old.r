##########################################################################################
# fit GJAM
##########################################################################################

library("gjam")

ml  <- list(ng=10000, burnin=2000, typeNames='PA', holdoutN=0)

if (set_no=="butterfly"|set_no=="bird") { 
form<-as.formula(.~a+b+c+d+e+f+g+h)
}
if (set_no=="diatom") { 
form<-as.formula(.~a+b+c+d)
}
if (set_no=="tree"|set_no=="plant") {
form<-as.formula(.~a+b+c+d+e+f)
}
if (set_no=="fungi") {
form<-as.formula(.~a+b+c+d+e)
}

# interpolation
##########################################################################################
Xtrain_I<-X_I_train[,-1]
colnames(Xtrain_I)<-letters[1:ncol(Xtrain_I)]

sT<-Sys.time()
gjam_I<-gjam(formula=form, xdata=as.data.frame(Xtrain_I), ydata=as.data.frame(y_I_train), modelList=ml)
eT<-Sys.time()
comTimes$GJAM<-eT-sT

save(gjam_I, file=paste(FD,set_no,"/gjam_I.r",sep=""))

# extrapolation
##########################################################################################
Xtrain_E<-X_E_train[,-1]
colnames(Xtrain_E)<-letters[1:ncol(Xtrain_E)]

gjam_E<-gjam(formula=form, xdata=as.data.frame(Xtrain_E), ydata=as.data.frame(y_E_train), modelList=ml)

save(gjam_E, file=paste(FD,set_no,"/gjam_E.r",sep=""))

##########################################################################################
