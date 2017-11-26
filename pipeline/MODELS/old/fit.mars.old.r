# MARS MODEL TO FREDNEWDATA FOR BAKEOFF
##########################################################################################
library("earth")

# interpolation
sT<-Sys.time()
mars_I	<-	earth(x=X_I_train[,c(2:(ncovar+1))], y=y_I_train,
			        degree=1, glm=list(family=binomial(link="probit")))
eT<-Sys.time()
comTimes$'MARS-COMM'<-eT-sT

sT<-Sys.time()
mars_int_I	<-	earth(x=X_I_train[,c(2:(ncovar+1))], y=y_I_train,
                    degree=2, glm=list(family=binomial(link="probit")))
eT<-Sys.time()
comTimes$'MARS-INT'<-eT-sT

# extrapolation
mars_E	<-	earth(x=X_E_train[,c(2:(ncovar+1))], y=y_E_train,
                degree=1, glm=list(family=binomial(link="probit")))
mars_int_E	<-	earth(x=X_E_train[,c(2:(ncovar+1))], y=y_E_train,
                    degree=2, glm=list(family=binomial(link="probit")))


##########################################################################################

save(mars_I, file=paste(FD,set_no,"/mars_I.r",sep=""))
save(mars_int_I, file=paste(FD,set_no,"/mars_int_I.r",sep=""))
save(mars_E, file=paste(FD,set_no,"/mars_E.r",sep=""))
save(mars_int_E, file=paste(FD,set_no,"/mars_int_E.r",sep=""))

##########################################################################################
