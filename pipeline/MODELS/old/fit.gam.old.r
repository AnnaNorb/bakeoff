# GAMs FOR THE BAKEOFF DATA
##########################################################################################
library('mgcv')
library('nlme')
##########################################################################################

if (set_no=="butterfly"|set_no=="bird") { 
form<-as.formula(sp~s(V1, bs="cr")+s(V2, bs="cr")+s(V3, bs="cr")+s(V4, bs="cr"))
form_spat<-as.formula(sp~s(V1, bs="cr")+s(V2, bs="cr")+s(V3, bs="cr")+s(V4, bs="cr")+s(Rand1,Rand2, bs="re"))
}
if (set_no=="diatom") { 
form<-as.formula(sp~s(V1, bs="cr")+s(V2, bs="cr"))
form_spat<-as.formula(sp~s(V1, bs="cr")+s(V2, bs="cr")+s(Rand1,bs="re"))
}
if (set_no=="tree"|set_no=="plant") {
form<-as.formula(sp~s(V1, bs="cr")+s(V2, bs="cr")+s(V3, bs="cr"))
form_spat<-as.formula(sp~s(V1, bs="cr")+s(V2, bs="cr")+s(V3, bs="cr")+s(Rand1,Rand2, bs="re"))
}
if (set_no=="fungi") {
form<-as.formula(sp~V1+s(V2, bs="cr")+s(V3, bs="cr"))
form_spat<-as.formula(sp~V1+s(V2, bs="cr")+s(V3, bs="cr")+s(Rand1, bs="re"))
ddI<-dd_i_t
ddE<-dd_e_t
	for (i in 1:length(dd_i_t)) {
	dd_i_t[[i]][,4]<-dd_i_t[[i]][,4]+rnorm(length(dd_i_t[[i]][,4]),mean=0,sd=0.01)
	}
	for (i in 1:length(dd_e_t)) {
	dd_e_t[[i]][,4]<-dd_e_t[[i]][,4]+rnorm(length(dd_e_t[[i]][,4]),mean=0,sd=0.01)
	}
}

# INTERPOLATION
##########################################################################################
gam_I <- list()
gam_I_spat1<- list()
gam_I_spat2<- list()

nsp <- length(dd_i_t)

sT<-Sys.time()
gam_I<-foreach(i=1:nsp, .packages='mgcv') %dopar% { tryCatch({gam(form, family=binomial(link="probit"), data=dd_i_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
eT<-Sys.time()
comTimes$GAM<-eT-sT


if (set_no=="diatom"|set_no=="fungi") { 
sT<-Sys.time()
gam_I_spat1<-foreach(i=1:nsp, .packages='mgcv') %dopar% { tryCatch({gamm(form, random=list(Rand1=~1), correlation=corAR1(), family=binomial(link = "probit"), data=dd_i_t[[i]]) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
eT<-Sys.time()
comTimes$GAM_spat1<-eT-sT

} else {
sT<-Sys.time()
gam_I_spat1<-foreach(i=1:nsp, .packages='mgcv') %dopar% { tryCatch({gamm(form, correlation=corGaus(form=~Rand1+Rand2), family=binomial(link = "probit"), data=dd_i_t[[i]]) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
eT<-Sys.time()
comTimes$GAM_spat1<-eT-sT
}

sT<-Sys.time()
gam_I_spat2<-foreach(i=1:nsp, .packages='mgcv') %dopar% { tryCatch({gam(form_spat, family=binomial(link = "probit"), data=dd_i_t[[i]], method="REML") }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
eT<-Sys.time()
comTimes$GAM_spat2<-eT-sT

save(gam_I, file=paste(FD,set_no,"/gam_I.r",sep=""))
save(gam_I_spat1, file=paste(FD,set_no,"/gam_I_spat1.r",sep=""))
save(gam_I_spat2, file=paste(FD,set_no,"/gam_I_spat2.r",sep=""))


# EXTRAPOLATION
##########################################################################################
gam_E <- list()
gam_E_spat1<- list()
gam_E_spat2<- list()

nsp <- length(dd_e_t)

gam_E<-foreach(i=1:nsp, .packages='mgcv') %dopar% { tryCatch({gam(form, family=binomial(link = "probit"), data=dd_e_t[[i]]) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

if (set_no=="diatom"|set_no=="fungi") { 
	gam_E_spat1<-foreach(i=1:nsp, .packages='mgcv') %dopar% { tryCatch({gamm(form, random=list(Rand1=~1), correlation=corAR1(), family=binomial(link = "probit"), data=dd_e_t[[i]]) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
} else {
	gam_E_spat1<-foreach(i=1:nsp, .packages='mgcv') %dopar% { tryCatch({gamm(form, correlation=corGaus(form=~Rand1+Rand2),  family=binomial(link = "probit"), data=dd_e_t[[i]]) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
}

gam_E_spat2<-foreach(i=1:nsp, .packages='mgcv') %dopar% { tryCatch({gam(form_spat, method="REML",  family=binomial(link = "probit"), data=dd_e_t[[i]]) }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

save(gam_E, file=paste(FD,set_no,"/gam_E.r",sep=""))
save(gam_E_spat1, file=paste(FD,set_no,"/gam_E_spat1.r",sep=""))
save(gam_E_spat2, file=paste(FD,set_no,"/gam_E_spat2.r",sep=""))

##########################################################################################

if (set_no=="fungi") {
dd_i_t<-ddI
dd_e_t<-ddE
}

##########################################################################################
