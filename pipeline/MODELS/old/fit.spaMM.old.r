# GLMs, also with SPATIAL STRUCTURE
##########################################################################################
library("spaMM")

if (set_no=="butterfly"|set_no=="bird") { 
form<-as.formula(sp~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
form_spat<-as.formula(sp~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2+Matern(1|Rand1+Rand2))
}
if (set_no=="diatom") { 
form<-as.formula(sp~V1+V2+V1_2+V2_2)
form_spat<-as.formula(sp~V1+V2+V1_2+V2_2+(1|Rand1))
}
if (set_no=="tree"|set_no=="plant") {
form<-as.formula(sp~V1+V2+V3+V1_2+V2_2+V3_2)
form_spat<-as.formula(sp~V1+V2+V3+V1_2+V2_2+V3_2+Matern(1|Rand1+Rand2))
}
if (set_no=="fungi") {
form<-as.formula(sp~V1+V2+V3+V2_2+V3_2)
form_spat<-as.formula(sp~V1+V2+V3+V2_2+V3_2+(1|Rand1))
}

### INTERPOLATION
##########################################################################################
nsp <- length(dd_i_t)

spaMM_I <- list()
spaMM_spat_I <- list()

sT<-Sys.time()
spaMM_I<-foreach(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ HLfit(form, family=binomial(link = "probit"), data=dd_i_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
eT<-Sys.time()
comTimes$spaMM<-eT-sT
save(spaMM_I, file=paste(FD,set_no,"/spaMM_I.r",sep=""))
	
if (set_no=="diatom" | set_no=="fungi") { 
	spaMM_spat_I<-foreach(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ HLfit(form_spat, family=binomial(link = "probit"), data=dd_i_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	eT<-Sys.time()
	comTimes$spaMM_spat<-eT-sT
} else {
	sT<-Sys.time()
	spaMM_spat_I<-foreach(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ corrHLfit(form_spat, family=binomial(link = "probit"), data=dd_i_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	eT<-Sys.time()
	comTimes$spaMM_spat<-eT-sT
}
save(spaMM_spat_I, file=paste(FD,set_no,"/spaMM_spat_I.r",sep=""))

### EXTRAPOLATION
##########################################################################################
nsp <- length(dd_e_t)

spaMM_E <- list()
spaMM_spat_E <- list()

spaMM_E<-foreach(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ HLfit(form, family=binomial(link = "probit"), data=dd_e_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
save(spaMM_E, file=paste(FD,set_no,"/spaMM_E.r",sep=""))
	
if (set_no=="diatom" | set_no=="fungi") { 
	spaMM_spat_E<-foreach(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ HLfit(form_spat, family=binomial(link = "probit"), data=dd_e_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
} else {
	spaMM_spat_E<-foreach(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ corrHLfit(form_spat, family=binomial(link = "probit"), data=dd_e_t[[i]])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
}
save(spaMM_spat_E, file=paste(FD,set_no,"/spaMM_spat_E.r",sep=""))

##########################################################################################