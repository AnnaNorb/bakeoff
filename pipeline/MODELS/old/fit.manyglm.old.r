# FIT MANYLGM
##########################################################################################

library("mvabund")

# interpolation
##########################################################################################
Yt <- mvabund(y_I_train)
X_I <- data.frame(X_I_train[,-1])
#X_Is <- data.frame(cbind(X_I_train[,-1],S_I_train))

if (set_no=="butterfly"|set_no=="bird") { 
form<-mvformula(Yt~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
#form_spat<-mvformula(Yt~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2+(1|Rand1)+(1|Rand2))
}
if (set_no=="diatom") { 
form<-as.formula(Yt~V1+V2+V1_2+V2_2)
#form_spat<-as.formula(Yt~V1+V2+V1_2+V2_2+(1|Rand1))
}
if (set_no=="tree"|set_no=="plant") {
form<-mvformula(Yt~V1+V2+V3+V1_2+V2_2+V3_2)
#form_spat<-as.formula(Yt~V1+V2+V3+V1_2+V2_2+V3_2+(1|Rand1)+(1|Rand2))
}
if (set_no=="fungi") {
form<-mvformula(Yt~V1+V2+V3+V2_2+V3_2)
#form_spat<-as.formula(Yt~V1+V2+V3+V2_2+V3_2+(1|Rand1))
}

sT<-Sys.time()
manyglm_I<-manyglm(form, family="binomial(link=probit)",data=X_I, method='glm1path')
eT<-Sys.time()
comTimes$MVABUND<-eT-sT

save(manyglm_I, file=paste(FD,set_no,"/manyglm_I.r",sep=""))

#manyglm_I_spat<-manyglm(form_spat, family="binomial(link=probit)",data=X_Is)
#save(manyglm_I_spat, file=paste(FD,"data",set_no,"/","manyglm_I_spat.r",sep=""))


# extrapolation
##########################################################################################
Yt <- mvabund(y_E_train)
X_E <- data.frame(X_E_train[,-1])
#X_Es <- as.data.frame(cbind(X_E_train[,-1],S_E_train))

if (set_no=="butterfly"|set_no=="bird") { 
form<-mvformula(Yt~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
#form_spat<-mvformula(Yt~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2+(1|Rand1)+(1|Rand2))
}
if (set_no=="diatom") { 
form<-as.formula(Yt~V1+V2+V1_2+V2_2)
#form_spat<-as.formula(Yt~V1+V2+V1_2+V2_2+(1|Rand1))
}
if (set_no=="tree"|set_no=="plant") {
form<-mvformula(Yt~V1+V2+V3+V1_2+V2_2+V3_2)
#form_spat<-as.formula(Yt~V1+V2+V3+V1_2+V2_2+V3_2+(1|Rand1)+(1|Rand2))
}
if (set_no=="fungi") {
form<-mvformula(Yt~V1+V2+V3+V2_2+V3_2)
#form_spat<-as.formula(Yt~V1+V2+V3+V2_2+V3_2+(1|Rand1))
}

manyglm_E<-manyglm(form, family="binomial(link=probit)",data=X_E, method='glm1path')

save(manyglm_E, file=paste(FD,set_no,"/manyglm_E.r",sep=""))

#manyglm_E_spat<-manyglm(form_spat, family="binomial(link=probit)",data=X_Es,method="glm1path")
#save(manyglm_E_spat, file=paste(FD,"data",set_no,"/","manyglm_E_spat.r",sep=""))

##########################################################################################

rm(Yt)
rm(X_I)
rm(X_E)
rm(manyglm_I)
rm(manyglm_E)
#rm(manyglm_I_spat)
#rm(manyglm_E_spat)
gc()