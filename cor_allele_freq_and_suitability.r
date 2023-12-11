site_sig <- data.frame()
line <- c(1,2,8,10,17,19,20,24,25,26,28,29,30,31,33,34,35,36,37,38)
k = 1
for(i in line){
	formula1 <- as.formula(paste("site",i,"~Habitat_suitability+(1|lineage)",sep=""))
	formula2 <- as.formula(paste("site",i,"~Annual_mean_fecundity+(1|lineage)",sep=""))
	wei <- paste("site",i,sep="")
	site_sig[k,1]<- wei
	mod1 <- lmer(data=missense_freq ,formula=formula1,weights=Sample_size)
	sum1 <- summary(mod1)
	mod2 <- lmer(data=missense_freq ,formula=formula2,weights=Sample_size)
	sum2 <- summary(mod2)
	site_sig[k,2]<- sum1[["coefficients"]][2,5]
	site_sig[k,3]<- sum2[["coefficients"]][2,5]
	k<-k+1
}
colnames(site_sig) <- c("site","cor_suit","cor_fec")
mod29<-lmer(data=missense_freq,site29~Habitat_suitability+(1|lineage),weights=Sample_size)
summary(mod29)
mod37<-lmer(data=missense_freq,site37~Habitat_suitability+(1|lineage),weights=Sample_size)
summary(mod37)
mod38<-lmer(data=missense_freq,site38~Habitat_suitability+(1|lineage),weights=Sample_size)
summary(mod38)

mod29<-lmer(data=missense_freq,site29~Annual_mean_fecundity+(1|lineage),weights=Sample_size)
summary(mod29)
mod37<-lmer(data=missense_freq,site37~Annual_mean_fecundity+(1|lineage),weights=Sample_size)
summary(mod37)
mod38<-lmer(data=missense_freq,site38~Annual_mean_fecundity+(1|lineage),weights=Sample_size)
summary(mod38)