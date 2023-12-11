library(lme4)
library(ggeffects)
library(ggplot2)
library(lmerTest)
mod_fec_PC1<-lm(data=all_pop_diversity,PC1~Annual_mean_fecundity*Topography,weights = Sample_size)
summary(mod_fec_PC1)
fit_fec_PC1<-ggpredict(mod_fec_PC1,terms=c("Annual_mean_fecundity","Topography"))
Figure_5c<-ggplot(data=fit_fec_PC1,aes(x=x,y=predicted,group=group))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high,group=group,fill = group),alpha=0.25)+
       geom_path(aes(color=group),linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Annual_mean_fecundity,y=PC1,group=Topography,color=Topography),size=2)+
       scale_fill_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_color_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_y_continuous(limits = c(-10,4.5),breaks = c(-8,-4,0,4),label = c(-8,-4,0,4))+
       scale_x_continuous(limits = c(3.4,6.2),breaks = c(4,5,6))+
       theme_bw()+
       theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Annual fecundity")+ylab("Genetic diversity")

mod_fec_He<-lm(data=all_pop_diversity,He~Annual_mean_fecundity*Topography,weights=Sample_size)
summary(mod_fec_He)
fit_fec_He<-ggpredict(mod_fec_He,terms=c("Annual_mean_fecundity","Topography"))
Figure_S8d<-ggplot(data=fit_fec_He,aes(x=x,y=predicted,group=group))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high,group=group,fill = group),alpha=0.25)+
       geom_path(aes(color=group),linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Annual_mean_fecundity,y=He,group=Topography,color=Topography),size=2)+
       scale_fill_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_color_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_y_continuous(limits = c(0.07,0.16),breaks = c(0.09,0.12,0.15))+
       scale_x_continuous(breaks = c(4,5,6))+
       theme_bw()+
       theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Annual fecundity")+ylab("He")

mod_fec_Pi<-lm(data=all_pop_diversity,Pi~Annual_mean_fecundity*Topography,weights=Sample_size)
summary(mod_fec_Pi)
fit_fec_Pi<-ggpredict(mod_fec_Pi,terms=c("Annual_mean_fecundity","Topography"))
Figure_S8e<-ggplot(data=fit_fec_Pi,aes(x=x,y=predicted,group=group))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high,group=group,fill = group),alpha=0.25)+
       geom_path(aes(color=group),linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Annual_mean_fecundity,y=Pi,group=Topography,color=Topography),size=2)+
       scale_fill_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_color_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_y_continuous(limits = c(0.07,0.17),breaks = c(0.1,0.13,0.16))+
       scale_x_continuous(breaks = c(4,5,6))+
       theme_bw()+
       theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Annual fecundity")+ylab("Pi")

mod_fec_Theta<-lm(data=all_pop_diversity,Theta~Annual_mean_fecundity*Topography,weights=Sample_size)
summary(mod_fec_Theta)
fit_fec_Theta<-ggpredict(mod_fec_Theta,terms=c("Annual_mean_fecundity","Topography"))
Figure_S8f<-ggplot(data=fit_fec_Theta,aes(x=x,y=predicted,group=group))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high,group=group,fill = group),alpha=0.25)+
       geom_path(aes(color=group),linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Annual_mean_fecundity,y=Theta,group=Topography,color=Topography),size=2)+
       scale_fill_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_color_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_y_continuous(limits = c(-3,133),breaks = c(30,75,120))+
       scale_x_continuous(breaks = c(4,5,6))+
       theme_bw()+
       theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Annual fecundity")+ylab("Watterson's theta")
