library(lme4)
library(ggeffects)
library(ggplot2)
library(lmerTest)
mod_suit_PC1<-lm(data=all_pop_diversity,PC1~Habitat_suitability*Topography,weights=Sample_size)
summary(mod_suit_PC1)
fit_suit_PC1<-ggpredict(mod_suit_PC1,terms=c("Habitat_suitability [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]","Topography"))
Figure_5b<-ggplot(data=fit_suit_PC1,aes(x=x,y=predicted,group=group))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high,group=group,fill = group),alpha=0.25)+
       geom_path(aes(color=group),linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Habitat_suitability,y=PC1,group=Topography,color=Topography),size=2)+
       scale_fill_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_color_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_y_continuous(limits = c(-10,4.5),breaks = c(-8,-4,0,4),label = c(-8,-4,0,4))+
       scale_x_continuous(limits = c(0,1),breaks = c(0,0.5,1))+
       theme_bw()+
       theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Habitat suitability")+ylab("Genetic diversity")

mod_suit_He<-lm(data=all_pop_diversity,He~Habitat_suitability*Topography,weights=Sample_size)
summary(mod_suit_He)
fit_suit_He<-ggpredict(mod_suit_He,terms=c("Habitat_suitability [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]","Topography"))
Figure_S8a<-ggplot(data=fit_suit_He,aes(x=x,y=predicted,group=group))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high,group=group,fill = group),alpha=0.25)+
       geom_path(aes(color=group),linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Habitat_suitability,y=He,group=Topography,color=Topography),size=2)+
       scale_fill_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_color_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_y_continuous(limits = c(0.07,0.16),breaks = c(0.09,0.12,0.15))+
       scale_x_continuous(limits = c(0,1),breaks = c(0,0.5,1))+
       theme_bw()+
       theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Habitat suitability")+ylab("He")

mod_suit_Pi<-lm(data=all_pop_diversity,Pi~Habitat_suitability*Topography,weights=Sample_size)
summary(mod_suit_Pi)
fit_suit_Pi<-ggpredict(mod_suit_Pi,terms=c("Habitat_suitability [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]","Topography"))
Figure_S8b<-ggplot(data=fit_suit_Pi,aes(x=x,y=predicted,group=group))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high,group=group,fill = group),alpha=0.25)+
       geom_path(aes(color=group),linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Habitat_suitability,y=Pi,group=Topography,color=Topography),size=2)+
       scale_fill_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_color_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_y_continuous(limits = c(0.07,0.17),breaks = c(0.1,0.13,0.16))+
       scale_x_continuous(limits = c(0,1),breaks = c(0,0.5,1))+
       theme_bw()+
       theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Habitat suitability")+ylab("Pi")

mod_suit_Theta<-lm(data=all_pop_diversity,Theta~Habitat_suitability*Topography,weights=Sample_size)
summary(mod_suit_Theta)
fit_suit_Theta<-ggpredict(mod_suit_Theta,terms=c("Habitat_suitability [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]","Topography"))
Figure_S8c<-ggplot(data=fit_suit_Theta,aes(x=x,y=predicted,group=group))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high,group=group,fill = group),alpha=0.25)+
       geom_path(aes(color=group),linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Habitat_suitability,y=Theta,group=Topography,color=Topography),size=2)+
       scale_fill_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_color_manual(values=c("#DD9C15","#4CB3A2","#C1759F"))+
       scale_y_continuous(limits = c(-3,132),breaks = c(30,75,120))+
       scale_x_continuous(limits = c(0,1),breaks = c(0,0.5,1))+
       theme_bw()+
       theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Habitat suitability")+ylab("Watterson's theta")
