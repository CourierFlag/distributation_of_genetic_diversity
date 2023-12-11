library(lme4)
library(ggeffects)
library(ggplot2)
library(lmerTest)
mod_hist_PC1<-lmer(data=leading_pop_diversity ,PC1~Distance_to_refuge*Topography+(1|Clade),weights = Sample_size)
summary(mod_hist_PC1)
fit_hist_PC1 <- ggpredict(mod_hist_PC1,terms=c("Distance_to_refuge [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]","Topography"))
fit_hist_PC1_plot <- rbind(fit_hist_PC1[which((fit_hist_PC1$group == "CR_CA") & (fit_hist_PC1$x <= 0.65)),],fit_hist_PC1[which((fit_hist_PC1$group == "LE") & (fit_hist_PC1$x >= 0.5)),])
Figure_4c<-ggplot(data=fit_hist_PC1_plot,aes(x=x,y=predicted,group=group))+
	geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,group=group,fill=group),alpha=0.25)+
	geom_line(aes(color=group))+
	scale_color_manual(values=c("#4CB3A2","#C1759F"))+
	scale_fill_manual(values=c("#4CB3A2","#C1759F"))+
	geom_point(data=leading_pop_diversity,aes(x=Distance_to_refuge,y=PC1,color=Topography,group="none"),size=2)+
	scale_y_continuous(limits=c(-6,3),expand=c(0,0),breaks=c(-4,-2,0,2))+
	scale_x_continuous(breaks=c(0,0.5,1))+
	theme_bw()+
	theme(panel.grid=element_blank(),legend.position="none")+
	xlab("Distance to refuge")+ylab("Genetic diversity")

mod_hist_He<-lmer(data=leading_pop_diversity,He~Distance_to_refuge*Topography+(1|Clade),weights = Sample_size)
summary(mod_hist_He)
fit_hist_He <- ggpredict(mod_hist_He,terms=c("Distance_to_refuge [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]","Topography"))
fit_hist_He_plot <- rbind(fit_hist_He[which((fit_hist_He$group == "CR_CA") & (fit_hist_He$x <= 0.65)),],fit_hist_He[which((fit_hist_He$group == "LE") & (fit_hist_He$x >= 0.5)),])
Figure_S7a<-ggplot(data=fit_hist_He_plot,aes(x=x,y=predicted,group=group))+
	geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,group=group,fill=group),alpha=0.25)+
	geom_line(aes(color=group))+
	scale_color_manual(values=c("#4CB3A2","#C1759F"))+
	scale_fill_manual(values=c("#4CB3A2","#C1759F"))+
	geom_point(data=leading_pop_diversity,aes(x=Distance_to_refuge,y=He,color=Topography,group="none"),size=2)+
	scale_y_continuous(breaks=c(0.12,0.13,0.14))+
	scale_x_continuous(breaks=c(0,0.5,1))+
	theme_bw()+
	theme(panel.grid=element_blank(),legend.position="none")+
	xlab("Distance to refuge")+ylab("He")

mod_hist_Pi<-lmer(data=leading_pop_diversity,Pi~Distance_to_refuge*Top ography+(1|Clade),weights = Sample_size)
summary(mod_hist_Pi)
fit_hist_Pi <- ggpredict(mod_hist_Pi,terms=c("Distance_to_refuge [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]","Topography"))
fit_hist_Pi_plot <- rbind(fit_hist_Pi[which((fit_hist_Pi$group == "CR_CA") & (fit_hist_Pi$x <= 0.65)),],fit_hist_Pi[which((fit_hist_Pi$group == "LE") & (fit_hist_Pi$x >= 0.5)),])
Figure_S7b<-ggplot(data=fit_hist_Pi_plot,aes(x=x,y=predicted,group=group))+
	geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,group=group,fill=group),alpha=0.25)+
	geom_line(aes(color=group))+
	scale_color_manual(values=c("#4CB3A2","#C1759F"))+
	scale_fill_manual(values=c("#4CB3A2","#C1759F"))+
	geom_point(data=leading_pop_diversity,aes(x=Distance_to_refuge,y=Pi,color=Topography,group="none"),size=2)+
	scale_y_continuous(breaks=c(0.13,0.14,0.15))+
	scale_x_continuous(breaks=c(0,0.5,1))+
	theme_bw()+
	theme(panel.grid=element_blank(),legend.position="none")+
	xlab("Distance to refuge")+ylab("Pi")

mod_hist_Theta<-lmer(data=leading_pop_diversity,Theta~Distance_to_refuge*Topography+(1|Clade),weights = Sample_size)
summary(mod_hist_Theta)
fit_hist_Theta <- ggpredict(mod_hist_Theta,terms=c("Distance_to_refuge [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]","Topography"))
fit_hist_Theta_plot <- rbind(fit_hist_Theta[which((fit_hist_Theta$group == "CR_CA") & (fit_hist_Theta$x <= 0.65)),],fit_hist_Theta[which((fit_hist_Theta$group == "LE") & (fit_hist_Theta$x >= 0.5)),])
Figure_S7c<-ggplot(data=fit_hist_Theta_plot,aes(x=x,y=predicted,group=group))+
	geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high,group=group,fill=group),alpha=0.25)+
	geom_line(aes(color=group))+
	scale_color_manual(values=c("#4CB3A2","#C1759F"))+
	scale_fill_manual(values=c("#4CB3A2","#C1759F"))+
	geom_point(data=leading_pop_diversity,aes(x=Distance_to_refuge,y=Theta,color=Topography,group="none"),size=2)+
	scale_y_continuous(breaks=c(60,80,100))+
	scale_x_continuous(breaks=c(0,0.5,1))+
	theme_bw()+
	theme(panel.grid=element_blank(),legend.position="none")+
	xlab("Distance to refuge")+ylab("Watterson's theta")
