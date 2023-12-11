mod_suit_fec<-lm(data=all_pop_diversity,Annual_mean_fecundity~Habitat_suitability)
summary(mod_suit_fec)
fit_suit_fec<-ggpredict(mod_suit_fec,terms="Habitat_suitability [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]")
Figure_5a<-ggplot(data=fit_suit_fec,aes(x=x,y=predicted))+
       geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="#879CA8",alpha=0.25)+
       geom_path(color="#386BAC",linewidth=0.8)+
       geom_point(data=all_pop_diversity,aes(x=Habitat_suitability,y=Annual_mean_fecundity),size=2)+
       scale_y_continuous(limits = c(2.5,7.3),breaks = c(3,5,7))+
       scale_x_continuous(limits = c(0,1),breaks = c(0,0.5,1))+
	   theme_bw()+
	   theme(panel.grid=element_blank(),legend.position="none")+
       xlab("Habitat suitability")+ylab("Annual fecundity")
