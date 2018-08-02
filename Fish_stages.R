p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot()
p


sure=dat %>% filter(Model==2,Phase_df==2,isfish==1) %>%
	group_by(simnum,Model,Nodes_df) %>%
	summarise(node_mean=mean(Biomass))

#sure2=sure %>% filter(Model==2,Phase_df==2,isfish==1) %>%
#	group_by(simnum,Model,species) %>%
#	summarise(fish_mean=mean(Biomass))

aight=dat %>% filter(Model==2,Year_df==max(Year_df),isfish==1) %>%
	left_join(.,sure) 

sure2=aight %>% group_by(simnum,Model,species) %>%
	summarise(fish_mean=sum(node_mean))

aight2=aight %>% left_join(.,sure2) %>%
	mutate(prop_stage=node_mean/fish_mean) %>%
	mutate(prop_stage2=ifelse(is.na(prop_stage),0,prop_stage))

boxplot(node_mean~lifestage,aight2,ylim=c(0,1))
boxplot(prop_stage2~lifestage,aight2,ylim=c(0,.01))
boxplot(prop_stage~lifestage,aight2,ylim=c(0,.01))

aight2 %>% group_by(lifestage) %>% summarise(mean(prop_stage,na.rm=T))
