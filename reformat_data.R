library(tidyverse)
seed_0=0
lifestages_linked=1
Adults_only=0
DATE="2017Jul19"
Version="0"
#simnum=1
#Exper=1
location="/GIT/Analysis"#For Running on my Mac
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
setwd(paste0("~/",location,"/",run_name))
################################################
simnum=1
Exper=1
pred=2
prey=0
################################################
############### Read in Data ###################
################################################
name=paste0(run_name,"_seed",seed_0,"_sim",simnum,"_Exper",Exper,"_pred",pred,"_prey",prey)

import_vars_sim='B_year_end'#c('B','B_year_end','B_stable_phase')
import_vars_web=c('isfish','basalsp','basal_ls','species','numyears','nichewebsize','ext_thresh','N_stages','lifestage','L_year','Mass','lifehis.splitdiet','lifehis.fishpred')
import_vars=c(import_vars_sim,import_vars_web)
for (item in 1:length(import_vars)){
	trial=paste0(name,"_",import_vars[item],".txt")
	trial=as.matrix(read.csv(trial,header=F))
	if (sum(dim(trial))==2) {trial=as.integer(trial)}
	do.call("<-",list(import_vars[item], trial))
}

################################################
############### Extract Data ###################
################################################
#colnames(B_year_end)=paste0('Node_',1:nichewebsize)
colnames(B_year_end)=1:nichewebsize
Phase_df=c();yr_in_phase=c()
for (item in 1:length(numyears)){
	if (numyears[item]>0) {yr_in_phase=c(yr_in_phase,1:numyears[item])}
	Phase_df=c(Phase_df,rep(item,numyears[item]))
	}
B_year_end=cbind(Year_df=1:dim(B_year_end)[1],B_year_end,Phase_df=Phase_df,yr_in_phase=yr_in_phase)

eh=data.frame(B_year_end) %>% gather(key="Nodes_df",value="Biomass",-Year_df,-Phase_df,-yr_in_phase,convert=TRUE) %>% 
	mutate(Day_df=Year_df*L_year) %>%
	mutate(Seed=seed_0,Simnum=simnum,Exper=Exper,Pred=pred,Prey=prey) %>%
	mutate(Nodes_df=as.numeric(gsub("[^0-9]","",Nodes_df))) %>%
	mutate(xkcd=c(50:100)[Nodes_df]) 


head(eh)
head(alldata)
























