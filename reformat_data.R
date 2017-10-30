rm(list=ls())
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
#---- CompileData ----
for (simnum in c(1:100)){
	for (Exper in 1:3){
		for (pred in 2){
			for (prey in 0){
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
				colnames(B_year_end)=1:nichewebsize
				
				Phase_df=c();yr_in_phase=c()
				for (item in 1:length(numyears)){
					if (numyears[item]>0) {yr_in_phase=c(yr_in_phase,1:numyears[item])}
					Phase_df=c(Phase_df,rep(item,numyears[item]))
				}
				B_year_end=cbind(Year_df=1:dim(B_year_end)[1],B_year_end,Phase_df,yr_in_phase)
				
				clean=data.frame(B_year_end) %>% gather(key="Nodes_df",value="Biomass",-Year_df,-Phase_df,-yr_in_phase) %>% 
					mutate(Day_df=Year_df*L_year) %>%
					mutate(Seed=seed_0,Simnum=simnum,Exper=Exper,Pred=pred,Prey=prey) %>%
					mutate(Nodes_df=as.numeric(gsub("[^0-9]","",Nodes_df))) %>%
					mutate(isfish=isfish[Nodes_df],basal_ls=basal_ls[Nodes_df],species=species[Nodes_df],lifestage=lifestage[Nodes_df],Mass=Mass[Nodes_df])
				
				################################################
				################## Save Data ###################
				################################################
				# Last line is Data entry into matrix, because line should only be entered if all data was collected
				# It loads it into line k for the data matrix. 
				write.table(clean,"clean.txt",append=T,col.names = F,row.names = F)
				print(simnum)
			}
		}
	}
}
write.table(colnames(clean),"colnames_clean.txt",col.names = F,row.names = F)
#---- LOAD_DATA ----
dat=read.table("clean.txt",header=F)
colnames(dat)=as.matrix(read.table("colnames_clean.txt"))
#Temporary hosting site for functions:
#dat %>% as_tibble() %>% mutate_all(is.double(), as.factor())





### Plot growth over life stages
dat %>% filter(Simnum==3,Exper==1,Year_df==1,isfish==1) %>%
	mutate(species=factor(species)) %>%
	ggplot(aes(x=lifestage, y=Mass, colour=species))+geom_line()

subdat=dat

# Biomass against weight_infty (make do w any mass for now though)
dat %>% filter(Year_df==max(Year_df),isfish==1,Exper==1) %>%
	group_by(Simnum,Exper,species) %>%
	summarise(Tot_fish=sum(Biomass),infty=log10(max(Mass))) %>%
	ggplot(aes(x=infty,y=Tot_fish)) + geom_point()
#For experiment 3 only and fits criteria 2

dat %>% filter(Year_df==max(Year_df)) %>%
	group_by(Simnum,Exper) %>%
	summarise(Tot=sum(Biomass)) 



# coefficient of variation plot (indep) against weight_\infty









