##### MAKE SURE I TEST THIS
dat %>% filter(isfish==1,Year_df==max(Year_df)) %>% filter(basal_ls==1)
##### TO CHECK FOR ERRORS!!!

rm(list=ls())
library(tidyverse)
seed_0=0
lifestages_linked=1
Adults_only=0
DATE="2017Oct30"
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
				import_vars_web=c('isfish','basalsp','basal_ls','species','numyears','nichewebsize','ext_thresh','N_stages','lifestage','L_year','Mass','lifehis.splitdiet','lifehis.fishpred','Z','meta','TrophLevel','orig_T')
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
					mutate(Seed=seed_0,simnum,Exper,pred,prey) %>%
					mutate(Nodes_df=as.numeric(gsub("[^0-9]","",Nodes_df))) %>%
					mutate(isfish=isfish[Nodes_df],basal_ls=basal_ls[Nodes_df],species=species[Nodes_df],lifestage=lifestage[Nodes_df],Mass=Mass[Nodes_df],Z=Z[Nodes_df],meta=meta[Nodes_df],TrophLevel=TrophLevel[Nodes_df],orig_T=orig_T[Nodes_df])
				
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
# Probability of fish persisting in at least one of the experiments
# Probability of fish persisting in all of the experiments
subdat_ls=dat %>% filter(Year_df==max(Year_df),isfish==1) %>% 
	group_by(simnum,Exper) %>%
	summarise(Tot_species=sum(Biomass)) %>% # But now it needs to survive in ALL experiments
	summarise(any=sum(Tot_species),all=prod(Tot_species)) %>%
	mutate_at(c("any","all"),as.logical)
# Subset the data that fit criteria 1 and 2
subdat1=dat %>% filter(simnum %in% (subdat_ls %>% filter(any==TRUE))$simnum)
subdat2=dat %>% filter(simnum %in% (subdat_ls %>% filter(all==TRUE))$simnum)

################################################
################################################
#### Temporary hosting site for functions: #####
################################################
################################################
# Actual Plots:
# This one works
sim_stats=subdat2 %>% filter(Year_df==max(Year_df),Exper==1) %>%
	group_by(simnum,Exper,species) %>% 
	mutate(Tot_spec=sum(Biomass)) %>%
	filter(Tot_spec>0) %>% # Important: Filter out extinct species first so you only get stats for extant species
	group_by(simnum,Exper) %>% 
	summarise(Tot_Bio=sum(Biomass),Tot_fish=sum(Biomass*isfish),max_Z=max(Z),max_Mass=max(Mass),max_fish_mass=max(Mass*isfish))

CV_plot=subdat2 %>% group_by(Exper,simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_bio=sum(Biomass),Tot_fish=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_bio),CV_fish=CV(Tot_fish))

full_stats=left_join(sim_stats,CV_plot) %>% 
	mutate(log_tot=log10(Tot_Bio),log_fish=log10(Tot_fish),log_max_mass=log10(max_Mass))

full_stats %>% ggplot(aes(x=log_max_mass,y=log_fish)) + geom_point()

# Okay and we can do one last one where it's by species W_infty against (that same species') final biomass and CV
CV_spec_stats=subdat2 %>% filter(Phase_df==2,Exper==1) %>%
	group_by(simnum,Exper,species,Year_df) %>%
	summarise(Tot_spec=sum(Biomass)) %>%
	summarise(CV_spec=CV(Tot_spec))

gen_spec_stats=subdat2 %>% filter(Year_df==max(Year_df),Exper==1) %>%
	group_by(simnum,Exper,species) %>%
	mutate(Tot_spec=sum(Biomass),max_Z=max(Z),max_Mass=max(Mass)) %>%
	filter(Tot_spec>0,isfish==1,lifestage==1)

all_spec_stats=left_join(gen_spec_stats,CV_spec_stats) %>% 
	mutate(log_spec=log10(Tot_spec),log_max_mass=log10(max_Mass))

all_spec_stats %>% ggplot(aes(x=log_max_mass,y=log_spec)) + geom_point()

################################################
