##### MAKE SURE I TEST THIS
dat %>% filter(isfish==1,Year_df==max(Year_df)) %>% filter(basal_ls==1)
##### TO CHECK FOR ERRORS!!!

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
# Make Processing faster, I think?
dat=dat %>% mutate_at(c("isfish","basal_ls"),as.logical) %>%
	mutate_at(c("Phase_df","Nodes_df","Seed","Exper","Pred","Prey","species","lifestage"),as.factor)

# Probability of fish persisting in at least one of the experiments
# Probability of fish persisting in all of the experiments
subdat_ls=dat %>% filter(Year_df==max(Year_df),isfish==1) %>% 
	group_by(Simnum,Exper) %>%
	summarise(Tot_species=sum(Biomass)) %>% # But now it needs to survive in ALL experiments
	summarise(any=sum(Tot_species),all=prod(Tot_species)) %>%
	mutate_at(c("any","all"),as.logical)
subdat_ls %>% summarise_at(c("any","all"),mean)
# Subset the data that fit criteria 1 and 2
subdat1=dat %>% filter(Simnum %in% (subdat_ls %>% filter(any==TRUE))$Simnum)
subdat2=dat %>% filter(Simnum %in% (subdat_ls %>% filter(all==TRUE))$Simnum)

### Coefficient of Variation
CV <- function(dat){sd(dat)/mean(dat)*100}
sem <- function(x) {sd(x,na.rm=T)/sqrt(sum(is.na(x)))} # Standard error

eh=subdat2 %>% group_by(Exper,Simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_Bio=sum(Biomass),Fish_tot_Bio=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_Bio),CV_fish=CV(Fish_tot_Bio))# %>% ### So here we see the CVs for fish and total
	ggplot(aes(x=Exper,y=CV_tot)) + barplot()
	hist(eh$CV_fish) # Not notmal
	
	summarise_at(c("CV_tot","CV_fish"),c(mean,sem))

### Plot growth over life stages
dat %>% filter(Simnum==3,Exper==1,Year_df==1,isfish==1) %>%
	mutate(species=factor(species),lifestage=as.integer(lifestage)) %>%
	ggplot(aes(x=lifestage, y=Mass, colour=species))+geom_line()

# Biomass against weight_infty (make do w any mass for now though)
subdat2 %>% filter(Year_df==max(Year_df),isfish==1,Exper==1) %>%
	group_by(Simnum,Exper,species) %>%
	summarise(log_Tot_fish=log10(sum(Biomass)),log_infty=log10(max(Mass))) %>%
	filter(Tot_fish>-1000) %>%
	ggplot(aes(x=infty,y=Tot_fish)) + geom_point()
#For experiment 3 only and fits criteria 2

subdat2 %>% filter(Year_df==max(Year_df)) %>%
	group_by(Simnum,Exper) %>%
	summarise(Tot=sum(Biomass)) 



# coefficient of variation plot (indep) against weight_\infty









