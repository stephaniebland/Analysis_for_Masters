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
# Probability of fish persisting in at least one of the experiments
# Probability of fish persisting in all of the experiments
subdat_ls=dat %>% filter(Year_df==max(Year_df),isfish==1) %>% 
	group_by(Simnum,Exper) %>%
	summarise(Tot_species=sum(Biomass)) %>% # But now it needs to survive in ALL experiments
	summarise(any=sum(Tot_species),all=prod(Tot_species)) %>%
	mutate_at(c("any","all"),as.logical)
# Subset the data that fit criteria 1 and 2
subdat1=dat %>% filter(Simnum %in% (subdat_ls %>% filter(any==TRUE))$Simnum)
subdat2=dat %>% filter(Simnum %in% (subdat_ls %>% filter(all==TRUE))$Simnum)

################################################
################################################
#### Temporary hosting site for functions: #####
################################################
################################################
library(stringr)
node_ls=unique(alldata$Nodes_df)[str_detect(unique(alldata$Nodes_df),"Node_")]
count_extant=alldata %>% group_by(Simnum, Exper, Nodes_df) %>%
	filter(Year_df %in% max(Year_df), Nodes_df %in% node_ls) %>% 
	summarise(Extant=(Biomass>0)) %>%
	spread(key=Nodes_df, value=Extant) %>%
	summarise(node_persist=sum(Node_1,Node_2,Node_3,Node_4,Node_5,Node_6,Node_7,Node_8,Node_9,Node_10,Node_11,Node_12,Node_13,Node_14,Node_15,Node_16,Node_17,Node_18,Node_19,Node_20,Node_21,Node_22,Node_23,Node_24,Node_25,Node_26,Node_27,Node_28,Node_29,Node_30,Node_31,Node_32,Node_33,Node_34,Node_35,Node_36,Node_37,Node_38,Node_39)) %>%
	spread(key=Exper,value=node_persist)
x=mean(count_extant$`1`)/39
y=mean(count_extant$`2`)/39
z=mean(count_extant$`3`)/30
sem <- function(x) {sd(x)/sqrt(length(x))}
meh=data.frame(Exper=1:3,Num_extant=c(x,y,z),se=c(sem(count_extant$`1`/39),sem(count_extant$`2`/39),sem(count_extant$`3`/30)))
ggplot(meh, aes(x=Exper, y=Num_extant)) +
	geom_bar(position=position_dodge(),stat="identity") + 
	geom_errorbar(aes(ymin=Num_extant-2*se, ymax=Num_extant+2*se), width=.1) +
	xlab("Experiment") + ylab("Percent of extant nodes") +
	labs(caption="Figure 1 The mean percent of extant nodes in each experiment. The error bars are of 2 times the standard error.")

################################################
extant_species=dat %>% filter(Year_df==max(Year_df)) %>%
	group_by(Exper,Simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) #%>%
boxplot(Num_extant~Exper,extant_species)
extant_species %>% summarise(mean(Num_extant),var(Num_extant))

#FIX THIS: I NEED TO DO PERCENT OF EXTANT NODES RATHER THAN NUMBER
#ALSO DOUBLE CHECK WITH MEH
dat %>% filter(Year_df==1) %>% group_by(Exper,Simnum) %>% summarise(num_nodes=sum(Biomass>0))
extant_nodes=dat %>% filter(Year_df==max(Year_df)) %>%
	group_by(Exper,Simnum,Nodes_df) %>% 
	summarise(extant=as.logical(Biomass)) %>%
	summarise(Num_extant=sum(extant)) %>% 
	mutate(Per_extant=Num_extant/c(39,39,30)[Exper])
boxplot(Per_extant~Exper,extant_nodes)
extant_nodes %>% summarise(mean(Per_extant),var(Per_extant))

extant_fish=dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Exper,Simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) #%>%
boxplot(Num_extant~Exper,extant_fish)
extant_fish %>% summarise(mean(Num_extant),var(Num_extant))


################################################

# Biomass against weight_infty (make do w any mass for now though)
subdat2 %>% filter(Year_df==max(Year_df),isfish==1,Exper==1) %>%
	group_by(Simnum,Exper,species) %>%
	summarise(log_Tot_fish=log10(sum(Biomass)),log_infty=log10(max(Mass))) %>%
	filter(log_Tot_fish>-1000) %>%
	ggplot(aes(x=log_infty,y=log_Tot_fish)) + geom_point()
#For experiment 3 only and fits criteria 2

subdat2 %>% filter(Year_df==max(Year_df)) %>%
	group_by(Simnum,Exper) %>%
	summarise(Tot=sum(Biomass)) 



# coefficient of variation plot (indep) against weight_\infty









