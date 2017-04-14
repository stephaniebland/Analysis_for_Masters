# Cluster R script
# Stephanie Bland
# Analysis of Data in the Cluster
# This function takes data from each simulation and merges it into one file per data type (so output will give a file for each variable, and each file will have data across all experiments and simulations) - actually i cant do that easily (bc # nodes might change and woah there's lots of data)

# Easiest way to prevent duplicate rows or missing rows (in case cluster processes a job multiple times or not at all) would probably be to create a NaN matrix fill rows up with values **at the end** (so if there's an error you'll be able to find it easily, and rows will only be entered if all data was collected)
# Input k tells you what row in the matrix to fill

################################################
############### Temp Testing ###################
################################################
rm(list=ls())
k=1
setwd("/Users/JurassicPark/Google Drive/GIT/Analysis")
library(R.matlab)
library(matrixStats)
library(RColorBrewer)
library(codyn)
library(knitr)
library(reshape2)
library(dplyr)

exp_type_all=c("complete","extended_unlinked","origweb")
exp_type=exp_type_all[1]
sim_data=readMat(paste0(exp_type,"_",k,".mat"))
#names(sim_data)
attach(sim_data)

################################################
############### Read in Data ###################
################################################
B
isfish


################################################
############### Extract Data ###################
################################################
B=B_test
# Set up - helpful data reformatting:
colnames(B)=paste0('Node_',1:nichewebsize)
inverts_only=setdiff(which(isfish==0),basalsp) #Find invertebrates (not fish or autotrophs)
fish_names=unique(species[isfish==1]) #The species number for fish
yr_ls=cumsum(unlist(num.years)) # Cumulative sums of years for phases
# 1 - add in columns for sum of nodes so we have biomass of groups of species (like all fish)
#Fish total per species
Fish_tot_per_sp_df=c()
for (item in fish_names){
  Fish_tot_per_sp_df=cbind(Fish_tot_per_sp_df,rowSums(B[,species==item]))
}
colnames(Fish_tot_per_sp_df)=paste0('Fish_sp_',fish_names)

#Fish total per lifestage
Fish_tot_per_ls_df=c()
for (item in 1:max(N.stages)){
  fish_stages=(lifestage==item & t(isfish)==1)
  Fish_tot_per_ls_df=cbind(Fish_tot_per_ls_df,rowSums(B[,fish_stages]))
}
colnames(Fish_tot_per_ls_df)=paste0('Fish_ls_',1:max(N.stages))

#Find sum of basic categories
Tot_df=rowSums(B)
Fish_tot_df=rowSums(B[,isfish==1])#Total biomass of all fish species
non_fish_df=rowSums(B[,isfish==0])#Total Biomass of Non Fish species
basal_tot_df=rowSums(B[,basalsp])#Total Biomass of all autotrophs
inverts_tot_df=rowSums(B[,inverts_only])#Total Biomass of invertebrates only

#Bind them to regular Data frame
B_df=cbind(B,Fish_tot_per_sp_df,Fish_tot_per_ls_df,Tot_df,Fish_tot_df,non_fish_df,basal_tot_df,inverts_tot_df)


B_df[seq(1,1000,by=100),]
B_df[1:5,]
# 2 melt data so [i j B] = [day, node (or sum of nodes), Biomass]
first_melt=setNames(melt(B_df), c('Day_df','Nodes_df','Biomass'))
head(first_melt)
first_melt$Calen_df=(first_melt$Day_df-1)%%(L.year)+1
first_melt$Year_df=(first_melt$Day_df-1)%/%L.year+1
for (item in length(num.years):1){
  first_melt$Phase_df[first_melt$Year_df <=yr_ls[item]]=item
}
first_melt$yr_in_phase=first_melt$Year_df-c(0,yr_ls)[first_melt$Phase_df]
first_melt[,"Seed"]=c(seed_0)
first_melt[,"Simnum"]=c(simnum)
first_melt[,"Exper"]=c(Exper)



ts.plot(first_melt[first_melt$Nodes_df=="Node_1",7])

first_melt[997:1005,]
first_melt[1:105,]
first_melt[9990:10005,]
# 3 melt data again, but this time they're categorized according to new columns for year, phase, and what not. 
B_df_ncol=dim(B_df)[2]
B_df=cbind(B_df)
B_df_other=melt(B_df, id.vars=c("Year_df","calen_df"), measure.vars =(1:nichewebsize)+2)[,c(3,1,2,4)]
# 4 Extract data - pull out means and vars for each node(or sum of nodes), and 
# We can do this in the second analysis file!








################################################
################## Save Data ###################
################################################
# Last line is Data entry into matrix, because line should only be entered if all data was collected
# It loads it into line k for the data matrix. 
c(seed_0,simnum,Exper)






