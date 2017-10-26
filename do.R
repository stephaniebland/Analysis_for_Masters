#---- Hidden ----
library(ggbiplot)
library(tidyverse)
library(MASS)
DATE="2017Jul19"
Version="0"
location="/GIT/Analysis"#For Running on my Mac
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
setwd(paste0("~/",location,"/",run_name))
#---- LOAD_DATA ----
backupdata=read.table("Melted.txt",header=F)
colnames(backupdata)=as.matrix(read.table("colnames.txt"))
alldata=backupdata
alldata=alldata[alldata$Prey==0,]
alldata=alldata[alldata$Pred==2,]
alldata=alldata[alldata$Phase_df==2,]

#---- LOAD_DATA ----
# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "experiment 1" and experiment 2" because people will definitely forget what that means.
node_names=levels(factor(alldata$Nodes_df))
# In the following line we have some difficulty with node names - after 10 the levels function tries to sort alphabetically but fails miserably.
node_names=cbind(node_names,c("Autotrophs",paste("Life stage",1:4),paste("Fish",1:3),"Fish","Invertebrates",rep("Node",39),"Non Fish","Total Biomass"))
exper_name=c("Leslie & History","Extended Web","Original Web")

#---- Percent_stable ----
# Find the total number of webs that reach stability
# Definition of phases: There are four phases in my general code design: 
#	1. Discards - time for it takes for web to stabilize 
#	2. Pristine Scenario - No fishing
#	The second two phases are turned off for this analysis, but will be turned on for the next paper.
#	3. Fishing Scenario
#	4. Recovery Period
# This analysis will only look at the Pristine scenario, so phase 2. 
# Okay, so clearly the first problem is how do we want to define "stability"? For the purpose of this analysis, I'll define it as the persistance of life (any or fish specifically) till the end of the simulation (so there is life at the end of the second phase)
# I will group these values by Experiment, and have a mean and variance so I can make bar plots. 
# So two sections are necessary:
# 1. What percentage of webs have at least one fish life stage (node) persist in ANY of the experiments?
# 2. What percentage of webs have at least some fish biomass persist in every experiment. 
# Also interesting, but not my focus:
# What percentage of webs have any life at all (without grouping by simulation)?
# Should I also make a general stat of how many webs have at least one life stage persist in all three experiments? 
nodes="Fish_tot_df"
persist=alldata %>% group_by(Simnum, Exper, Nodes_df) %>%
	filter(Year_df %in% max(Year_df), Nodes_df %in% nodes) %>%
	spread(key=Exper, value=Biomass) %>%
	summarise(any=as.logical(sum(`1`,`2`,`3`)),all=as.logical(prod(`1`,`2`,`3`)))
# 1.
sum(persist$any)/length(persist$any)
# 2.
sum(persist$all)/length(persist$all)
	












