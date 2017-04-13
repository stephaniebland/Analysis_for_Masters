# Cluster R script
# Stephanie Bland
# Analysis of Data in the Cluster

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












################################################
################## Save Data ###################
################################################
# Last line is Data entry into matrix, because line should only be entered if all data was collected
# It loads it into line k for the data matrix. 







