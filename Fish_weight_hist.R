rm(list=ls())
setwd("/Users/JurassicPark/Google Drive/GIT/Analysis")
library(R.matlab)
library(matrixStats)
library(RColorBrewer)
library(codyn)
library(knitr)
library(reshape2)
library(dplyr)

N=200
fish_weights=matrix(NA,N,30)
fish_weights_scaled=matrix(NA,N,30)

for (i in 1:N){
  sim_data=readMat(cat("setup_",i,".mat",sep=""))
  #names(sim_data)
  attach(sim_data)
  num_fish=length(which(lifestage==4))
  fish_weights_vector=Mass[which(lifestage==4)]
  fish_weights[i,1:num_fish]=fish_weights_vector
  fish_weights_scaled[i,1:num_fish]=fish_weights_vector/W.scalar
  detach(sim_data)
}


write.csv(fish_weights,file = "fish_weights.csv")
write.csv(fish_weights_scaled,file = "fish_weights_scaled.csv")

fish_weights=read.csv("fish_weights.csv")
fish_weights_scaled=read.csv("fish_weights_scaled.csv")
fish_weights=as.matrix(fish_weights)[,-1]
fish_weights_scaled=as.matrix(fish_weights_scaled)[,-1]

hist(log10(fish_weights))
hist(log10(fish_weights_scaled))
