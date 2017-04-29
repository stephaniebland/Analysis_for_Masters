rm(list=ls())
k=0
n_webs=10000
for (j in 1:10){
  setwd("/Users/JurassicPark/Documents/Testing code/Fixed_for_morethan3fish")
  n_mass=matrix(0,0,5)
  k_orig=k
  for (i in 1:n_webs){
    k=i+k_orig
    x=paste0("n_mass_",k,".txt")
    temp=read.csv(x,header=F)
    n_mass=rbind(n_mass,temp)
  }
  print(min(levels(factor(n_mass$V1))))
  print(max(levels(factor(n_mass$V1))))
  colnames(n_mass)=c("N","niche","mass","ln_m","log_m","isfish","plant","either","Troph","meta","T1","T2")
  setwd("/Users/JurassicPark/Documents/Testing code/grouped")
  write.csv(n_mass,file=paste0("fixed_data_",(k_orig+1),"_to_",k,".txt"))
}
