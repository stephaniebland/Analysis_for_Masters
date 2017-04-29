rm(list=ls())
setwd("/Users/JurassicPark/Documents/Testing code/grouped")
n_mass=matrix(0,0,5)
x=dir()
for (i in 1:10){
  temp=read.csv(x[i],header=T)
  n_mass=rbind(n_mass,temp)
}
print(as.numeric(levels(factor(n_mass$N))))
#colnames(n_mass)=c("N","niche","mass","ln_m","log_m","isfish","plant","either","Troph","meta","T1","T2")
setwd("/Users/JurassicPark/Documents/Testing code")
write.csv(n_mass,file=paste0("fixed_data_everything_multifish.txt"))
