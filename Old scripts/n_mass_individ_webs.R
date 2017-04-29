rm(list=ls())
detach(neither)
setwd("/Users/JurassicPark/Google Drive/GIT/Masters Project/Testing Code Files/Niche_metabolic_rate")
#n_mass=matrix(,0,5)
N=1000#10000
adjR_m=R_sq_m=adjR_Z=R_sq_Z=adjR_T=R_sq_T=rep(0,N)

for (i in 1:N){
  x=paste0("n_mass_",i,".txt")
  n_mass=read.csv(x,header=F)
  colnames(n_mass)=c("niche","mass","fish","plant","either","Trophic","Metabolic")
  neither=n_mass[n_mass$either==0,]
  neither$mass=log10(neither$mass)
  neither$Metabolic=log10(neither$Metabolic)
  attach(neither)
  #plot(niche,mass)
  M0=lm(mass~niche)
  # MZ=lm(Metabolic~niche)
  # MT=lm(Trophic~niche)
  #summary(M0)
  adjR_m[i]=summary(M0)$adj.r.squared
  R_sq_m[i]=summary(M0)$r.squared
  # adjR_Z[i]=summary(MZ)$adj.r.squared
  # R_sq_Z[i]=summary(MZ)$r.squared
  # adjR_T[i]=summary(MT)$adj.r.squared
  # R_sq_T[i]=summary(MT)$r.squared
  #adjR.M0
  #abline(M0)
  detach(neither)
}
#adjR
#R_sq
hist(adjR_m,main="Histogram of adjusted R squared for mass")
hist(R_sq_m,main="Histogram of R squared for mass")
mean(adjR_m)
# hist(adjR_Z,main="Histogram of adjusted R squared for metabolic rate")
# hist(R_sq_Z,main="Histogram of R squared for metabolic rate")
# mean(adjR_Z)
# hist(adjR_Z,main="Histogram of adjusted R squared for trophic level")
# hist(R_sq_Z,main="Histogram of R squared for trophic level")
# mean(adjR_T)
#setwd("/Users/JurassicPark/Google Drive/GIT/Masters Project/Testing Code Files")
#write.csv(adjR,file=paste0("adjR_",N,".txt"))
#write.csv(R_sq,file=paste0("Rsq_",N,".txt"))


