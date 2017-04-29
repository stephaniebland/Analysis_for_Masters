rm(list=ls())
detach(neither)
detach(fish_only)
detach(reduced_fish)
detach(no_plants)
library(RColorBrewer)
col_scheme=brewer.pal(8,'Dark2')
#Declare function to standardize niche values
squash=function(x){
  y=x-min(x)
  y=y/max(y)
  return(y)
}
std.fun=function(x){
  y=(x-mean(x))/sd(x)
  return(y)
}
#Function to do a linear regression
lin.regr=function(N,bodymass,nichevalue){
  Mregr=lm(bodymass~nichevalue)
  adjR=summary(Mregr)$adj.r.squared
  R.squar=summary(Mregr)$r.squared
  intercept.regr=summary(Mregr)$coefficients[1,1]
  slope.regr=summary(Mregr)$coefficients[2,1]
  web_num=mean(N)
  beep=cbind(web_num, intercept.regr,slope.regr,R.squar,adjR)
  return(beep)
}
#Import Data - Limited Number
setwd("/Users/JurassicPark/Documents/Testing code/Fixed_for_morethan3fish")
n_mass=matrix(0,0,5)
n_webs=10
for (i in 1:n_webs){
  x=paste0("n_mass_",i,".txt")
  temp=read.csv(x,header=F)
  n_mass=rbind(n_mass,temp)
}
colnames(n_mass)=c("N","niche","mass","ln_m","log_m","isfish","plant","either","Troph","Z","T1","T2")

#Import Data - All
setwd("/Users/JurassicPark/Documents/Testing code")
n_mass=read.csv("fixed_data_everything_multifish.txt",header=T)
head(n_mass)
n_mass=n_mass[,3:14]
colnames(n_mass)=c("N","niche","mass","ln_m","log_m","isfish","plant","either","Troph","Z","T1","T2")

neither=n_mass[n_mass$either==0,]#only use invertebrates
fish_only=n_mass[n_mass$isfish==1,]#only use fish
no_plants=n_mass[n_mass$plant==0,]#fish and invertebrates
####################################################

attach(no_plants)
indep_var=log_m
x=tapply(niche,N,std.fun)
y=tapply(indep_var,N,std.fun)
x=as.vector(unlist(x))
y=as.vector(unlist(y))
A=cbind(x,y,isfish,N)
rm(x,y)
detach(no_plants)
#Reduce it by getting rid of inverts
reduced_fish=A[A[,3]==1,]
reduced_fish=as.data.frame(reduced_fish)
colnames(reduced_fish)=c("xvar","yvar","isfish","N")
attach(fish_only)
indep_var=log_m
x=tapply(niche,N,std.fun)
y=tapply(indep_var,N,std.fun)
x=as.vector(unlist(x))
y=as.vector(unlist(y))

#attach(reduced_fish); x=xvar;y=yvar
M0=lm(y~x)

adjR.M0=summary(M0)$adj.r.squared
adjR.M0
#plot(x,y,col=N,pch=19,ylab="individual body mass",xlab="niche value")

#plot(x,y,col=col_scheme[N],pch=19,ylab="individual body mass",xlab="niche value")
plot(x,y,col=col_scheme[isfish+2],pch=19,ylab="individual body mass",xlab="niche value")
#legend("bottomright",legend=c("hi","hello"))
abline(M0)
#legend(x=.5, y=0.5,legend=c("hi"))


M1=lm(x~y)
adjR.M1=summary(M1)$adj.r.squared
adjR.M1
plot(y,x,col=col_scheme[isfish+2],pch=19,xlab="individual body mass",ylab="niche value")
abline(M1)


detach(neither)
detach(fish_only)
detach(no_plants)
detach(reduced_fish)



#Try again, but with individual linear regressions
attach(fish_only)
indep_var=log_m
x=tapply(niche,N,squash)
y=tapply(indep_var,N,std.fun)
x=as.vector(unlist(x))
y=as.vector(unlist(y))
run_lm=as.data.frame(cbind(N,y,x))

running_lm=by(run_lm, N, function(k) lin.regr(k$N,k$y, k$x))
lm_by_web=matrix(unlist(running_lm),dim(running_lm),length(unlist(running_lm[1])),byrow=T)
colnames(lm_by_web)=c("web","Intercept","slope","R squared","adjusted R squared")
plot(x,y,col=N,pch=19,ylab="individual body mass",xlab="niche value")
silent=by(lm_by_web,lm_by_web[,1],function(k) abline(k$Intercept,k$slope,col=k$web))
#Histograms
hist(lm_by_web[,4],main="Histogram of R squared for mass")
hist(lm_by_web[,5],main="Histogram of adjusted R squared for mass")

detach(fish_only)





