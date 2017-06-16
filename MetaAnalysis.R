#---- Hidden ----
library(ggbiplot)
seed_0=0
lifestages_linked=1
Adults_only=0
DATE="2017Jun08"
Version="1"
#simnum=1
#Exper=1
location="/GIT/Analysis"#For Running on my Mac
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
setwd(paste0("~/",location,"/",run_name))
#---- LOAD_DATA ----
alldata=read.table("Melted.txt",header=F)
colnames(alldata)=colnames(melt_B.yr.end)

#---- Functions ----
# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "experiment 1" and experiment 2" because people will definitely forget what that means.
node_names=levels(factor(alldata$Nodes_df))
# In the following line we have some difficulty with node names - after 10 the levels function tries to sort alphabetically but fails miserably.
node_names=cbind(node_names,c("Autotrophs",paste("Life stage",1:4),paste("Fish",1:3),"Fish","Invertebrates",rep("Node",39),"Non Fish","Total Biomass"))
exper_name=c("Life history","New Nodes","No Life history")

# A Quick PCA graphics function
pca_func <- function(dat,nodes,func,grouped,axes,exper_n){
	ls_ret =BLAND(dat,nodes,func,grouped,axes,exper_n)
	response=ls_ret$response
	indep_var=ls_ret$indep_var
	ir.pca <- prcomp(indep_var,center = F, scale. = F) 
	g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = response, ellipse = TRUE, circle = TRUE)
	g <- g + scale_color_discrete(name = '')
	g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
	print(g)
}

# Now we can write some functions to create PCAs or LDAs or whatever you need:
# dat = The data frame you will be inputing
# nodes = The species you will use
# func = mean, logmean, var, logvar....
# grouped = the name you would like to appear in the legend
# Axes = the lines in the center.
# exper_n = list of experiments you want to produce it on
BLAND <- function(dat,nodes,func,grouped,axes,exper_n){
	dat$Nodes_df=factor(dat$Nodes_df)
	test <- dat %>% group_by(Exper, Simnum, Nodes_df) %>% 
		summarise(mean = mean(Biomass),var=var(Biomass)) %>% 
		mutate(logmean = log10(mean + 0.1), logvar=log10(var+.1)) %>% 
		filter(Nodes_df %in% nodes) %>%
		filter(Exper %in% exper_n)
	
	test$Exper=exper_name[test$Exper]
	levels(test$Nodes_df)=node_names[,2]
	tryout<-select_(test, "Exper", "Simnum", "Nodes_df", func) %>% 
		spread_(key = axes, value = func) %>% arrange_(grouped)
	response=factor(tryout[[grouped]])
	indep_var=tryout[,3:ncol(tryout)]
	# Run a PCA: 
	#pca_func(indep_var,response)
	# Run a LDA:
	asdf<-lda(response~.,indep_var)
	lscore = as.matrix(indep_var)%*%asdf$scaling
	# plot(lscore[,1], col=response, ylab="1st canonical variate", main=paste("Discriminant analysis")) # I tried this with random values - it still produces the same ordering so Index is meaningless - just the way we ordered the data in the dataframe.
	# plot(lscore[,1], lscore[,2], col=response, xlab="1st canonical variate", ylab="2nd canonical variate", main=paste("Discriminant analysis using"))
	# legend("topright", 1, pch=1, legend=unique(response), col=unique(response))
	# return(asdf)
	ls_ret=list("response"=(response),"indep_var"=(indep_var))
	return(ls_ret)
}

#BLAND(alldata,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),"logmean")
pca_func(alldata,node_names[grep("Fish_ls_",node_names),1],"logmean","Nodes_df","Exper",1:3)
pca_func(alldata,node_names[grep("Fish_ls_",node_names),1],"logmean","Exper","Nodes_df",1:2)
pca_func(alldata,node_names[grep("Fish_ls_",node_names),1],"logmean","Exper","Nodes_df",1:3)
pca_func(alldata,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),"logmean","Exper","Nodes_df",1:2)
pca_func(alldata,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),"logmean","Exper","Nodes_df",1:3)
pca_func(alldata,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),"logmean","Nodes_df","Exper",1:3)


#---- Plots ----
bland_plot <- function(ie){
	p = matplot(ie,type="l",lwd=3)
	print(p)
	#plot(1:10,type="l")
}

subdat=alldata
dat$Nodes_df=factor(dat$Nodes_df)
test$Exper=exper_name[test$Exper]
ie=alldata %>% 
	filter(Nodes_df %in% "Tot_df") %>%
	filter(Simnum %in% 205) %>%
	select(Simnum,Day_df,Exper,Biomass) %>%
	spread(key=Exper,value=Biomass) %>%
	select(3:5) # %>%
	# do(plot=bland_plot(.))
ie
#plot(ie$Day_df,ie$Biomass,type="l")
matplot(ie,type="l",lwd=3)

do(plot=bland_plot(.))
	
plot_species <- function(species_data){
	p <- qplot(data=species_data,
			   x=Sepal.Length,
			   y=Sepal.Width)
	print(p)
}

(((tapply(melt_B.yr.end$Biomass,list(melt_B.yr.end$Nodes_df,melt_B.yr.end$Phase_df),mean))))



matplot(log10(t(tail(tapply(melt_B.yr.end$Biomass,list(melt_B.yr.end$Nodes_df,melt_B.yr.end$Phase_df),mean)))),type="l",lwd=3, xlab="Phase",ylab="Mean of Biomass (log)")
legend("bottomleft",c("fish adults","Tot_B","fish","nonfish","Basal","inverts"),col=1:6,lty=1:6,lwd=3)





#---- Stats ----
#Find the mean and variance of each group. The variance here is the variance of the time series means. 
alldata %>% group_by(Exper, Simnum, Nodes_df) %>% # Group by simnum too because you want to take the means of the time series means
	summarise(mean = mean(Biomass)) %>% # First step is to take the time series means. 
	group_by(Nodes_df,Exper) %>% 
	summarise (avg=mean(mean),var=var(mean)) %>% # Then we can find the mean and var across simulations
	filter (Nodes_df %in% "Fish_tot_df")



	
	mutate(logmean = log10(mean + 0.1), logvar=log10(var+.1)) %>% 
	filter(Nodes_df %in% nodes) %>%
	filter(Exper %in% exper_n)

#---- Other ----
# Now try Repeated Measures ANOVA
# https://datascienceplus.com/two-way-anova-with-repeated-measures/
subdat=alldata
subdat=subdat[subdat$Phase_df==2,]
subdat=subdat[subdat$Nodes_df=="Fish_tot_df",]
myData.mean <- aggregate(subdat$Biomass,by=list(subdat$Simnum,subdat$Exper), FUN = 'mean')
colnames(myData.mean) <- c("Simnum_a","Exper_a","Biomass_a")
myData.mean <- myData.mean[order(myData.mean$Simnum_a), ]
head(myData.mean)
stress.aov <- with(myData.mean,aov(Biomass_a~Exper_a +Error(Simnum_a / (Exper_a))))
summary(stress.aov)
# Ok so no differences yet. Let's look at extinctions instead.
subdat=alldata
subdat=subdat[subdat$Year_df==max(subdat$Year_df),] # Only use data once it's stable; assume all plots reach stability (no multi-year cycles)
subdat=subdat[grep("Node_",subdat$Nodes_df),] # Only look at individual nodes
subdat=subdat[subdat$Biomass!=0,] # Find all non-extinct species
head(subdat)
dim(subdat)
extant_counts=tapply(subdat$Nodes_df, list(subdat$Simnum,subdat$Exper), function(x) length(unique(x))) 
extant_counts[is.na(extant_counts)]=0
colMeans(extant_counts)
diag(var(extant_counts))
melt_counts=melt(extant_counts); colnames(melt_counts)=c("Simnum_b","Exper_b","Count_extant")
head(melt_counts)
stress.aov <- with(melt_counts,aov(Count_extant~Exper_b +Error(Simnum_b / (Exper_b))))
summary(stress.aov)
#Now try the same thing, but just with fish species
subdat=alldata
subdat=subdat[subdat$Year_df==max(subdat$Year_df),] # Only use data once it's stable; assume all plots reach stability (no multi-year cycles)
subdat=subdat[grep("Fish_sp_",subdat$Nodes_df),] # Only look at individual nodes
subdat=subdat[subdat$Biomass!=0,] # Find all non-extinct species
head(subdat)
dim(subdat)
extant_counts=tapply(subdat$Nodes_df, list(subdat$Simnum,subdat$Exper), function(x) length(unique(x))) 
extant_counts[is.na(extant_counts)]=0
colMeans(extant_counts)
diag(var(extant_counts))
melt_counts=melt(extant_counts); colnames(melt_counts)=c("Simnum_b","Exper_b","Count_extant")
head(melt_counts)
stress.aov <- with(melt_counts,aov(Count_extant~Exper_b +Error(Simnum_b / (Exper_b))))
summary(stress.aov)
tapply(melt_counts$Count_extant,melt_counts$Exper_b,mean)
library(dplyr)
melt_counts %>% group_by(Exper_b) %>% summarise(mean = mean(Count_extant))
# Now try between the first two experiments
first_two_Exper=melt_counts
first_two_Exper=first_two_Exper[first_two_Exper$Exper_b<3,]
stress.aov <- with(first_two_Exper,aov(Count_extant~Exper_b +Error(Simnum_b / (Exper_b))))
summary(stress.aov)







