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

alldata=read.table("Melted.txt",header=F)
colnames(alldata)=colnames(melt_B.yr.end)


# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "experiment 1" and experiment 2" because people will definitely forget what that means.
node_names=levels(factor(alldata$Nodes_df))
# In the following line we have some difficulty with node names - after 10 the levels function tries to sort alphabetically but fails miserably.
node_names=cbind(node_names,c("Autotrophs",paste("Life stage",1:4),paste("Fish",1:3),"Fish","Invertebrates",rep("Node",39),"Non Fish","Total Biomass"))
exper_name=c("Life history","New Nodes","No Life history")

# A Quick PCA graphics function
pca_func <- function(data,group_by){
	ir.pca <- prcomp(data,center = F, scale. = F) 
	g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = group_by, ellipse = TRUE, circle = TRUE)
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
	test <- dat %>% group_by(Exper, Simnum, Nodes_df) %>% 
		summarise(mean = mean(Biomass),var=var(Biomass)) %>% 
		mutate(logmean = log10(mean + 0.1), logvar=log10(var+.1)) %>% 
		filter(Nodes_df %in% nodes) %>%
		filter(Exper %in% exper_n)
	
	test$Exper=exper_name[test$Exper]
	test$Nodes_df=factor(test$Nodes_df)
	levels(test$Nodes_df)=node_names[,2]
	tryout<-select_(test, "Exper", "Simnum", "Nodes_df", func) %>% 
		spread_(key = axes, value = func) %>% arrange_(grouped)
	response=factor(tryout[[grouped]])
	indep_var=tryout[,3:ncol(tryout)]
	# Run a PCA: 
	pca_func(indep_var,response)
	# Run a LDA:
	asdf<-lda(response~.,indep_var)
	lscore = as.matrix(indep_var)%*%asdf$scaling
	plot(lscore[,1], col=response, ylab="1st canonical variate", main=paste("Discriminant analysis")) # I tried this with random values - it still produces the same ordering so Index is meaningless - just the way we ordered the data in the dataframe. 
	plot(lscore[,1], lscore[,2], col=response, xlab="1st canonical variate", ylab="2nd canonical variate", main=paste("Discriminant analysis using"))
	legend("topright", 1, pch=1, legend=unique(response), col=unique(response))
	return(asdf)
}

#BLAND(alldata,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),"logmean")
BLAND(alldata,node_names[grep("Fish_ls_",node_names),1],"logmean","Nodes_df","Exper",1:3)
BLAND(alldata,node_names[grep("Fish_ls_",node_names),1],"logmean","Exper","Nodes_df",1:2)
BLAND(alldata,node_names[grep("Fish_ls_",node_names),1],"logmean","Exper","Nodes_df",1:3)
BLAND(alldata,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),"logmean","Exper","Nodes_df",1:2)
BLAND(alldata,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),"logmean","Exper","Nodes_df",1:3)
BLAND(alldata,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),"logmean","Nodes_df","Exper",1:3)




