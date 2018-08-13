#//////////////////////////////////////////////////////////////////////////
#----Appendix Analysis----
# Created by Stephanie Bland
# Prepping Publication Version
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

memory.limit(70000)
#library(ggbiplot)
library(tidyverse)
library(stringr)
library(knitr)
start_fig=5
z=0

## ----load_data-----------------------------------------------------------
DATE="2018May03"
Version="1"
#location="/GIT/Analysis"#For Running on my Mac
location="C:/Users/Stephanie/Desktop"#For Running on Windows
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
#setwd(paste0("~/",location,"/",run_name))
setwd(paste0("",location,"/",run_name))
pardefault <- par()
#---- LOAD_DATA ----
dat=read.table(paste0("clean_",run_name,".txt"),header=F)
# Load Data in chunks
	#file_in    <- file(paste0("clean_",run_name,".txt"),"r")
	#chunk_size <- 100000 # choose the best size for you
	#x=c()
	#repeat{
	#	myLines <- readLines(file_in, n=chunk_size)
	#	if (length(myLines) == 0) break
	#	myLines=do.call(rbind,strsplit(myLines,' ',fixed=T))
	#	x=rbind(x,myLines)
	#}
	#
	## Convert to a dataframe
	#dat <- setNames(
	#		as.data.frame(lapply(1:ncol(x), function(i) {
	#			type.convert(x[,i])}), stringsAsFactors = FALSE), 
	#		paste0('v', 1:ncol(x)))

# Load the column names
colnames(dat)=as.matrix(read.table(paste0("colnames_clean_",run_name,".txt")))
colnames(dat)[9]="Model"
# Replace NA values (mistake -> they go to 0)
dat[dat$simnum==2401 & dat$Nodes_df==21 & dat$Year_df>max(dat$Year_df)-6 & dat$Model==2 & is.na(dat$Biomass),"Biomass"]=0
# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "model 1" and model 2" because people will definitely forget what that means.
exper_name=c("Original Web","Extended Web","Leslie & History")
# Make Processing faster, I think? - DONT USE THIS YET - IT WILL BREAK THINGS!
# dat=dat %>% mutate_at(c("isfish","basal_ls"),as.logical) %>%
#	mutate_at(c("Phase_df","Nodes_df","Seed","Model","pred","prey","species","lifestage"),as.factor)

## ----SAVE RESULTS IN NEW FOLDER------------------------------------------
setwd(paste0("",location,"/",run_name,"/RESULTS_Publication2018Aug1"))

## ----DefineMultiplot-----------------------------------------------------
# From https://stackoverflow.com/a/24387436
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	require(grid)

	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)

	numPlots = length(plots)

	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
						 ncol = cols, nrow = ceiling(numPlots/cols))
	}

	if (numPlots==1) {
		print(plots[[1]])

	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
											layout.pos.col = matchidx$col))
		}
	}
}

## ----CV_total, fig.cap=cap-----------------------------------------------
# Coefficient of Variation Function
CV <- function(dat){sd(dat)/mean(dat)*100}
sem <- function(x) {sd(x,na.rm=T)/sqrt(sum(is.na(x)))} # Standard error

## ----Percent_Stable, include=FALSE---------------------------------------
# Subset the data that fit criteria 1 and 2
subdat_ls=dat %>% filter(Year_df==max(Year_df),isfish==1) %>% 
	group_by(simnum,Model) %>%
	summarise(Tot_species=sum(Biomass)) %>% # But now it needs to survive in ALL models
	summarise(any=sum(Tot_species),all=prod(Tot_species)) %>%
	mutate_at(c("any","all"),as.logical)
persist=subdat_ls %>% summarise_at(c("any","all"),mean)*100
persist # Percent of webs that meet criteria 1 and 2.
# Criteria 1: Probability of fish persisting in at least one of the models
# Criteria 2: Probability of fish persisting in all of the models
subdat1=dat %>% filter(simnum %in% (subdat_ls %>% filter(any==TRUE))$simnum)
subdat2=dat %>% filter(simnum %in% (subdat_ls %>% filter(all==TRUE))$simnum)

## ----Von-Bert-Multi, fig.cap=cap-----------------------------------------
# Caution: The clunky formatting here is because we need to plot ALL life stages if a single life stage survives. This way you won't end up with a partial line between two life stages
postscript(paste0("Figure",1+start_fig,"_Von-Bert-Multi.eps"),horiz=FALSE,width=8.5,height=11)

subdat1 %>% filter(simnum<20,Model==3,Year_df==max(Year_df),isfish==1) %>%
	group_by(simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	filter(tot_fish_biom>0) %>% # Can't filter by Biomass>0 because some life stages go extinct and you end up with incomplete curves
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	#select(simnum,species,Biomass, tot_fish_biom,Mass)
	ggplot(aes(x=lifestage, y=log10(Mass), linetype=simnum)) + geom_line(aes(group=species)) + labs(x="Fish life stage",y="log of individual body mass", linetype="Simulation") + theme_bw() + theme(text = element_text(size = 14))

z=z+1;VBmult=z
cap=paste("Figure",VBmult,"Von-Bertalanffy curves for surviving fish in several simulated food webs. Each colour represents a different food web.")
dev.off()

## ----Mass_Overlap, fig.cap=cap-------------------------------------------
mass_overlap=dat %>% filter(Year_df==1,Model==1,isfish==1,lifestage %in% range(lifestage)) %>%
	select(simnum,species,lifestage,Mass) %>%
	group_by(simnum) %>%
	spread(key=lifestage,value=Mass) %>%
	summarise(youngest_large=max(`1`),oldest_small=min(`4`)) %>%
	mutate(range_overlap=youngest_large<oldest_small,size_ratio=oldest_small/youngest_large)
percent_range_overlap=mass_overlap %>% summarise(100*sum(range_overlap)/n())
percent_range_overlap # Percentage of webs where the oldest life stage of the smallest fish is larger than the youngest life stage of the largest fish.

## ----freq_extant_fish, fig.cap=cap---------------------------------------
freq_extant_fish=dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) %>%
	ungroup() %>% mutate(Model=as.factor(Model)) %>%
	group_by(Num_extant,Model) %>%
	summarise(n=n()) %>% group_by(Model) %>%
	mutate(Freq=n/sum(n))

postscript(paste0("Figure",5+start_fig,"_freq_extant_fish.eps"),horiz=FALSE,width=8.5,height=11)
freq_extant_fish %>% ggplot(., aes(x=Num_extant, y=Freq)) + geom_point(aes(group=Model, color=Model, shape=Model), size=5, stroke=2)+ labs(x="Number of surviving fish species", y="Frequency of simulations") + theme_bw() + theme(text = element_text(size = 14)) + scale_shape_manual(values=c(2,0,20))

z=z+1;freq_ex_fish=z
cap=paste("Figure",freq_ex_fish,"The frequency of fish surviving in each model.")
dev.off()

#//////////////////////////////////////////////////////////////////////////
#----Setup Life History Correlations----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Change the Model being plotted with Test_Model (if you alter this you need to re-run everything below this line to properly update the graphs)
for (Test_Model in 1:3){
# Also setup plots where you're looking at individual surviving fish species, and comparing their life history stats to that specific species' biomass.
# Where it's by species W_infty against (that same species') final biomass and CV
CV_spec_stats=subdat2 %>% filter(Phase_df==2,Model==Test_Model) %>%
	group_by(simnum,Model,species,Year_df) %>%
	summarise(Tot_spec=sum(Biomass)) %>%
	summarise(CV_spec=CV(Tot_spec),mean_spec=mean(Tot_spec))

CV_tot_stats=subdat2 %>% filter(Phase_df==2,Model==Test_Model) %>%
	group_by(simnum,Model,Year_df) %>%
	summarise(Tot_Bio=sum(Biomass)) %>%
	summarise(CV_tot=CV(Tot_Bio),mean_tot=mean(Tot_Bio))

gen_spec_stats=subdat2 %>% filter(Year_df==max(Year_df),Model==Test_Model) %>%
	group_by(simnum,Model,species) %>%
	mutate(Tot_spec=sum(Biomass),max_Z=max(Z),max_Mass=max(Mass)) %>%
	filter(Tot_spec>0,isfish==1,lifestage==1)

all_spec_stats=left_join(gen_spec_stats,CV_spec_stats)
all_spec_stats=left_join(all_spec_stats,CV_tot_stats) %>% 
	mutate(log_spec=log10(mean_spec),log_tot=log10(mean_tot),log_max_mass=log10(max_Mass)) %>%
	filter(!(simnum==11 && species==30), CV_spec<800, max_Mass<1e+10) # Remove outliers!!


#//////////////////////////////////////////////////////////////////////////
#----Plot Life History Correlations----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Start by creating plot_relations function.

### OUTPUTS: lm_test, corr_test, ls_graphs and cor_val_printout
# Change the max number of figures (n_fig) and subplots (n_plot)
n_fig=5 # Max number of figures
n_plot=4 # Max number of subplots
# Linear regression p-values
lm_test=matrix(NA,n_fig,n_plot)
# cor.test p-values
corr_test=matrix(NA,n_fig,n_plot)
# Graphs, saved as list where ls_graphs[[x]][y] is the yth subplot of figure x
ls=vector("list",n_plot)
ls_graphs=rep(list(ls),n_fig)
# cor.test printout, saved as list where cor_val_printout[[x]][[y]] is the data for the yth subplot of figure x
ls2=vector("list",1)
ls2=rep(list(ls2),n_plot)
cor_val_printout=rep(list(ls2),n_fig)

# Allow us to modify global variables within a function
envir <- globalenv()

### INPUTS:
# xk_fig  >  Figure number (so all xk_fig=1 will be different subplots of the same figure)
# xk_plot >  Subplot number within the figure (so quadrant a b c or d in a figure)
# dat     >  Data set used to generate this plot. Choose between full_stats (data set for the largest surviving fish species), or all_spec_stats (data set contains every surviving fish species).
# xvar    >  x variable
# yvar    >  y variable
# xlab    >  x label
# ylab    >  y label
plot_relations <- function(xk_fig,xk_plot,dat,xvar,yvar,xlab,ylab){
	dat = dat %>% 
		mutate_(xvar2=xvar,yvar2=yvar)
	# Correlation Values
	cor_val=cor.test(dat$xvar2,dat$yvar2)
	cor_val$data.name=paste(xlab,"and",ylab)
	cor_vals=paste0("t=",round(cor_val$statistic,2),", df=",cor_val$parameter,", p=",round(cor_val$p.value,3))
	cor_pval=cor_val$p.value
	# lm values
	mod=lm(yvar2~xvar2,dat)
	lm_pval=summary(mod)$coefficients[2,"Pr(>|t|)"]
	# Significance Indicator
	# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
	sig_val=case_when(
		cor_pval <= 0.001 ~ "***",
		cor_pval <= 0.01 ~ "**",
		cor_pval <= 0.05 ~ "*",
		cor_pval <= 0.1 ~ ".",
		cor_pval > 0.1 ~ " "
	)
	# Plot the graph
	graph=dat %>% 
		ggplot(aes(x=xvar2, y=yvar2)) + 
		geom_point() + 
		labs(x=xlab,y=ylab,title=paste0(letters[xk_plot],sig_val)) + 
		geom_smooth(method = "lm") + 
		theme_bw() + 
		theme(text = element_text(size = 14))
	# Return values
	envir[[ "corr_test" ]][xk_fig,xk_plot] <- cor_vals
	envir[[ "lm_test" ]][xk_fig,xk_plot] <- lm_pval
	envir[[ "ls_graphs" ]][[xk_fig]][xk_plot] <- list(graph)
	envir[[ "cor_val_printout" ]][[xk_fig]][[xk_plot]] <- print(cor_val)
	# And some data to be returned while running analysis
	qqplot(dat$xvar2,dat$yvar2) # Check Normality
	return(cor_val)
	#return(graph)
}
# Example use:
#plot_relations(1,1,iris,quo(Sepal.Length),quo(Sepal.Width),"Allometric Ratio","CV of total biomass")

plot_relations(4,1,all_spec_stats,quo(log10(max_Mass)),quo((mean_tot)),"log of fish asymptotic body mass","mean total biomass") #updated
plot_relations(4,2,all_spec_stats,quo(log10(max_Mass)),quo((mean_spec)),"log of fish asymptotic body mass","mean fish biomass") #updated
plot_relations(4,3,all_spec_stats,quo(log10(max_Mass)),quo((CV_tot)),"log of fish asymptotic body mass","CV of total biomass")
plot_relations(4,4,all_spec_stats,quo(log10(max_Mass)),quo((CV_spec)),"log of fish asymptotic body mass","CV of fish biomass")

postscript(paste0("Figure",8+start_fig,"_Model",Test_Model,"_row4_all_stats2.eps"),horiz=FALSE,width=8.5,height=11)
multiplot(plotlist=ls_graphs[[4]],cols=2)
dev.off()

#//////////////////////////////////////////////////////////////////////////
#----Save Model Output----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
write.csv(lm_test, file = paste0("lm_test_Model",Test_Model,".csv"),row.names=FALSE, na="",quote=T)
write.csv(corr_test, file = paste0("corr_test_Model",Test_Model,".csv"),row.names=FALSE, na="",quote=T)
sink(paste0("cor_val_printout_Model",Test_Model,".txt"))
print(cor_val_printout)
sink()  # returns output to the console
}

#//////////////////////////////////////////////////////////////////////////
#----CV Boxplot for Fish & Total Biomass across model types----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

postscript(paste0("Figure",4+start_fig,"_CV_boxplot.eps"),horiz=FALSE,width=8.5,height=11)
par(mfrow=c(2,1), mai = c(0.7, 1, 0.5, 0.1),mgp=c(2,1,0))
boxplot(CV_tot~Model,CV_plot,xlab="Model Type",ylab="Coefficient of Variation",main="Total Ecosystem Biomass",ylim=c(0,100))

boxplot(CV_fish~Model,CV_plot,xlab="Model Type",ylab="Coefficient of Variation",main="Total Fish Biomass",ylim=c(0,100))
dev.off()
par(pardefault)



