#//////////////////////////////////////////////////////////////////////////
#----Appendix Analysis----
# Created by Stephanie Bland
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#memory.limit(70000)
#library(ggbiplot)
library(tidyverse)
library(stringr)
library(knitr)
start_fig=5
z=0

## ----load_data-----------------------------------------------------------
DATE="2017Nov28"
Version="0"
location="/GIT/Analysis"#For Running on my Mac
#location="C:/Users/Stephanie/Desktop"#For Running on Windows
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
setwd(paste0("~/",location,"/",run_name))
#setwd(paste0("",location,"/",run_name))
pardefault <- par()
#---- LOAD_DATA ----
dat=read.table("clean.txt",header=F)
colnames(dat)=as.matrix(read.table("colnames_clean.txt"))
colnames(dat)[9]="Model"
# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "model 1" and model 2" because people will definitely forget what that means.
exper_name=c("Original Web","Extended Web","Leslie & History")
# Make Processing faster, I think? - DONT USE THIS YET - IT WILL BREAK THINGS!
# dat=dat %>% mutate_at(c("isfish","basal_ls"),as.logical) %>%
#	mutate_at(c("Phase_df","Nodes_df","Seed","Model","pred","prey","species","lifestage"),as.factor)

## ----SAVE RESULTS IN NEW FOLDER------------------------------------------
setwd(paste0("",location,"/",run_name,"/RESULTS"))

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
	ggplot(aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum)) + labs(x="Fish life stage",y="log of individual body mass") + theme_bw() + theme(text = element_text(size = 14))

z=z+1;VBmult=z
cap=paste("Figure",VBmult,"Von-Bertalanffy curves for surviving fish in several simulated food webs. Each colour represents a different food web.")
dev.off()

## ----Von-Bert-Multi2, fig.cap=cap----------------------------------------
VB_orig_fish=subdat1 %>% filter(simnum<20,Model==3,Year_df==max(Year_df),isfish==1) %>%
	group_by(simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	#select(simnum,species,Biomass, tot_fish_biom,Mass)
	ggplot(aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum)) + labs(x="Fish life stage",y="log of individual\n body mass") + theme_bw() + theme(legend.key.height=unit(0.5,"line"))

# Caution: The clunky formatting here is because we need to plot ALL life stages if a single life stage survives. This way you won't end up with a partial line between two life stages
VB_end_fish=subdat1 %>% filter(simnum<20,Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	filter(tot_fish_biom>0) %>% # Can't filter by Biomass>0 because some life stages go extinct and you end up with incomplete curves
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	group_by(Model) %>%
	do(g=ggplot(.,aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum))  + labs(x="Fish life stage",y="log of individual\n body mass") + theme_bw() + theme(legend.key.height=unit(0.5,"line")))

## ----VB-Hist-------------------------------------------------------------
VB_orig=dat %>% filter(Year_df==1,Model==1) %>%
	group_by(simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)
VB_hist_orig = VB_orig %>% ggplot(.,aes(Z))+geom_histogram() + coord_cartesian(xlim=c(min(VB_orig$Z),max(VB_orig$Z))) + labs(x="Allometric Ratio" + theme_bw())

VB_hist=subdat1 %>% filter(Year_df==max(Year_df),Biomass>0) %>%
	group_by(Model,simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)

## ----VB_Exper_compare, fig.cap=cap,echo=FALSE----------------------------
meh=VB_hist %>% group_by(Model) %>%
	do(k=ggplot(.,aes(Z))+geom_histogram()+coord_cartesian(xlim=c(min(VB_orig$Z),max(VB_orig$Z))) + labs(x="Allometric Ratio") + theme_bw())

postscript(paste0("Figure",2+start_fig,"_VB_Exper_compare.eps"),horiz=FALSE,width=8.5,height=11)
multiplot(VB_orig_fish, VB_end_fish$g[[1]], VB_end_fish$g[[2]], VB_end_fish$g[[3]],
		  VB_hist_orig, meh$k[[1]], meh$k[[2]], meh$k[[3]], cols=2)

z=z+1;VB_Exper_compare=z
cap=paste("Figure",VB_Exper_compare,"A histogram of the allometric ratios for all the surviving adult fish life stages.")
dev.off()

## ----Mass_Overlap, fig.cap=cap-------------------------------------------
mass_overlap=dat %>% filter(Year_df==1,Model==1,isfish==1,lifestage %in% range(lifestage)) %>%
	select(simnum,species,lifestage,Mass) %>%
	group_by(simnum) %>%
	spread(key=lifestage,value=Mass) %>%
	summarise(youngest_large=max(`1`),oldest_small=min(`4`)) %>%
	mutate(range_overlap=youngest_large<oldest_small,size_ratio=oldest_small/youngest_large)
percent_range_overlap=mass_overlap %>% summarise(100*sum(range_overlap)/n())

postscript(paste0("S",2,"_AllometricOverlap.eps"),horiz=FALSE,width=8.5,height=11)
hist(log10(mass_overlap$size_ratio),main="",xlab="Allometric ratio of the smallest fish adult\n to the youngest life stage of the largest fish (log10)")

z=z+1;mass_overlap_cap=z
cap=paste("Supplementary Figure",mass_overlap_cap,"A histogram of the logged allometric ratios between the oldest life stage of the smallest fish species and the youngest life stage of the largest fish species for any model.")
dev.off()

## ----TS_solo, fig.cap=cap------------------------------------------------
postscript(paste0("Figure",3+start_fig,"_TS_solo.eps"),horiz=FALSE,width=8.5,height=11)
subdat2 %>% filter(Model==3,simnum==unique(simnum)[4]) %>%
	mutate(lifestage=as.factor(lifestage),species=as.factor(isfish*species)) %>%
	group_by(Year_df,lifestage,species) %>%
	mutate(Biomass=sum(Biomass)) %>% ungroup() %>% mutate(species=c("Other","Fish 1","Fish 2","Fish 3")[species]) %>%
	ggplot(aes(x=Year_df,y=log10(Biomass))) + geom_line(aes(group=Nodes_df, colour=species,linetype=lifestage)) + labs(x="Year",y="Biomass (log 10)") + theme_bw() + labs(colour="Species", linetype='Life stage') + theme(text = element_text(size = 14))

z=z+1;TSsolo=z
cap=paste("Figure",TSsolo,"A typical time series for model 1. This shows the logged biomass at the end of each year cycle for each fish life stage along with the combined biomass of the rest of the ecosystem.")
dev.off()

## ----freq_extant_fish, fig.cap=cap---------------------------------------
xkcd=dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) %>%
	ungroup() %>% mutate(Model=as.factor(Model)) %>%
	group_by(Num_extant,Model) %>%
	summarise(n=n()) %>% group_by(Model) %>%
	mutate(Freq=n/sum(n))

xkcd1=xkcd %>% 
	ggplot(., aes(x=Num_extant, y=Freq)) + geom_point(aes(group=Model, color=Model), size=3)+ labs(x="Number of surviving fish species", y="Frequency of simulations") + theme_bw() + theme(text = element_text(size = 14))
xkcd2=xkcd %>% filter(Num_extant %in% range(Num_extant)) %>%
	mutate(Num_extant=as.factor(Num_extant)) %>%
	ggplot(., aes(x=Num_extant, y=Freq)) + geom_point(aes(group=Model, color=Model), size=3)+ labs(x="Number of surviving fish species", y="Frequency of simulations")+ scale_x_discrete(labels=c("None","All")) + theme_bw() + theme(text = element_text(size = 14))
postscript(paste0("Figure",5+start_fig,"_freq_extant_fish.eps"),horiz=FALSE,width=8.5,height=11)
multiplot(xkcd1,xkcd2)

z=z+1;freq_ex_fish=z
cap=paste("Figure",freq_ex_fish,"The frequency of fish surviving in each model.")
dev.off()

#//////////////////////////////////////////////////////////////////////////
#----Setup Life History Correlations----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
Test_Model=3
# First setup plots where you just find life his stats for largest surviving fish species and compare them to the tot fish biomass (all species combined) and total biomass
sim_stats=subdat2 %>% filter(Year_df==max(Year_df),Model==Test_Model) %>%
	group_by(simnum,Model,species) %>% 
	mutate(Tot_spec=sum(Biomass)) %>%
	filter(Tot_spec>0) %>% # Important: Filter out extinct species first so you only get stats for surviving species
	group_by(simnum,Model) %>% 
	summarise(Tot_Bio=sum(Biomass),Tot_fish=sum(Biomass*isfish),max_Z=max(Z),max_Mass=max(Mass),max_fish_mass=max(Mass*isfish))

CV_plot=subdat2 %>% group_by(Model,simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_bio=sum(Biomass),Tot_fish=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_bio),CV_fish=CV(Tot_fish),mean_tot=mean(Tot_bio),mean_fish=mean(Tot_fish))

full_stats=left_join(sim_stats,CV_plot) %>% 
	mutate(log_tot=log10(mean_tot),log_fish=log10(mean_fish),log_max_mass=log10(max_Mass),log_max_fish_mass=log10(max_fish_mass),FT_ratio=mean_fish/mean_tot)


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
	filter(!(simnum==11 && species==30)) # Remove that outlier!!


#//////////////////////////////////////////////////////////////////////////
#----Plot Life History Correlations----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

lm_test=matrix(NA,5,4)
corr_test=matrix(NA,5,4)
ls=vector("list",4)
ls_graphs=list(ls,ls,ls,ls,ls)
cor_val_printout=list(ls,ls,ls,ls,ls)

envir <- globalenv()

# xk_fig # The figure (in relation to all these figures)
# xk_plot # The plot number within the figure
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
		labs(x=xlab,y=ylab,title=paste0(c("a","b","c","d")[xk_plot],sig_val)) + 
		geom_smooth(method = "lm") + 
		theme_bw() + 
		theme(text = element_text(size = 14))
	# Return values
	envir[[ "corr_test" ]][xk_fig,xk_plot] <- cor_vals
	envir[[ "lm_test" ]][xk_fig,xk_plot] <- lm_pval
	envir[[ "ls_graphs" ]][[xk_fig]][xk_plot] <- list(graph)
	envir[[ "cor_val_printout" ]][[(xk_fig-1)*4+xk_plot]] <- print(cor_val)
	# And some data to be returned while running analysis
	qqplot(dat$xvar2,dat$yvar2) # Check Normality
	return(cor_val)
	#return(graph)
}
# Example use:
#plot_relations(1,1,iris,quo(Sepal.Length),quo(Sepal.Width),"Allometric Ratio","CV of total biomass")

plot_relations(1,1,full_stats,quo(max_Z),quo(log10(mean_tot)),"Allometric Ratio","log of total biomass")
plot_relations(1,2,full_stats,quo(max_Z),quo(log10(mean_fish)),"Allometric Ratio","log of fish biomass")
plot_relations(1,3,full_stats,quo(max_Z),quo(CV_tot),"Allometric Ratio","CV of total biomass")
plot_relations(1,4,full_stats,quo(max_Z),quo(CV_fish),"Allometric Ratio","CV of fish biomass")

plot_relations(2,1,full_stats,quo(log10(max_fish_mass)),quo(mean_tot),"log of fish asymptotic body mass","total biomass") #updated
plot_relations(2,2,full_stats,quo(log10(max_fish_mass)),quo(mean_fish),"log of fish asymptotic body mass","fish biomass") #updated
plot_relations(2,3,full_stats,quo(log10(max_fish_mass)),quo(CV_tot),"log of fish asymptotic body mass","CV of total biomass")
plot_relations(2,4,full_stats,quo(log10(max_fish_mass)),quo(CV_fish),"log of fish asymptotic body mass","CV of fish biomass")

plot_relations(3,1,all_spec_stats,quo(max_Z),quo(CV_spec),"Allometric Ratio","CV of fish biomass")
plot_relations(3,2,all_spec_stats,quo(max_Z),quo(mean_spec),"Allometric Ratio","fish biomass") #updated
plot_relations(3,3,all_spec_stats,quo(orig_T),quo(CV_spec),"Trophic Level","CV of fish biomass")
plot_relations(3,4,all_spec_stats,quo(orig_T),quo(mean_spec),"Trophic Level","fish biomass") #updated

plot_relations(4,1,all_spec_stats,quo(log10(max_Mass)),quo(mean_tot),"log of fish asymptotic body mass","total biomass") #updated
plot_relations(4,2,all_spec_stats,quo(log10(max_Mass)),quo(mean_spec),"log of fish asymptotic body mass","fish biomass") #updated
plot_relations(4,3,all_spec_stats,quo(log10(max_Mass)),quo(CV_tot),"log of fish asymptotic body mass","CV of total biomass")
plot_relations(4,4,all_spec_stats,quo(log10(max_Mass)),quo(CV_spec),"log of fish asymptotic body mass","CV of fish biomass")

plot_relations(5,1,full_stats,quo(max_Z),quo(FT_ratio),"Allometric Ratio","Fish to total biomass ratio")
plot_relations(5,2,full_stats,quo(log10(max_fish_mass)),quo(FT_ratio),"log of fish mass","Fish to total biomass ratio")

postscript(paste0("Figure",6+start_fig,"_Model",Test_Model,"_row1_Allometric_full_stats.eps"),horiz=FALSE,width=8.5,height=11)
multiplot(plotlist=ls_graphs[[1]],cols=2)
dev.off()

postscript(paste0("Figure",7+start_fig,"_Model",Test_Model,"_row2_logmass_full_stats.eps"),horiz=FALSE,width=8.5,height=11)
multiplot(plotlist=ls_graphs[[2]],cols=2)
dev.off()

postscript(paste0("S",3,"_Model",Test_Model,"_row3_all_stats.eps"),horiz=FALSE,width=8.5,height=11)
multiplot(plotlist=ls_graphs[[3]],cols=2)
dev.off()

postscript(paste0("Figure",8+start_fig,"_Model",Test_Model,"_row4_all_stats2.eps"),horiz=FALSE,width=8.5,height=11)
multiplot(plotlist=ls_graphs[[4]],cols=2)
dev.off()

postscript(paste0("S",4,"_Model",Test_Model,"_row5_full_stats.eps"),horiz=FALSE,width=8.5,height=11)
multiplot(plotlist=ls_graphs[[5]][1:2],cols=2)
dev.off()

#//////////////////////////////////////////////////////////////////////////
#----Save Model Output----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
write.csv(lm_test, file = "lm_test_Model",Test_Model,".csv",row.names=FALSE, na="")
write.csv(corr_test, file = "corr_test_Model",Test_Model,".csv",row.names=FALSE, na="")
sink("cor_val_printout_Model",Test_Model,".txt")
print(cor_val_printout)
sink()  # returns output to the console

#//////////////////////////////////////////////////////////////////////////
#----CV Boxplot for Fish & Total Biomass across model types----
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

postscript(paste0("Figure",4+start_fig,"_CV_boxplot.eps"),horiz=FALSE,width=8.5,height=11)
par(mfrow=c(2,1), mai = c(0.7, 1, 0.5, 0.1),mgp=c(2,1,0))
boxplot(CV_tot~Model,CV_plot,xlab="Model Type",ylab="Coefficient of Variation",main="Total Ecosystem Biomass",ylim=c(0,100))

boxplot(CV_fish~Model,CV_plot,xlab="Model Type",ylab="Coefficient of Variation",main="Total Fish Biomass",ylim=c(0,100))
dev.off()
par(pardefault)



