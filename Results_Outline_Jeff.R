## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE,echo=FALSE,fig.pos = 'H',cache.lazy = FALSE)

## ----hidden, include=FALSE-----------------------------------------------
#---- Hidden ----
#library(ggbiplot)
library(tidyverse)
library(stringr)
library(knitr)
z=0

## ----load_data-----------------------------------------------------------
DATE="2017Nov28"
Version="0"
location="/GIT/Analysis"#For Running on my Mac
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
setwd(paste0("~/",location,"/",run_name))
#---- LOAD_DATA ----
dat=read.table("clean.txt",header=F)
colnames(dat)=as.matrix(read.table("colnames_clean.txt"))
colnames(dat)[9]="Model"
# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "model 1" and model 2" because people will definitely forget what that means.
exper_name=c("Original Web","Extended Web","Leslie & History")
# Make Processing faster, I think? - DONT USE THIS YET - IT WILL BREAK THINGS!
# dat=dat %>% mutate_at(c("isfish","basal_ls"),as.logical) %>%
#	mutate_at(c("Phase_df","Nodes_df","Seed","Model","pred","prey","species","lifestage"),as.factor)

## ----DefineMultiplot-----------------------------------------------------
# From https://stackoverflow.com/a/24387436
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
png("Figure1_Von-Bert-Multi.png")

subdat1 %>% filter(simnum<20,Model==3,Year_df==max(Year_df),isfish==1) %>%
	group_by(simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	filter(tot_fish_biom>0) %>% # Can't filter by Biomass>0 because some life stages go extinct and you end up with incomplete curves
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	#select(simnum,species,Biomass, tot_fish_biom,Mass)
	ggplot(aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum)) + labs(x="Fish life stage",y="log of individual body mass")

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
	ggplot(aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum)) + labs(x="Fish life stage",y="log of individual\n body mass")

# Caution: The clunky formatting here is because we need to plot ALL life stages if a single life stage survives. This way you won't end up with a partial line between two life stages
VB_end_fish=subdat1 %>% filter(simnum<20,Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	filter(tot_fish_biom>0) %>% # Can't filter by Biomass>0 because some life stages go extinct and you end up with incomplete curves
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	group_by(Model) %>%
	do(g=ggplot(.,aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum))  + labs(x="Fish life stage",y="log of individual\n body mass"))

#multiplot(VB_orig_fish, VB_end_fish$g[[1]], VB_end_fish$g[[2]], VB_end_fish$g[[3]],cols=1)

#z=z+1;VBmult2=z
#cap=paste("Figure",VBmult2,"Von-Bertalanffy curves for surviving fish in several simulated food webs. Each colour represents a different food web.")

## ----VB-Hist-------------------------------------------------------------
VB_orig=dat %>% filter(Year_df==1,Model==1) %>%
	group_by(simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)
VB_hist_orig = VB_orig %>% ggplot(.,aes(Z))+geom_histogram() + coord_cartesian(xlim=c(min(VB_orig$Z),max(VB_orig$Z))) + labs(x="Allometric Ratio")

VB_hist=subdat1 %>% filter(Year_df==max(Year_df),Biomass>0) %>%
	group_by(Model,simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)

## ----VB_Exper_compare, fig.cap=cap,echo=FALSE----------------------------
meh=VB_hist %>% group_by(Model) %>%
	do(k=ggplot(.,aes(Z))+geom_histogram()+coord_cartesian(xlim=c(min(VB_orig$Z),max(VB_orig$Z))) + labs(x="Allometric Ratio"))

png("Figure2_VB_Exper_compare.png")
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

png("SuppFigure1_AllometricOverlap.png")
hist(log10(mass_overlap$size_ratio),main="",xlab="Allometric ratio of the smallest fish adult\n to the youngest life stage of the largest fish (log10)")

z=z+1;mass_overlap_cap=z
cap=paste("Supplementary Figure",mass_overlap_cap,"A histogram of the logged allometric ratios between the oldest life stage of the smallest fish species and the youngest life stage of the largest fish species for any model.")
dev.off()

## ----TS_solo, fig.cap=cap------------------------------------------------
png("Figure3_TS_solo.png")
subdat2 %>% filter(Model==3,simnum==unique(simnum)[4]) %>%
	mutate(lifestage=as.factor(lifestage),species=as.factor(isfish*species)) %>%
	group_by(Year_df,lifestage,species) %>%
	mutate(Biomass=sum(Biomass)) %>% ungroup() %>% mutate(species=c("Other","Fish 1","Fish 2","Fish 3")[species]) %>%
	ggplot(aes(x=Year_df,y=log10(Biomass))) + geom_line(aes(group=Nodes_df, colour=species,linetype=lifestage)) + labs(x="Year",y="Biomass (log 10)")

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
	ggplot(., aes(x=Num_extant, y=Freq)) + geom_point(aes(group=Model, color=Model))+ labs(x="Number of surviving fish species", y="Frequency of simulations")
xkcd2=xkcd %>% filter(Num_extant %in% range(Num_extant)) %>%
	mutate(Num_extant=as.factor(Num_extant)) %>%
	ggplot(., aes(x=Num_extant, y=Freq)) + geom_point(aes(group=Model, color=Model))+ labs(x="Number of surviving fish species", y="Frequency of simulations")+ scale_x_discrete(labels=c("None","All"))
png("Figure5_freq_extant_fish.png")
multiplot(xkcd1,xkcd2)

z=z+1;freq_ex_fish=z
cap=paste("Figure",freq_ex_fish,"The frequency of fish surviving in each model.")
dev.off()

## ----CV_total, fig.cap=cap-----------------------------------------------
# Coefficient of Variation Function
CV <- function(dat){sd(dat)/mean(dat)*100}
sem <- function(x) {sd(x,na.rm=T)/sqrt(sum(is.na(x)))} # Standard error

CV_plot=subdat2 %>% group_by(Model,simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_bio=sum(Biomass),Tot_fish=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_bio),CV_fish=CV(Tot_fish)) # %>% ### So here we see the CVs for fish and total
	#summarise_at(c("CV_tot","CV_fish"),c(mean,sem))

boxplot(CV_tot~Model,CV_plot,xlab="Model",ylab="Coefficient of Variation")

#ggplot(CV_plot,aes(x=Model,y=CV_tot))+geom_boxplot() + xlab("Model") + ylab("Mean Coefficient of Variation") + labs(caption="Figure 3 Box plots for the CV of the total biomass.")

z=z+1;totCV=z
cap=paste("Figure",totCV,"The coefficient of variation for the total biomass in each model")

## ----CV_fish, fig.cap=cap, include=F-------------------------------------
boxplot(CV_fish~Model,CV_plot,xlab="Model",ylab="Coefficient of Variation") 
#	labs(caption="Figure 4 The CV of total fish biomass.")

z=z+1;fishCV=z
cap=paste("Figure",fishCV,"The coefficient of variation for the total fish biomass in each model")

