# Cluster R script
# Stephanie Bland
# Analysis of Data in the Cluster
# This function takes data from each simulation and merges it into one file per data type (so output will give a file for each variable, and each file will have data across all experiments and simulations) - actually i cant do that easily (bc # nodes might change and woah there's lots of data)

# Easiest way to prevent duplicate rows or missing rows (in case cluster processes a job multiple times or not at all) would probably be to create a NaN matrix fill rows up with values **at the end** (so if there's an error you'll be able to find it easily, and rows will only be entered if all data was collected)
# Input k tells you what row in the matrix to fill

################################################
############### Temp Testing ###################
################################################
#---- Hidden ----
rm(list=ls())
library(reshape2)
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
for (simnum in 1:100){
#---- CompileData ----
  for (Exper in 1:3){
    for (pred in 0:2){for (prey in 0:1){
    ################################################
    ############### Read in Data ###################
    ################################################
    name=paste0(run_name,"_seed",seed_0,"_sim",simnum,"_Exper",Exper,"_pred",pred,"_prey",prey)
    
    import_vars_sim='B_year_end'#c('B','B_year_end','B_stable_phase')
    import_vars_web=c('isfish','basalsp','basal_ls','species','numyears','nichewebsize','ext_thresh','N_stages','lifestage','L_year','Mass','lifehis.splitdiet','lifehis.fishpred')
    import_vars=c(import_vars_sim,import_vars_web)
    for (item in 1:length(import_vars)){
      trial=paste0(name,"_",import_vars[item],".txt")
      trial=as.matrix(read.csv(trial,header=F))
      if (sum(dim(trial))==2) {trial=as.integer(trial)}
      do.call("<-",list(import_vars[item], trial))
    }
    
    ################################################
    ############### Extract Data ###################
    ################################################
    # Set up - helpful data reformatting:
    inverts_only=setdiff(which(isfish==0),basalsp) #Find invertebrates (not fish or autotrophs)
    fish_names=unique(species[isfish==1]) #The species number for fish
    yr_ls=cumsum(unlist(numyears)) # Cumulative sums of years for phases
    yr_ls_start=yr_ls-numyears
    Timegroups=cbind(yr_ls_start,yr_ls,numyears)
    Timegroups=cbind(Timegroups,Timegroups*c(L_year))
    colnames(Timegroups)=c("Year_start","Year_end","Year_length","Day_start","Day_end","Days_length")
    
    # ---- DATA_FOR_TAPPLY ----
    lump_Bio_sums <- function(B_mat){# 1 Add columns for sum of nodes so we have biomass of groups of species (ex. all fish)
      colnames(B_mat)=paste0('Node_',1:nichewebsize)
      #Fish total per species
      Fish_tot_per_sp_df=c()
      for (item in fish_names){
        Fish_tot_per_sp_df=cbind(Fish_tot_per_sp_df,rowSums(B_mat[,species==item]))
      }
      colnames(Fish_tot_per_sp_df)=paste0('Fish_sp_',fish_names)
      
      #Fish total per lifestage
      Fish_tot_per_ls_df=c()
      for (item in 1:max(N_stages)){
        fish_stages=(lifestage==item & t(isfish)==1)
        Fish_tot_per_ls_df=cbind(Fish_tot_per_ls_df,rowSums(B_mat[,fish_stages]))
      }
      colnames(Fish_tot_per_ls_df)=paste0('Fish_ls_',1:max(N_stages))
      
      #Find sum of basic categories
      Tot_df=rowSums(B_mat)
      Fish_tot_df=rowSums(B_mat[,isfish==1])#Total biomass of all fish species
      non_fish_df=rowSums(B_mat[,isfish==0])#Total Biomass of Non Fish species
      basal_tot_df=rowSums(B_mat[,basalsp])#Total Biomass of all autotrophs
      inverts_tot_df=rowSums(B_mat[,inverts_only])#Total Biomass of invertebrates only
      
      #Bind them to regular Data frame
      B_df=cbind(B_mat,Fish_tot_per_sp_df,Fish_tot_per_ls_df,Tot_df,Fish_tot_df,non_fish_df,basal_tot_df,inverts_tot_df)
      B_df=as.matrix(B_df)
    }
    
    melt_new_col=function(melted_df){
      # 3 Add columns into melt for each category:
      melted_df$Calen_df=(melted_df$Day_df-1)%%(L_year)+1
      melted_df$Year_df=(melted_df$Day_df-1)%/%L_year+1
      for (item in length(numyears):1){
        melted_df$Phase_df[melted_df$Year_df <=yr_ls[item]]=item
      }
      melted_df$yr_in_phase=melted_df$Year_df-c(0,yr_ls)[melted_df$Phase_df]
      melted_df[,"Seed"]=c(seed_0)
      melted_df[,"Simnum"]=c(simnum)
      melted_df[,"Exper"]=c(Exper)
      melted_df[,"Prey"]=c(prey)
      melted_df[,"Pred"]=c(pred)
      melted_df[,"ShouldBe0"]=c(abs(prey-lifehis.splitdiet)+abs(pred-lifehis.fishpred))
      result=melted_df
    }
    
    #B_df=lump_Bio_sums(B)
    B_df_yr_ends=lump_Bio_sums(B_year_end)
    # 2 Melt data so [i j B] = [day, node (or sum of nodes), Biomass]
    #melt_B=setNames(melt(B_df), c('Day_df','Nodes_df','Biomass'))
    melt_B.yr.end=setNames(melt(B_df_yr_ends), c('Day_df','Nodes_df','Biomass'))
    
    melt_B.yr.end$Day_df=melt_B.yr.end$Day_df*100
    #melt_B=melt_new_col(melt_B)
    melt_B.yr.end=melt_new_col(melt_B.yr.end)
    
    # ---- SURVIVING_SPECIES ----
    
    B_year_end[yr_ls,]>c(ext_thresh)
    B_year_end[yr_ls,]>0
    
    #extant=which(B[tot_days,]>0)
    #extinct=which(B[tot_days,]==0)
    
    ################################################
    ############### Sample Plots ###################
    ################################################
    samp_plot=1:10
    if (simnum %in% samp_plot){
      # # Von Bertalanffy curve
      # matplot(matrix(log10(Mass[species %in% fish_names]),max(lifestage),length(fish_names)),type="l",lwd=3,xlab="Lifestage",ylab="Individual Body Mass (log10)",main="Von Bertalanffy curve")
      # #dev.print()
      # # Phase Diagrams
      # library(plot3D)
      # library(plot3Drgl)
      # xkcd=log10(B_df[,5])
      # lag_h=0.21#0.21 is nice
      # xkcd_h=lag(xkcd,n=L_year*lag_h)
      # xkcd_2h=lag(xkcd,n=L_year*lag_h*2)
      # lines3D(xkcd,xkcd_h,xkcd_2h)
      # # Species plot against each other
      # t_0=500
      # t_f=dim(B)[1]
      # 
      # plot_time=Timegroups[2,"Day_start"]:Timegroups[2,"Day_end"]
      # xkcd1=log10(B_df[plot_time,"Fish_tot_df"])
      # xkcd2=log10(B_df[plot_time,"basal_tot_df"])
      # xkcd3=log10(B_df[plot_time,"inverts_tot_df"])
      # scatter3Drgl(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Biomass of All Fish (log)",ylab="Biomass of All Autotrophs (log)",zlab="Biomass of All Invertebrates (log)")
      # mtext(side = 3, text = "Sum all lifestages", line = -2.5)
      # 
      # 
      # plot_time=Timegroups[2,"Year_start"]:Timegroups[2,"Year_end"]
      # xkcd1=log10(B_df_yr_ends[plot_time,"Fish_tot_df"])
      # xkcd2=log10(B_df_yr_ends[plot_time,"basal_tot_df"])
      # xkcd3=log10(B_df_yr_ends[plot_time,"inverts_tot_df"])
      # scatter3Drgl(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Biomass of All Fish (log)",ylab="Biomass of All Autotrophs (log)",zlab="Biomass of All Invertebrates (log)")
      # mtext(side = 3, text = "Sum all lifestages", line = -2.5)
    }
    ################################################
    ################## Save Data ###################
    ################################################
    # Last line is Data entry into matrix, because line should only be entered if all data was collected
    # It loads it into line k for the data matrix. 
    c(seed_0,simnum,Exper)
    #melt_B
    melt_B.yr.end
    write.table(melt_B.yr.end,"Melted.txt",append=T,col.names = F,row.names = F)
    print(simnum)
    }}
  }
}
write.table(colnames(melt_B.yr.end),"colnames.txt",col.names = F,row.names = F)
#---- LOAD_DATA ----
alldata=read.table("Melted.txt",header=F)
colnames(alldata)=colnames(melt_B.yr.end)
#---- Other ----
subdat=alldata
subdat=subdat[subdat$Nodes_df=="Fish_tot_df",]
tapply(subdat$Biomass,list(subdat$Exper,subdat$Phase_df),mean)
tapply(subdat$Biomass,list(subdat$Exper,subdat$Phase_df),var)
subdat=alldata
subdat=subdat[subdat$Phase_df==2,]
tapply(subdat$Biomass,list(subdat$Exper,subdat$Nodes_df),mean)
tapply(subdat$Biomass,list(subdat$Exper,subdat$Nodes_df),var)
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
# Now try between the first two experiments
first_two_Exper=melt_counts
first_two_Exper=first_two_Exper[first_two_Exper$Exper_b<3,]
stress.aov <- with(first_two_Exper,aov(Count_extant~Exper_b +Error(Simnum_b / (Exper_b))))
summary(stress.aov)

################################################
############# May be of interest ###############
################################################
matplot(log10(t(tail(tapply(melt_B.yr.end$Biomass,list(melt_B.yr.end$Nodes_df,melt_B.yr.end$Phase_df),mean)))),type="l",lwd=3, xlab="Phase",ylab="Mean of Biomass (log)")
legend("bottomleft",c("fish adults","Tot_B","fish","nonfish","Basal","inverts"),col=1:6,lty=1:6,lwd=3)

matplot((t(tail(tapply(melt_B.yr.end$Biomass,list(melt_B.yr.end$Nodes_df,melt_B.yr.end$Phase_df),mean)))),type="l",lwd=3, xlab="Phase",ylab="Mean of Biomass")
legend("bottomleft",c("fish adults","Tot_B","fish","nonfish","Basal","inverts"),col=1:6,lty=1:6,lwd=3)





