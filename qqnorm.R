

ls=vector("list",4)
ls_qq=list(ls,ls,ls,ls,ls,ls,ls,ls,ls,ls,ls,ls,ls,ls,ls,ls,ls,ls)

qqnorm_plot <- function(xk_fig,dat,xvar,yvar){
	dat = dat %>% mutate_(xvar=xvar,yvar=yvar) %>% 
		mutate(logx=log10(xvar),logy=log10(yvar))
	# Make Figures
	xk1=qqplot(dat$xvar,dat$yvar,xlab=paste0(xvar)[2],ylab=paste0(yvar)[2],main=paste0("x=",xvar," and y=",yvar)[2])
	xk2=qqplot(dat$logx,dat$yvar,xlab=paste0("log(",xvar,")")[2],ylab=paste0(yvar)[2],main=paste0("x=",xvar," and y=",yvar)[2])
	xk3=qqplot(dat$xvar,dat$logy,xlab=paste0(xvar)[2],ylab=paste0("log(",yvar,")")[2],main=paste0("x=",xvar," and y=",yvar)[2])
	xk4=qqplot(dat$logx,dat$logy,xlab=paste0("log(",xvar,")")[2],ylab=paste0("log(",yvar,")")[2],main=paste0("x=",xvar," and y=",yvar)[2])
	# Figures Saved
	envir[[ "ls_qq" ]][[xk_fig]][1] <- list(xk1)
	envir[[ "ls_qq" ]][[xk_fig]][2] <- list(xk2)
	envir[[ "ls_qq" ]][[xk_fig]][3] <- list(xk3)
	envir[[ "ls_qq" ]][[xk_fig]][4] <- list(xk4)
	# Figures to hard drive
	par(mfrow=c(2,2))
	#png(paste0("mkTest",xk_fig,".png"))
	print({
	xk1
	xk2
	xk3
	xk4
	})
	#multiplot(xk1,xk2,cols=2)
	#dev.off()
}

qqnorm_plot(1,full_stats,quo(max_Z),quo(mean_tot))
qqnorm_plot(2,full_stats,quo(max_Z),quo(mean_fish))
qqnorm_plot(3,full_stats,quo(max_Z),quo(CV_tot))
qqnorm_plot(4,full_stats,quo(max_Z),quo(CV_fish))
qqnorm_plot(5,full_stats,quo(max_fish_mass),quo(mean_tot))
qqnorm_plot(6,full_stats,quo(max_fish_mass),quo(mean_fish))
qqnorm_plot(7,full_stats,quo(max_fish_mass),quo(CV_tot))
qqnorm_plot(8,full_stats,quo(max_fish_mass),quo(CV_fish))
qqnorm_plot(9,all_spec_stats,quo(max_Z),quo(CV_spec))
qqnorm_plot(10,all_spec_stats,quo(max_Z),quo(mean_spec))
qqnorm_plot(11,all_spec_stats,quo(orig_T),quo(CV_spec))
qqnorm_plot(12,all_spec_stats,quo(orig_T),quo(mean_spec))
qqnorm_plot(13,all_spec_stats,quo(max_Mass),quo(mean_tot))
qqnorm_plot(14,all_spec_stats,quo(max_Mass),quo(mean_spec))
qqnorm_plot(15,all_spec_stats,quo(max_Mass),quo(CV_tot))
qqnorm_plot(16,all_spec_stats,quo(max_Mass),quo(CV_spec))
qqnorm_plot(17,full_stats,quo(max_Z),quo(FT_ratio))
qqnorm_plot(18,full_stats,quo(max_fish_mass),quo(FT_ratio))

for (i in 1:18){
	postscript(paste0("Test",i,".eps"),horiz=FALSE,width=8.5,height=11)
	multiplot(plotlist=ls_qq[[i]],cols=2)
	dev.off()
}

for (i in 1:18){
	png(paste0("qTest",i,".png"))
	multiplot(plotlist=ls_qq[[i]],cols=2)
	dev.off()
}
