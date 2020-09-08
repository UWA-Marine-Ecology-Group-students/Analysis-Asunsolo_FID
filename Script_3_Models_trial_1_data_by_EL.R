### Models ####
setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

# librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub


data<- read.csv("data_wide_BG_AA.csv", na.strings = c("", NA))
head(data)

incudes.na<-data%>%
  filter(school_individual%in%c("School"))

data<-na.omit(data)#%>%
  # filter(school_individual%in%c("School"))

# install package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

cont.preds=c("length","speed.fid","speed.priorAvg", "DFSAvg","dfs.fid") # use as continuous predictors.

cat.preds= c("Treatment","genus", "scientific", "school_individual")

null.vars="site" # use as random effect and null model

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
# running a correlation of continuous variables####
round(cor(data[,cont.preds], use = "complete.obs"),2)

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in cont.preds) {
  x<-data[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

#Transform pred.vars accordingly##

data$sqrt.length <- sqrt(data$length) #not sure which transformation to use
data$log.length <- log(data$length + 1) #between log and sqrt
data$sqrt.speed.fid <- sqrt(data$speed.fid)  
data$sqrt.speed.priorAVG <- sqrt(data$speed.priorAvg) # not sure if distribution is good enough
data$sqrt.DFSAvg <- sqrt(data$DFSAvg) 
data$log.DFSAvg <- log(data$DFSAvg + 1) 
data$log.dfs.fid <- log(data$dfs.fid +1)

#transformed variables###

cont.preds=c("log.length","sqrt.speed.fid","sqrt.speed.priorAVG", "log.DFSAvg","log.dfs.fid") # use as continuous predictors.

cat.preds= c("Treatment","genus", "scientific", "school_individual")

null.vars="site" # use as random effect and null model
# take a look at the response variables
resp.var=data$fid
resp.var

resp.var=list("fid"=gaussian(link = "identity"))
resp.var=names(resp.var)

pdf(file="resp_var.pdf",onefile=T)
for(r in 1:length(resp.var)){
  par(mfrow=c(2,1))
  hist(data[,resp.var[r]],main=resp.var[r])
  plot(jitter(data[,resp.var[r]]))
}
dev.off()

### now fit the models ---------------------------------------------------------
i=1
out.all=list()
var.imp=list()
fss.all=list()
top.all=list()
pdf(file="mod_fits_fid.pdf",onefile=T)
for(i in 1:length(resp.var)){
  use.dat=data[,c(null.vars,cont.preds,cat.preds,resp.var[i])]
  use.dat$response=use.dat[,resp.var[i]]
  Model1=gam(response~s(log.length,k=4,bs='cr')+
               s(site,bs="re"),
             family=gaussian(link = "identity"),
             data=use.dat)
  
model.set=generate.model.set(use.dat=use.dat,max.predictors=2,   # limit size here because null model already complex
                               test.fit=Model1,k=3,
                               pred.vars.cont=cont.preds,
                               pred.vars.fact=cat.preds,
                               null.terms="s(site,bs='re',k=4)")
  
  out.list=fit.model.set(model.set)
  #names(out.list)
  # examine the list of failed models
  #out.list$failed.models
  #out.list$success.models
  fss.all=c(fss.all,list(out.list))
  mod.table=out.list$mod.data.out
  mod.table=mod.table[order(mod.table$AICc),]
  out.i=mod.table
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw))
  all.less.2AICc=mod.table[which(mod.table$delta.AICc<2),]
  top.all=c(top.all,list(all.less.2AICc))
  
  # plot the all best models
  par(oma=c(1,1,4,1))
  for(r in 1:nrow(all.less.2AICc)){
    best.model.name=as.character(all.less.2AICc$modname[r])
    best.model=out.list$success.models[[best.model.name]]
    if(best.model.name!="null"){
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=3,text=resp.var[i],outer=T)}
  }
}
dev.off()

names(out.all)=resp.var
names(var.imp)=resp.var
names(top.all)=resp.var
names(fss.all)=resp.var

all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
top.mod.fits=do.call("rbind",top.all)

require(car)
require(doBy)
require(gplots)
require(RColorBrewer)

pdf(file="var_importance_heatmap_functional_biomass.pdf",height=5,width=7,pointsize=10)
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","orange","red"))(30),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,14), lhei=c(3,10),lwid=c(3,10),
          Rowv=FALSE,Colv=FALSE)
dev.off()

write.csv(all.mod.fits[,-2],"all_model_fits_functional_biomass.csv")
write.csv(top.mod.fits[,-2],"top_model_fits_functional_biomass.csv")
write.csv(model.set$predictor.correlations,"predictor_correlations.csv")

#### pretty plots of best models -----------------------------------------------
zones=levels(dat$ZONE)
pdf("best_top_model_quick_plots.pdf",height=8,width=7,pointsize=12)
par(mfcol=c(4,2),mar=c(4,4,0.5,0.5),oma=c(2,0.5,0.5,0.5),bty="l")
for(r in 1:length(resp.vars)){
  tab.r=out.all[[resp.vars[r]]]
  top.mods.r=tab.r[1,]
  mod.r.m=as.character(top.mods.r[1,"modname"])
  mod.m=fss.all[[resp.vars[r]]]$success.models[[mod.r.m]]
  mod.vars=unique(unlist(strsplit(unlist(strsplit(mod.r.m,split="+",fixed=T)),
                                  split=".by.")))
  # which continuous predictor is the variable included?
  plot.var=as.character(na.omit(mod.vars[match(cont.preds,mod.vars)]))
  # plot that variables, with symbol colours for zone
  plot(dat[,plot.var],dat[,resp.vars[r]],pch=16,
       ylab=resp.vars[r],xlab=plot.var,col=dat$ZONE)
  legend("topleft",legend=paste("(",LETTERS[r],")",sep=""),
         bty="n")
  range.v=range(dat[,plot.var])
  seq.v=seq(range.v[1],range.v[2],length=20)
  newdat.list=list(seq.v,# across the range of the included variable
                   mean(use.dat$depth), # for a median depth
                   mean(use.dat$SQRTSA),# for a median SQRTSA
                   "MANGROVE", # pick the first site, except don't predict on
                   # this by setting terms=c(plot.var,"ZONE")
                   zones)  # for each zone
  names(newdat.list)=c(plot.var,"depth","SQRTSA","site","ZONE")
  pred.vals=predict(mod.m,newdata=expand.grid(newdat.list),
                    type="response",se=T,exclude=c("site","SQRTSA","depth"))
  for(z in 1:length(zones)){
    zone.index=which(expand.grid(newdat.list)$ZONE==zones[z])
    lines(seq.v,pred.vals$fit[zone.index],col=z)
    lines(seq.v,pred.vals$fit[zone.index]+pred.vals$se[zone.index]*1.96,lty=3,col=z)
    lines(seq.v,pred.vals$fit[zone.index]-pred.vals$se[zone.index]*1.96,lty=3,col=z)}
}
legend("bottom",legend= zones,bty="n",ncol=2,col=c(1,2),pch=c(16,16),
       inset=-0.61,xpd=NA,cex=.8)
dev.off()

