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


data<- read.csv("data_wide_SchoolsMean_BG_AA.csv")%>%
  glimpse()


dat <- data %>%
  dplyr::select(unique_id, speed.fid, length, Treatment, family, genus, scientific, activity, school_individual, speed.priorAvg, site)%>%
  glimpse()

data<-na.omit(dat)#%>%

glimpse(data)

## Lose ~24 obs

table<-table(data$Treatment, data$scientific)
No.fish.Treatment<-apply(table, MARGIN = 2, FUN = sum)
No.fish.Treatment

No.fish.scientific<-apply(table, MARGIN = 1, FUN = sum)
No.fish.scientific
# install package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

cont.preds=c("length","speed.priorAvg") # use as continuous predictors.

cat.preds= c("Treatment","genus", "school_individual")

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
data$sqrt.speed.priorAvg <- sqrt(data$speed.priorAvg) 
data$log.speed.priorAvg <- log(data$speed.priorAvg + 1) 


#transformed variables###

cont.preds=c("log.length", "sqrt.speed.priorAvg") # use as continuous predictors.

cat.preds= c("Treatment","genus", "school_individual")

null.vars="site" # use as random effect and null model
# take a look at the response variables

resp.var=data$log.speed.fid
resp.var

resp.var=list("speed.fid"=gaussian(link = "identity"))
resp.var=names(resp.var)

pdf(file="resp_var.speed.pdf",onefile=T)
for(r in 1:length(resp.var)){
  par(mfrow=c(2,1))
  hist(data[,resp.var[r]],main=resp.var[r])
  plot(jitter(data[,resp.var[r]]))
}
dev.off()

glimpse(data)

#Transform response variable###

data$sqrt.speed.fid <- sqrt(data$speed.fid)
data$log.speed.fid <- log(data$speed.fid + 1)

### now fit the models ---------------------------------------------------------
i=1
out.all=list()
var.imp=list()
fss.all=list()
top.all=list()
pdf(file="mod_fits_speed.fid.pdf",onefile=T)
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

#pdf(file="var_importance_heatmap_speed_fid.pdf",height=5,width=7,pointsize=10)
#heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
#          col=colorRampPalette(c("white","yellow","orange","red"#))(30),
#          trace="none",key.title = "",keysize=2,
#          notecol="black",key=T,
#          sepcolor = "black",margins=c(12,14), lhei=c(3,10),lwid=c(3,10),
#          Rowv=FALSE,Colv=FALSE)
#dev.off()
name="FID"
write.csv(all.mod.fits[,-2],"all_model_fits_speed.fid.csv")
write.csv(top.mod.fits[,-2],"top_model_fits_speed.fid.csv")
write.csv(model.set$predictor.correlations,"predictor_correlations_speed.fid.csv")
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.speed.fid.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.speed.fid.csv",sep="_"))
all.mod.fits
all.var.imp

#### pretty plots of best model -----------------------------------------------
gamm <- gam (speed.fid~s(log.length,k=4,bs='cr') + s(sqrt.speed.priorAvg,k=4,bs='cr') + Treatment + s(site,bs="re"), family=gaussian(link = "identity"),
             data=data)

summary(gamm)
mod<-gamm
par(mfrow=c(1,1))
plot(gamm)
gam.check(gamm)

#model predictions for log.length

gamm <- gam (speed.fid~s(log.length,k=4,bs='cr') + Treatment + s(site,bs="re"), family=gaussian(link = "identity"),
             data=data)

summary(gamm)
mod<-gamm
par(mfrow=c(1,1))
plot(gamm)
gam.check(gamm)



detach("package:plyr", unload=TRUE)#will error - don't worry. Just get rid of this bastard.

testdata <- expand.grid(log.length = seq(min(data$log.length),max(data$log.length),length.out = 20),
                        Treatment = (mod$model$Treatment),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()



head(testdata)
fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
# head(fits,2)

## Plot log. length

predicts.log.length= testdata%>%data.frame(fits)%>%
  group_by(log.length)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.log.length


## Colour
library(ggplot2)

ggmod.log.length <-  ggplot(aes(x=log.length ,y=response), data=predicts.log.length)+
  ylab("Speed.FID")+
  xlab('Log length')+
  geom_line(data=predicts.log.length,aes(x=log.length, y=response),colour="#293462",alpha=0.8,size=1,show.legend=TRUE)+
  geom_point(data=data,aes(x=log.length, y=speed.fid),colour="#293462",alpha=0.2)+
  geom_ribbon(aes(ymin=response-se.fit, ymax=response + se.fit), alpha=0.4, fill="#293462", linetype='blank')+
  theme_classic()

ggmod.log.length
## Model predictions for sqrt.speed.priorAvg

gamm <- gam (speed.fid~s(sqrt.speed.priorAvg,k=4,bs='cr') + Treatment + s(site,bs="re"), family=gaussian(link = "identity"),
             data=data)

summary(gamm)
mod<-gamm
par(mfrow=c(1,1))
plot(gamm)
gam.check(gamm)



testdata <- expand.grid(sqrt.speed.priorAvg = seq(min(data$sqrt.speed.priorAvg),max(data$sqrt.speed.priorAvg),length.out = 20),
                        Treatment = (mod$model$Treatment),
                        site=(mod$model$site))%>%
  distinct()%>%
  glimpse()



head(testdata)
fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
## Plot Sqrt.speedAvg####

predicts.sqrt.speed.avg= testdata%>%data.frame(fits)%>%
  group_by(sqrt.speed.priorAvg)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sqrt.speed.avg

## Colour
library(ggplot2)

ggmod.sqrt.speed.prior.avg <-  ggplot(aes(x=sqrt.speed.priorAvg ,y=response), data=predicts.sqrt.speed.avg)+
  ylab("Speed.FID")+
  xlab('Sqrt Speed prior Avg')+
  geom_line(data=predicts.sqrt.speed.avg,aes(x=sqrt.speed.priorAvg, y=response),colour="#293462",alpha=0.8,size=1,show.legend=TRUE)+
  geom_point(data=data,aes(x=log.length, y=speed.fid),colour="#293462",alpha=0.2)+
  geom_ribbon(aes(ymin=response-se.fit, ymax=response + se.fit), alpha=0.4, fill="#293462", linetype='blank')+
  theme_classic()

ggmod.sqrt.speed.prior.avg

## Plot Treatment


testdata1 <- expand.grid(log.length = mean(mod$model$log.length),
                         Treatment = (mod$model$Treatment),
                         site=(mod$model$site))%>%
  distinct()%>%
  glimpse()



head(testdata1)
fits1 <- predict.gam(mod, newdata=testdata1, type='response', se.fit=T)
# head(fits,2)

## Plot log. length

predicts.Treatment= testdata1%>%data.frame(fits1)%>%
  group_by(Treatment)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.Treatment


## Colour


ggmod.Treatment<-  ggplot(aes(x=Treatment ,y=response), data=predicts.Treatment)+
  ylab("Speed.FID")+
  xlab('Treatment')+
  geom_bar(data=predicts.Treatment,aes(x=Treatment, y=response),alpha=0.8,stat = "identity",size=1,show.legend=TRUE)+
  geom_point(data=data,aes(x=Treatment, y=speed.fid),alpha=0.2)+
  geom_errorbar(aes(ymin = response-se.fit,ymax = response+se.fit),width = 0.5, size=1, alpha=0.6, colour="grey30") +
  theme_classic()
  
  theme_classic()

ggmod.Treatment

#### to use different colors in the graph###
#geom_bar(stat = "identity", alpha=0.6, fill="#293462")+
  #scale_fill_manual(labels = c("shallow", "deep"),values=c("#00818a", "#00818a"))+  ## Change the names here
