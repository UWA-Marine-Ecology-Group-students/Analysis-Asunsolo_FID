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
data<- read.csv("wrangled_data_EL.csv", na.strings = c("", NA))
head(data)

# install package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

cont.preds=c("Length","speed.FID","SpeedAvg", "DFSAvg","DFS.FID") # use as continuous predictors.

cat.preds= c("Treatment","Genus", "Species", "School.Individual")

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

data$sqrt.Length <- sqrt(data$Length) #not sure which transformation to use
data$log.Length <- log(data$Length + 1) #between log and sqrt
data$sqrt.speed.FID <- sqrt(data$speed.FID)  
data$sqrt.SpeedAVG <- sqrt(data$SpeedAvg) # not sure if distribution is good enough
data$sqrt.DFSAvg <- sqrt(data$DFSAvg) 
data$sqrt.DFS.FID <- sqrt(data$DFS.FID)

#skipped the part about response variable havinf less than 80% zeros. FID values are all above zero

# take a look at the response variables
resp.var=data$FID
resp.var

pdf(file="resp_vars.pdf",onefile=T)
for(i in 1:length(resp.var)){
  par(mfrow=c(2,1))
  hist(data[,resp.var[i]],main=resp.var[i])
  plot(jitter(data[,resp.var[i]]))
}
dev.off()
