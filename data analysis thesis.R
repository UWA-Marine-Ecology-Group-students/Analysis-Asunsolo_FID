setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

library(ggplot2)
library(tidyr)
library(dplyr)

data<- read.csv("finaldata .csv", na.strings = c("", NA))
head(data)


data$Treatment <- as.factor(data$Treatment)
data$Species <- as.factor(data$Species)
data$Family <- as.factor(data$Family)
data$Genus <- as.factor(data$Genus)
data$OpCode <- as.factor(data$OpCode)
data$Activity <- as.factor(data$Activity)
data$School.Individual <- as.factor(data$School.Individual)

data <- data %>%
    dplyr::mutate(uniqueID = paste0(Fish.ID, OpCode))
data$uniqueID <- as.factor(data$uniqueID)

idmatch <- data %>%
  dplyr::select(uniqueID, Treatment, OpCode,
                Family, Genus, Species, Activity,
                School.Individual) %>%
  dplyr::distinct() %>%
  dplyr::arrange(uniqueID) %>% 
  dplyr::filter(!duplicated(uniqueID)) 

data2 <- data %>%
  pivot_wider(
  id_cols = uniqueID,
  names_from = Length.FID,
  values_from = Measurements,
) %>%
  left_join(idmatch, by = "uniqueID")

#### transform to long format #### can't bind many data frames because they don't have same number of rows, use the original data in long format for subset script

#data_longspeed <- gather(data2, speed.prior.1, speed.prior.2, speed.prior.3, speed.FID, key = "Speed", value = "speed_value", na.rm = FALSE,
                   # convert = FALSE, factor_key = FALSE) 
#data_longspeed <- select(data_longspeed, Speed, speed_value)

#data_longDFF<- gather(data2, DFF.prior.1, DFF.prior.2, DFF.prior.3, DFF.FID, DFF.post.FID, key = "DFF", value = "value_DFF", na.rm = FALSE,
                  # convert = FALSE, factor_key = FALSE) 

#data_longDFF <- select(data_longDFF, DFF, value_DFF)

#data_longDFS<- gather(data2, DFS.prior.1, DFS.prior.2, DFS.prior.3, DFS.FID, key = "DFS", value = "value_DFS", na.rm = FALSE,
                    #  convert = FALSE, factor_key = FALSE) 

#data_longDFS <- select(data_longDFS, DFS, value_DFS)

#data_longmisc<- select(data2, uniqueID, FID, Length, Treatment, OpCode, Family, Genus, Species, Activity, School.Individual)

#data_long = NULL

#data_long <- cbind(data_long, data_longspeed)
#data_long <- cbind(data_long, data_longDFS)
#data_long <- cbind(data_long, data_longmisc)
#data_long <- cbind(data_long, data_longDFF)

#write.csv(data2, "data2.csv", sep=",")

dim(data2)
head(data2)
library(plyr)
#use when you want to see what rows have the mistake in this case duplicate.
#check <- plyr::ddply(data, .(OpCode, Fish.ID, Length.FID), nrow)
#unique(check$V1)
#check[check$V1 != 1, ]

#length(unique(idmatch$uniqueID))
#dim(idmatch)
#length(unique(data$uniqueID))

#how to get the mean for the prior speed and distances
#DFSprior<- mean(data2$DFS.prior.1,data2$DFS.prior.2, data2$DFS.prior.3)

CountTreat <-table(count(data2$Treatment))
CountTreat

#Exploring###
#tru to apply this to your data
## based on the limited family data available, we can calculate the number of individuals of each family at each site as follows
#BCIt$species <- names(BCI)
#head(BCIt)
#(BCIt.subset <- merge(BCIt,class.data3,by.x='species',by.y='species'))
#aggregate(BCIt.subset[,2:51],list(BCIt.subset$Family),sum)

### Plots ####
par(mfrow=c(1,1))
Perspex<- dplyr::filter(data2, Treatment %in% c("Perspex"))
Perspex

plot(data2$Treatment, data2$FID, xlab= "Treatment", ylab= "FID (mm)")

scatter.smooth(data2$Length, data2$FID, xlab= "Fish length (mm)", ylab= "FID (mm)")
dotchart(data2$Length, data2$FID, xlab= "Fish size (mm)", ylab= "FID (mm)")

plot(data2$Species, data2$FID, xlab= "", ylab= "FID (mm)", las=3, cex= 0.2)

plot(data2$Genus, data2$FID, xlab= "Genus", ylab= "FID (mm)", las=3, cexlab= 0.7)

plot(data2$Treatment, data2$Activity, xlab= "Treatment", ylab= "Type of reaction", col=
       1:length(data2$Activity))
#legend(1,95, legend=c("swim backwards", "no response", "hide", "flight", "c-turn"))

plot(data2$Treatment, data2$Length, xlab= "Treatment", ylab= "Fish Length (mm)")


scatter.smooth(data2$DFF.FID, data2$FID, xlab= "DFF FID (mm)", ylab= "FID (mm)")

scatter.smooth(data2$DFS.FID, data2$FID, xlab= "DFS FID (mm)", ylab= "FID (mm)")

boxplot(data2$DFF.FID~ data2$DFF.post.FID, xlab= "DFF FID (mm)", ylab= "DFF post FID (mm)")
#boxplot(data2$DFF.post.FID~ data2$DFF.FID)

# Theme for plotting ----
Theme1 <-    theme_bw()+
  theme( # use theme_get() to see available options
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    legend.title = element_blank(),
    #legend.position = "top",
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.y=element_text(size=12),
    axis.text.x=element_text(size=12),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=12, face="bold.italic"))


# functions for summarising data on plots ----
#se <- function(x) sd(x) / sqrt(length(x))
#se.min <- function(x) (mean(x)) - se(x)
#se.max <- function(x) (mean(x)) + se(x)

data2long<- gather(data2, DFF.FID, DFF.post.FID, key = "DFF", value = "value", na.rm = FALSE,
                   convert = FALSE, factor_key = FALSE) 
data2long.1<-gather(data2, speed.prior.3, speed.FID, key = "speed", value = "value", na.rm = FALSE,
                    convert = FALSE, factor_key = FALSE) 

ggplot(data2long,aes(x = factor(Treatment), y = value,  fill = DFF, notch=FALSE, outlier.shape = NA),alpha=0.5) +
  stat_boxplot(geom='errorbar')+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  #scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+ # this sets 10% above the max for each on the Y scale
  # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
  xlab("Treatment") + ylab("Distance") +
  #annotation_custom(grob.sci)+ #adds a title
  Theme1

ggplot(data2long.1,aes(x = factor(Treatment), y = value,  fill = speed, notch=FALSE, outlier.shape = NA),alpha=0.5) +
  stat_boxplot(geom='errorbar')+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  #scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+ # this sets 10% above the max for each on the Y scale
  # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
  xlab("Treatment") + ylab("Speed (m/s)") +
  #annotation_custom(grob.sci)+ #adds a title
  Theme1

plot(data2$School.Individual, data2$FID, xlab= "Treatment", ylab= "FID (mm)")
boxplot(data2$FID~data2$School.Individual*data2$Treatment, xlab= "", ylab= "FID (mm)", las=3, cex= 0.2)

scatter.smooth(data2$speed.FID, data2$FID, xlab= "Speed FID (m/s)", ylab= "FID (mm)")

#Speed prior vs during FID-- can I do a boxplot?
FID_speed<- ggplot(data2, aes(speed.prior.3, speed.FID))+ facet_wrap(~Treatment)+
  geom_point()+
  theme_classic()
FID_speed

#Speed during FID per treatment
FID_speed.FID<- ggplot(data2, aes(FID, speed.FID))+ facet_wrap(~Treatment)+
  geom_point()+
  theme_classic()
FID_speed.FID

FID_Length<- ggplot(data2, aes(Length, FID))+ facet_wrap(~Treatment)+
  geom_point()+
  theme_classic()
FID_Length


FID_Length_Genus<- ggplot(data2, aes(Length, FID))+ facet_wrap(~Genus)+
  geom_point()+
  theme_classic()
FID_Length_Genus

####Normality####

shapiro.test(data2$FID)
shapiro.test(data2$Length)
shapiro.test(data2$speed.FID)
shapiro.test(data2$speed.prior.1)
shapiro.test(data2$speed.prior.2)
shapiro.test(data2$speed.prior.3)
shapiro.test(data2$DFS.FID)
shapiro.test(data2$DFS.prior.1)
shapiro.test(data2$DFS.prior.2)
shapiro.test(data2$DFS.prior.3)
shapiro.test(data2$DFF.FID)
shapiro.test(data2$DFF.prior.1)
shapiro.test(data2$DFF.prior.2)
shapiro.test(data2$DFF.prior.3)
shapiro.test(data2$DFF.post.FID)

#### Homogeinity #####

var.test(data2$FID, data2$Length)
var.test(data2$FID, data2$speed.FID)
var.test(data2$FID, data2$speed.prior.1)
var.test(data2$FID, data2$speed.prior.2)
var.test(data2$FID, data2$speed.prior.3)
var.test(data2$FID, data2$DFF.FID)
var.test(data2$FID, data2$DFF.prior.1)
var.test(data2$FID, data2$DFF.prior.2)
var.test(data2$FID, data2$DFF.prior.3)
var.test(data2$FID, data2$DFF.post.FID)
var.test(data2$FID, data2$DFS.FID)
var.test(data2$FID, data2$DFS.prior.1)
var.test(data2$FID, data2$DFS.prior.2)
var.test(data2$FID, data2$DFS.prior.3)
var.test(data2$speed.FID, data2$speed.prior.1)
var.test(data2$speed.FID, data2$speed.prior.2)
var.test(data2$speed.FID, data2$speed.prior.3)
var.test(data2$speed.FID, data2$DFF.FID)
var.test(data2$speed.FID, data2$DFF.prior.1)
var.test(data2$speed.FID, data2$DFF.prior.2)
var.test(data2$speed.FID, data2$DFF.prior.3)
var.test(data2$speed.FID, data2$DFF.post.FID)
var.test(data2$speed.FID, data2$DFS.FID)
var.test(data2$speed.FID, data2$DFS.prior.1)
var.test(data2$speed.FID, data2$DFS.prior.2)
var.test(data2$speed.FID, data2$DFS.prior.3)
var.test(data2$DFS.FID, data2$DFS.prior.1)
var.test(data2$DFS.FID, data2$DFS.prior.2)
var.test(data2$DFS.FID, data2$DFS.prior.3)
var.test(data2$DFS.FID, data2$DFF.FID)
var.test(data2$DFS.FID, data2$DFF.prior.1)
var.test(data2$DFS.FID, data2$DFF.prior.2)
var.test(data2$DFS.FID, data2$DFF.prior.3)
var.test(data2$DFS.FID, data2$DFF.post.FID)
var.test(data2$DFF.FID, data2$DFF.prior.1)
var.test(data2$DFF.FID, data2$DFF.prior.2)
var.test(data2$DFF.FID, data2$DFF.prior.3)
var.test(data2$DFF.FID, data2$DFF.post.FID)

### Independece ####

chisq.test(data2$Species, data2$School.Individual)
chisq.test(data2$Species, data2$Activity)

### Linear Regression ####

#FID~Length#
scatter.smooth(data2$Length, data2$FID)
cor.test(data2$Length, data2$FID)

FID.length<- lm(FID~Length, data = data2)
summary(FID.length)

#FID~DFS.FID#
scatter.smooth(data2$DFS.FID, data2$FID)
cor.test(data2$DFS.FID, data2$FID)

FID.DFS.FID<- lm(FID~DFS.FID, data = data2)
summary(FID.DFS.FID)

#FID~DFF.FID#
scatter.smooth(data2$DFF.FID, data2$FID)
cor.test(data2$DFF.FID, data2$FID)

FID.DFF.FID <- lm(FID~DFF.FID, data = data2)
summary(FID.DFF.FID)

### Models ####
setwd("~/Documents/Master UWA/thesis/Results")

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
data<- read.csv("finaldata.csv", na.strings = c("", NA))
head(data)

data <- data %>%
  dplyr::mutate(uniqueID = paste0(Fish.ID, OpCode))
data$uniqueID <- as.factor(data$uniqueID)

idmatch <- data %>%
  dplyr::select(uniqueID, Treatment, OpCode,
                Family, Genus, Species, Activity,
                School.Individual) %>%
  dplyr::distinct() %>%
  dplyr::arrange(uniqueID) %>% 
  dplyr::filter(!duplicated(uniqueID))

data2 <- data %>%
  pivot_wider(
    id_cols = uniqueID,
    names_from = Length.FID,
    values_from = Measurements,
  ) %>%
  left_join(idmatch, by = "uniqueID")

data2$Treatment <- as.factor(data2$Treatment)
data2$Species <- as.factor(data2$Species)
data2$Family <- as.factor(data2$Family)
data2$Genus <- as.factor(data2$Genus)
data2$OpCode <- as.factor(data2$OpCode)
data2$Activity <- as.factor(data2$Activity)
data2$School.Individual <- as.factor(data2$School.Individual)
data2$Length <- as.numeric(data2$Length, rm.na = TRUE)
data2$DFS.prior.1 <- as.numeric(data2$DFS.prior.1, rm.na = TRUE)
data2$DFS.prior.2 <- as.numeric(data2$DFS.prior.2, rm.na = TRUE)
data2$DFS.prior.3 <- as.numeric(data2$DFS.prior.3, rm.na = TRUE)
data2$DFS.FID <- as.numeric(data2$DFS.FID, rm.na = TRUE)


#rm(list=ls())


# install package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

data2.1 <- data2 %>%
  group_by(uniqueID)%>%
  mutate(DFSAvg=mean(c(DFS.prior.1, DFS.prior.2,DFS.prior.3), na.rm=T))  %>% 
  glimpse()

data3<- na.omit(data2) 

data4 <- data3 %>%
  group_by(uniqueID)%>%
  mutate(DFSAvg=mean(c(DFS.prior.1, DFS.prior.2,DFS.prior.3), na.rm=T))  %>% 
  glimpse()

cont.preds=c("Length","DFSAvg","DFS.FID") # use as continuous predictors.

cat.preds= c("Treatment","Genus", "Species", "School.Individual")

null.vars="site" # use as random effect and null model

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---


par(mfrow=c(1,1))
plot(data3$Length, data3$DFS.prior.1)
plot(data3$Length, data3$DFS.prior.2)
plot(data3$Length, data3$DFS.FID)


write.csv(data2, "data2.csv", sep=",")
write.csv(data3, "data3.csv", sep=",")
write.csv(data4, "data4.csv", sep=",")

# running a correlation of continuous variables####
cor(data4[,cont.preds])


# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in cont.preds) {
  x<-data4[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}
# Review of individual predictors - we have to make sure they have an even distribution---
#If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3

#why won't it let me include these new variables in my data.frame?
str(data2)
glimpse(data2)
data2<- as.data.frame(data2)
data2$sqrt.Length <- sqrt(data2$Length) #not sure which transformation to use

data2$log.Length <- log(data2$Length + 1) #between log and sqrt
data2$sqrt.DFS.prior.3 <- sqrt(data2$DFS.prior.3) # is it suggesting a relation?
data2$sqrt.DFS.FID <- sqrt(data2$DFS.FID) # is it suggesting a relation?
data2$sqrt.DFS.prior.2 <-sqrt(data2$DFS.prior.2) #don'think it is informative; remove?
data2$log.DFF.FID <- log(data2$DFF.FID+ 1) #??
data2$sqrt.DFF.prior.1 <- sqrt(data2$DFF.prior.1) #??

# Decided that Length, DFS.prior.3, DFS.FID needed a sqrt transformation and DFF.FID a log transformation.

#Decided DFF.prior.1, DFF.prior.2 were not informative variables. 

# # Re-set the predictors for modeling----


pred.vars=c("Length","DFS.prior.3","DFS.FID","DFF.FID","Treatment","Genus", "Species", "School.Individual") 

# Check to make sure Response vector has not more than 80% zeros----
#Do I need to do this since my FID has no value of zeros, I am not looking at abundance. finding it hard to apply since my study looks at something very different!!!
unique.vars=unique(as.character(data2$Treatment))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
 temp.dat=data2[which(data2$Treatment==unique.vars[i]),]
  if(length(which(temp.dat$FID==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     

#unique.vars=unique(as.numeric(data2$FID))
#unique.vars.use=numeric()
#for(i in 1:length(unique.vars)){
  #temp.dat=data2[which(data2$FID==unique.vars[i]),]
  #if(length(which(temp.dat$FID==0))/nrow(temp.dat)<0.8){
    #unique.vars.use=c(unique.vars.use,unique.vars[i])}
#}
#unique.vars.use  

resp.vars.=data2$FID

# take a look at the response variables
pdf(file="resp_vars.pdf",onefile=T)
for(r in 1:length(resp.vars)){
  par(mfrow=c(2,1))
  hist(dat[,resp.vars[r]],main=resp.vars[r])
  plot(jitter(dat[,resp.vars[r]]))
}
dev.off()
# Run the full subset model selection----
resp.vars=unique.vars.use
use.dat=data2
factor.vars=c("Treatment")# Treatment as a Factor with five levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Taxa==resp.vars[i]),]
  
  Model1=gam(response~s(lobster,k=3,bs='cr')+ s(Location,Site,bs="re"),
             family=tw(),  data=use.dat)
  #?gam  
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               linear.vars="Length",
                               k=3,
                               null.terms="s(Site,bs='re')")
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=3),]
  out.all=c(out.all,list(out.i))  
  