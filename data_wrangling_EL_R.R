############################
## Step 1: data exploration
## Script from Andrea
############################

setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

library(ggplot2)
library(tidyr)
library(dplyr)

data<- read.csv("finaldata .csv", na.strings = c("", NA))%>%
  mutate(Treatment=as.factor(Treatment))%>%
  mutate(Species=as.factor(Species))%>%
  mutate(Family=as.factor(Family))%>%
  mutate(Genus=as.factor(Genus))%>%
  mutate(OpCode=as.factor(OpCode))%>%
  mutate(Activity=as.factor(Activity))%>%
  mutate(School.Individual=as.factor(School.Individual))%>%
  mutate(Length.FID=as.factor(Length.FID))%>%
  dplyr::mutate(uniqueID = paste0(Fish.ID, OpCode))%>%
  mutate(uniqueID=as.factor(uniqueID))%>%
  glimpse()


head(data)

## Step 1 get Length.FID data from long to wide format
library(reshape2)

## get rid of all column expect unique id, length.FID and measurements

dat <- data %>%
  dplyr::select(uniqueID, Length.FID, Measurements)%>%
  glimpse()

summary(dat$Measurements)

## Turn Length.FID into wide format using info from Measurement column 

data_wide <- spread(dat, Length.FID, Measurements)
head(data_wide)

#Now join this to the rest of the variables of interest
head(data)

dat1 <- data %>%
  dplyr::select(Genus, Species, Treatment, Activity, School.Individual, uniqueID)%>%
  glimpse()


dat2 <- left_join(data_wide, dat1, by="uniqueID")%>%
  glimpse()


dat3 <- unique(dat2)

## I think this fixed the weird stuff that was happening with the NAs

## Step 2: Now we want to work out mean speed prior 1, 2, 3  and DFF prior 1, 2, 3 and DFS prior 1, 2, 3 

dat4 <- dat3 %>%
  group_by(uniqueID)%>%
  mutate(DFSAvg=mean(c(DFS.prior.1, DFS.prior.2,DFS.prior.3), na.rm=T))  %>% 
  mutate(DFFAvg=mean(c(DFF.prior.1, DFF.prior.2,DFF.prior.3), na.rm=T))  %>% 
  mutate(SpeedAvg=mean(c(speed.prior.1, speed.prior.2 ,speed.prior.3), na.rm=T))%>% 
  ungroup%>%
  dplyr::select(uniqueID, FID, Length, Treatment, Genus, Species, Activity, School.Individual, speed.FID, SpeedAvg, DFS.FID, DFSAvg)%>%
  glimpse()



write.csv(dat4, "wrangled_data_EL.csv")
## I think you should remove distance from fish because a lot of NAs and that is why you are losing all your 
## data when you run na.omit. Just use school as a factor.

## I think you should be good to run models with this? 