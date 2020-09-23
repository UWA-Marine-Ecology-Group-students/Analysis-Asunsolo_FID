setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

library(plyr)
library(tidyverse)
library(readxl)
library(dplyr)

dat<- read.csv("data_wide_BG_AA_edited.csv")

#### Getting min, max and mean for schools ####


schools<- dat[dat$school_individual == 'School',] %>%
  dplyr:: mutate (school_id = paste0(`Treatment`, opcode)) 

names(schools)
schools$school_id<- as.factor(schools$school_id)

school.mean.var<- dplyr::select(schools, dfs.fid, dfs.prior.1, dfs.prior.2, dfs.prior.3, fid, length, speed.fid, speed.prior.1, speed.prior.2, speed.prior.3, DFSAvg, speed.priorAvg, school_id)


school.mean <- aggregate(school.mean.var[c(1:12)], list (school.mean.var$school_id), MARGIN =  2, FUN = mean)

school.mean$Treatment<- tapply(schools$Treatment, schools$school_id, function(x)  as.character(x) [1])

school.mean$opcode<- tapply(schools$opcode, schools$school_id, function(x) as.character(x)[1])

school.mean$family<- tapply(schools$family, schools$school_id, function(x) as.character(x)[1])

school.mean$genus<- tapply(schools$genus, schools$school_id, function(x) as.character(x)[1])

school.mean$species<- tapply(schools$species, schools$school_id, function(x) as.character(x)[1])

school.mean$activity<- tapply(schools$activity, schools$school_id, function(x) as.character(x)[1])

school.mean$school_individual<- tapply(schools$school_individual, schools$school_id, function(x) as.character(x)[1])

school.mean$scientific<- tapply(schools$scientific, schools$school_id, function(x) as.character(x)[1])

school.mean$site<- tapply(schools$site, schools$school_id, function(x) as.character(x)[1])

school.mean<- dplyr::rename(school.mean, unique_id = Group.1 )
dat1<-dplyr::filter(dat, !school_individual=="School")%>%
  dplyr::select(!X)

dat<- rbind(dat1,school.mean)
dat<- dplyr::filter(dat, !genus=="Pomacanthus")

write.csv(dat, "data_wide_SchoolsMean_BG_AA.csv")

#Check why the factors are going as numbers!!!!!!

#### max ####
school.max<- aggregate(school.mean.var[c(1:12)], list (school.mean.var$school_id), MARGIN =  2, FUN = max)

school.max

### Min #####
school.min<- aggregate(school.mean.var[c(1:12)], list (school.mean.var$school_id), MARGIN =  2, FUN = min)

school.min
