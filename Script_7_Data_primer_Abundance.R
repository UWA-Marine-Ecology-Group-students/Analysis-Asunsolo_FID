setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

library(plyr)
library(tidyverse)
library(readxl)
library(dplyr)

data<- read.csv("data_wide_SchoolsMean_BG_AA.csv")

data<-dplyr:: mutate (data, rep_id = paste0(`Treatment`, opcode)) 

dat.abund<-dplyr:: select(data, scientific, rep_id)

abundance<-count(dat.abund, c("rep_id", "scientific"))


abundance.wide<-spread(abundance, rep_id, freq)%>%
  dplyr::select(!V1)%>%
  glimpse()

write.csv(abundance.wide, "data_abundance_Primer.csv")
