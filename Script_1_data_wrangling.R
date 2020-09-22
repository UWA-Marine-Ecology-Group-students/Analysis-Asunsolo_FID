
setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

library(plyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(GlobalArchive)
#.Library

us_tolower <- function(x) {
  tolower(gsub(".", "_", x, fixed = TRUE))
}

col_tolower <- function(x) {
  us_tolower(x) %>%
    gsub("__", "_", x = ., fixed = TRUE) %>%
    gsub("\\_$", "", x = .)
}

extract_time <- function(x) {
  x <- tolower(gsub("(time", "(", x, fixed = TRUE))
  if (grepl("sec)", x, fixed = TRUE)) {
    strsplit(x, " sec)", fixed = TRUE)[[1]][1] %>%
      strsplit(., "(", fixed = TRUE) %>%
      .[[1]] %>% .[2] %>%
      gsub(" ", "", ., fixed = TRUE) %>%
      as.numeric
  } else {
    NA
  }
}

# read the matching file for correct fid_vars names

better_names <- "better_names.csv" %>%
  read.csv(file = ., header = TRUE) %>%
  dplyr::mutate(across(where(is.factor), as.character)) %>%
  dplyr::mutate(new = tolower(new))

df_names <- better_names %>%
  dplyr::mutate(new = tolower(new))%>%
  dplyr::rename(`Length/FID`=old, fid_vars=new)

df_names$`Length/FID`<-gsub(' +',' ',df_names$`Length/FID`) 

df_names<-distinct(df_names)

new_names <- better_names$new
names(new_names) <- better_names$old

# read the marching file for speed,DFS, DFF with value zero
#addedrows<- read_csv("added rows DFF DFS and Speed value zero.csv")

#addedrows$Filename <- as.character(addedrows$Filename)
#addedrows$Frame <- as.numeric(addedrows$Frame)
#addedrows$Period <- as.character(addedrows$Period)
#data<- rbind(data,addedrows, match.names=FALSE ) %>%

data.raw <- "Data output 12_August_2020_activityUpdated.xlsx" %>%
  readxl::read_excel(path = ., sheet = 1) %>%
  dplyr::mutate(`Length/FID`=tolower(`Length/FID`))%>%
  dplyr::rename(Length=`Length (mm)`)%>%
  dplyr::mutate(Length=as.numeric(Length))%>%
  dplyr::select(`Time (mins)`,Period,Length,OpCode,Family,Genus,Species,Activity,`Fish ID`,`Length/FID`,`School/Individual`)%>%
  glimpse()

data.raw$`Length/FID`<-gsub(' +',' ',data.raw$`Length/FID`) 

addedrows <- "added rows DFF DFS and Speed value zero.xlsx" %>%
  readxl::read_excel(path = ., sheet = 1) %>%
  dplyr::rename(Length=`Length (mm)`)%>%
  dplyr::mutate(Length=as.numeric(Length))%>%
  dplyr::mutate(`Length/FID`=tolower(`Length/FID`))%>%
  dplyr::select(`Time (mins)`,Period,Length,OpCode,Family,Genus,Species,Activity,`Fish ID`,`Length/FID`,`School/Individual`)%>%
  glimpse()

# data.com<- rbind(data.raw,addedrows, match.names=FALSE ) %>%
#   dplyr::mutate(Length=as.numeric(Length))%>%
#   glimpse()

2178+332

data.com<-bind_rows(data.raw,addedrows)
  
data<-data.com%>%
  left_join(df_names)%>%
  dplyr::mutate(time_sec = sapply(`Length/FID`, extract_time),
                length_m = Length * 1e-3,
                speed_m_sec = length_m / time_sec,
                measurements = speed_m_sec,
                #fid_vars = tolower(gsub("  ", " ", `Length/FID`)),
                #fid_vars = ifelse(grepl(" (", fid_vars, fixed = TRUE),
                #                  strsplit(fid_vars, " (",
                #                            fixed = TRUE)[[1]][1],
                #                   fid_vars),
                # fid_vars = new_names[fid_vars],
                unique_id = paste0(`Fish ID`, OpCode)) %>%
  dplyr::rename(length_mm=Length)%>%
  dplyr::filter(!is.na(fid_vars)) %>%
  data.frame %>%
  dplyr::rename_with(col_tolower) %>%
  dplyr::select(period,unique_id,family,genus,species,activity,fish_id, school_individual, measurements, fid_vars,speed_m_sec,length_m,length_mm, opcode)%>%
  tidyr::replace_na(list(species="spp"))%>%glimpse()

names(data)
glimpse(data)

for (i in seq_len(nrow(data))) {
  if (is.na(data$speed_m_sec[i]) &
      !is.na(data$length_m[i])) {
    data$measurements[i] <- data[i, "length_mm"]
  }
}

# check to make sure there are no duplicates
#dup_check <- plyr::ddply(data, .(opcode, fish_id, length_fid), nrow)
#dup_check[dup_check$V1 != 1, ] # empty, good

idmatch <- data %>%
  dplyr::select(unique_id, period, opcode,
                family, genus, species, activity,
                school_individual) %>%
  na.omit() %>%
  dplyr::distinct() %>%
  dplyr::arrange(unique_id) %>%
  dplyr::filter(!duplicated(unique_id))

length(unique(data$unique_id)) == nrow(idmatch)

test<-wide <- distinct(data)%>%
  select(unique_id,fid_vars,measurements)%>%
  group_by(unique_id,fid_vars)%>%
  summarise(n=n())

data_wide <- distinct(data)%>%
  select(unique_id,fid_vars,measurements)%>%
  spread(.,fid_vars,measurements)%>%
  dplyr::select (-prior.range.1, -prior.range.2, -prior.range.3, -range.1, -dfc.prior.1, -dfc.prior.2, -dfc.prior.3, -fid.range, -range.2, -dff.prior.1, -dff.prior.2, -dff.prior.3, -dff.fid, -dff.post.fid)%>%
  left_join(idmatch)%>%
  dplyr::rename(Treatment=period) %>%
  glimpse()


# data_wide <- data %>%
#   tidyr::pivot_wider(id_cols = unique_id,
#                      names_from = fid_vars,
#                      values_from = measurements) %>%
#   dplyr::left_join(idmatch, by = "unique_id") %>%
#   as.data.frame%>%
#   glimpse()
# 
# data_wide <- dplyr::select (data_wide, -prior.range.1, -prior.range.2, -prior.range.3, -range.1, -dfc.prior.1, -dfc.prior.2, -dfc.prior.3, -fid.range, -range.2, -dff.prior.1, -dff.prior.2, -dff.prior.3, -dff.fid, -dff.post.fid)
# 
# data_wide <- dplyr::rename(data_wide, Treatment=period) 
# glimpse(data_wide)
# dim(data_wide)
# head(data_wide)


dat <- data_wide %>%
  # mutate(dfs.prior.1=as.numeric(dfs.prior.1))
  # group_by(unique_id)%>%
  #change that into average per uniqueID 
  ungroup()%>%
  rowwise() %>% 
  mutate(DFSAvg=mean(c(dfs.prior.1,dfs.prior.2,dfs.prior.3), na.rm=T)) %>%
  # mutate(DFSAvg=(dfs.prior.1,dfs.prior.2,dfs.prior.3),na.rm=T))  %>%
  #change that into average per uniqueID
  mutate(speed.priorAvg=mean(c(speed.prior.1, speed.prior.2,speed.prior.3), na.rm=T))  %>%
  mutate(scientific=paste(genus,species, sep="."))%>%
  glimpse()

prefix <- sub("_.*", "", dat$opcode)
prefix
site <- substr(prefix, 7, 7)
site

dat$site <- site
glimpse(dat)

#### trying to get min, max and mean for schools ####

schools<- dat[dat$school_individual == 'School',] %>%
dplyr:: mutate (school_id = paste0(`Treatment`, opcode)) 

names(schools)


school.mean.var<- dplyr::select(schools, dfs.fid, dfs.prior.1, dfs.prior.2, dfs.prior.3, fid, length, speed.fid, speed.prior.1, speed.prior.2, speed.prior.3, DFSAvg, speed.priorAvg, school_id)

school.mean <- aggregate(school.mean.var[c(1:12)], list (school.mean.var$school_id), MARGIN =  2, FUN = mean)

school.mean$Treatment<- tapply(schools$Treatment, schools$school_id, function(x) x[1])

school.mean$opcode<- tapply(schools$opcode, schools$school_id, function(x) x[1])

school.mean$family<- tapply(schools$family, schools$school_id, function(x) x[1])

school.mean$genus<- tapply(schools$genus, schools$school_id, function(x) x[1])

school.mean$species<- tapply(schools$species, schools$school_id, function(x) x[1])

school.mean$activity<- tapply(schools$activity, schools$school_id, function(x) x[1])

school.mean$school_individual<- tapply(schools$school_individual, schools$school_id, function(x) x[1])

school.mean$scientific<- tapply(schools$scientific, schools$school_id, function(x) x[1])

school.mean$site<- tapply(schools$site, schools$school_id, function(x) x[1])

school.mean<- dplyr::rename(school.mean, unique_id = Group.1 )
dat1<-dplyr::filter(dat, !school_individual=="School")
dat<- rbind(dat1,school.mean)
#### max ####
school.max<- aggregate(school.mean.var[c(1:12)], list (school.mean.var$school_id), MARGIN =  2, FUN = max)

school.max

### Min #####
school.min<- aggregate(school.mean.var[c(1:12)], list (school.mean.var$school_id), MARGIN =  2, FUN = min)

school.min



write.csv(dat, "data_wide_SchoolMean_BG_AA.csv")
