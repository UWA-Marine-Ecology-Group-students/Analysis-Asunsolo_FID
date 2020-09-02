
setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

library(plyr)
library(tidyverse)
library(readxl)
library(dplyr)
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

data <- "Data output 12_August_2020_activityUpdated.xlsx" %>%
  readxl::read_excel(path = ., sheet = 1) %>%
  dplyr::mutate(`Length/FID`=tolower(`Length/FID`))

data$`Length/FID`<-gsub(' +',' ',data$`Length/FID`) 

data<-data%>%
  left_join(df_names)%>%
  dplyr::mutate(time_sec = sapply(`Length/FID`, extract_time),
                length_m = `Length (mm)` * 1e-3,
                speed_m_sec = length_m / time_sec,
                measurements = speed_m_sec,
                #fid_vars = tolower(gsub("  ", " ", `Length/FID`)),
                #fid_vars = ifelse(grepl(" (", fid_vars, fixed = TRUE),
                #                  strsplit(fid_vars, " (",
                #                            fixed = TRUE)[[1]][1],
                #                   fid_vars),
                # fid_vars = new_names[fid_vars],
                unique_id = paste0(`Fish ID`, OpCode)) %>%
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


write.csv(dat, "data_wide_BG_AA.csv")
#### PLOTS ####

#how to get the data for only one variable within a column
Perspex<- dplyr::filter(dat, Treatment %in% c("Perspex"))
Perspex

par(mfrow=c(1,1))

boxplot(dat$fid~dat$Treatment, xlab= "Treatment", ylab= "FID (mm)")

scatter.smooth(dat$length, dat$fid, xlab= "Fish size (mm)", ylab= "FID (mm)")
dotchart(dat$length, dat$fid, xlab= "Fish size (mm)", ylab= "FID (mm)")


boxplot(dat$fid~dat$species, xlab= "Sp", ylab= "FID (mm)", las=3, cex= 0.2)

boxplot(dat$fid~dat$genus, xlab= "Genus", ylab= "FID (mm)", las=3, cex= 0.2)

plot(dat$Treatment, dat$activity, xlab= "Treatment", ylab= "Type of reaction", col=
       1:length(dat$activity))
#legend(1,95, legend=c("swim backwards", "no response", "hide", "flight", "c-turn"))

plot(dat$Treatment, dat$length, xlab= "Treatment", ylab= "Length")
dotchart(dat$length, dat$Treatment, xlab= "Fish size (mm)", ylab= "Treatment")

scatter.smooth(dat$dff_fid, dat$fid, xlab= "DFF FID (mm)", ylab= "FID (mm)")

scatter.smooth(dat$dfs_fid, dat$fid, xlab= "DFS FID (mm)", ylab= "FID (mm)")

boxplot(dat$dff_fid~ dat$dff_post_fid, xlab= "DFF FID (mm)", ylab= "DFF post FID (mm)")
#boxplot(dat$DFF.post.FID~ dat$DFF.FID)

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

#datlong<- gather(dat, dff_fid, dff_post_fid, key = "DFF", value = "value", na.rm = FALSE,
                   #convert = FALSE, factor_key = FALSE) 
datlong.1<-gather(dat, speed.prior.3, speed.fid, key = "speed", value = "value", na.rm = FALSE,
                    convert = FALSE, factor_key = FALSE) 

#ggplot(datlong.1,aes(x = factor(Treatment), y = value,  fill = DFF, notch=FALSE, outlier.shape = NA),alpha=0.5) +
  #stat_boxplot(geom='errorbar')+
  #geom_boxplot(outlier.color = NA, notch=FALSE)+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  #scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+ # this sets 10% above the max for each on the Y scale
  # scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
  #xlab("DFF") + ylab("Distance") +
  #annotation_custom(grob.sci)+ #adds a title
  #Theme1

#get speed in the data first

ggplot(datlong.1,aes(x = factor(Treatment), y = value,  fill = speed, notch=FALSE, outlier.shape = NA),alpha=0.5) +
  stat_boxplot(geom='errorbar')+
  geom_boxplot(outlier.color = NA, notch=FALSE)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+ # this sets 10% above the max for each on the Y scale
   #scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))+
  xlab("DFF") + ylab("Distance") +
  #annotation_custom(grob.sci)+ #adds a title
  Theme1

plot(dat$school_individual, dat$fid, xlab= "Treatment", ylab= "FID (mm)")
boxplot(dat$fid~dat$school_individual*dat$Treatment, xlab= "", ylab= "FID (mm)", las=3, cex= 0.2)

#scatter.smooth(dat$speed.FID, dat$FID, xlab= "Speed FID (m/s)", ylab= "FID (mm)")

#Speed prior vs during FID-- can I do a boxplot?
#FID_speed<- ggplot(dat, aes(speed.prior.3, speed.FID))+ facet_wrap(~Treatment)+
#  geom_point()+
 # theme_classic()
#FID_speed

#Speed during FID per treatment
#FID_speed.FID<- ggplot(dat, aes(FID, speed.FID))+ facet_wrap(~Treatment)+
#  geom_point()+
 # theme_classic()
#FID_speed.FID

#FID_Length<- ggplot(dat, aes(Length, FID))+ facet_wrap(~Treatment)+
# geom_point()+
 # theme_classic()
#FID_Length


#FID_Length_Genus<- ggplot(dat, aes(Length, FID))+ facet_wrap(~Genus)+
#  geom_point()+
# theme_classic()
#FID_Length_Genus
