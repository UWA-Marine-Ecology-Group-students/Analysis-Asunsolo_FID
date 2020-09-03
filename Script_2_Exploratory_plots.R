
setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

dat<-read.csv("data_wide_BG_AA.csv")

library(ggplot2)
library(dplyr)
library(tidyr)
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

#eliminated DFF variable
#boxplot(dat$dff.fid~ dat$dff.post.fid, xlab= "DFF FID (mm)", ylab= "DFF post FID (mm)")
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
datlong.1<-gather(dat, speed.priorAvg, speed.fid, key = "speed", value = "value", na.rm = FALSE,
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

scatter.smooth(dat$speed.FID, dat$FID, xlab= "Speed FID (m/s)", ylab= "FID (mm)")

#Speed prior vs during FID-- can I do a boxplot?
FID_speed<- ggplot(dat, aes(speed.priorAvg, speed.fid))+ facet_wrap(~Treatment)+
geom_point()+
theme_classic()
FID_speed

#Speed during FID per treatment
FID_speed.FID<- ggplot(dat, aes(fid, speed.fid))+ facet_wrap(~Treatment)+
 geom_point()+
theme_classic()
FID_speed.FID

FID_Length<- ggplot(dat, aes(length, fid))+ facet_wrap(~Treatment)+
 geom_point()+
 theme_classic()
FID_Length


FID_Length_Genus<- ggplot(dat, aes(length, fid))+ facet_wrap(~genus)+
# geom_point()+
#theme_classic()
FID_Length_Genus
