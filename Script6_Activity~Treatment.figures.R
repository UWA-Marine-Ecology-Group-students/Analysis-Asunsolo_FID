setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

dat<-read.csv("data_wide_SchoolsMean_BG_AA.csv")

library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(scales)
library(reshape2)
library(png)

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
    legend.text = element_text(size=8),
    legend.title = element_text(vjust=0.3, size=10),
    #legend.position = "top",
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.y=element_text(size=12),
    axis.text.x=element_text(vjust=0.6, angle=45,size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=12, face="bold.italic"))


ggplot(dat, aes(x= activity,  group=Treatment)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Activity") +
  facet_grid(~Treatment) +
  scale_y_continuous(labels = scales::percent)+
  Theme1

## Read in fish pic
dir()
size = unit(10, "cm")
largepic <- readPNG("large.png")%>%
  rasterGrob(interpolate=TRUE)


## Make nice plot

ggmod.log.small <-  ggplot(aes(x=log.small ,y=response), data=predicts.log.small)+
  ylab("
       ")+
  xlab('Log no. small competitors')+
  geom_line(data=predicts.log.small,aes(x=log.small,y=response),colour="#293462",alpha=0.8,size=1,show.legend=TRUE)+
  geom_ribbon(aes(ymin=response-se.fit, ymax=response + se.fit), alpha=0.4, fill="#293462", linetype='blank')+
  theme_classic()+
  Theme2+
  annotation_custom(largepic, xmin=1.44, xmax=3.44, ymin=41, ymax=53)+
  annotate("text", x = -Inf, y=Inf, label = "(b)",vjust = 1, hjust = -.1,size=6)