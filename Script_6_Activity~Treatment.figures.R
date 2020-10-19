setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

dat<-read.csv("data_wide_SchoolsMean_BG_AA.csv")

library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(scales)
library(reshape2)
library(RColorBrewer)
#library(png)
#library(grid)
#library(cowplot)
#library(magick)


no.resp<- filter(dat, activity == "no response")

escape<- filter(dat, !activity == "no response")

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
    axis.title.x=element_text(vjust=0.3, size=15, face="bold"),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15, face="bold"),
    axis.text.y=element_text(size=12),
    axis.text.x=element_text(vjust=0.6, angle=45,size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=12, face="bold.italic"))

## Read in fish pic

#size = unit(10, "cm")
#diver <- readPNG("diver.png")
#g<- rasterGrob(diver, interpolate=TRUE)

##Make a nice plot###

my_plot<- ggplot(dat, aes(x= activity,  group=Treatment)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",show.legend=TRUE) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", x= "Type of response",fill="Type of response") +
  facet_grid(~Treatment) +
  scale_y_continuous(labels = scales::percent)+
  Theme1

  #theme_cowplot()
  #annotation_custom(g, xmin=1.44, xmax=3.44, ymin=41, ymax=53)
 #annotate("text", x = -Inf, y=Inf, label ="Diver",vjust = 1, hjust = -.1,size=5)

my_plot2<- my_plot +  scale_fill_manual(values=c( "#F08A5D", "#C70039","#A35D6A", "white"))
my_plot2
#CD0A0A
#help("draw_image")
#help(image_re)
#diver <- system.file("diver.html", package = "cowplot")
#my_plot_2 <- ggdraw() +
 # draw_image(diver,  x = 0.0, y = 0.4, scale = .2) +
 # draw_plot(my_plot)
#my_plot_2

#### No response graph####

my_plot1<- ggplot(no.resp, aes(x= Treatment,  group=activity, fill = Treatment)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",show.legend=TRUE) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent of no response", x= "Treatment") +
  scale_y_continuous(labels = scales::percent)+
  Theme1
my_plot1

final.plot<- my_plot1 +  scale_fill_manual(values=c( "#C70039","#2BB2BB", "#8FC0A9","#F08A5D","#8675A9" ))
final.plot

### Escape responses ####

my_plot3<- ggplot(escape, aes(x= activity,  group=Treatment)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",show.legend=TRUE) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", x= "Type of response",fill="Type of response") +
  facet_grid(~Treatment) +
  scale_y_continuous(labels = scales::percent)+
  Theme1

escape<- my_plot3 +  scale_fill_manual(values=c( "#F08A5D", "#C70039","#A35D6A", "#2BB2BB"))
escape
