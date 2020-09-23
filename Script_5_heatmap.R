setwd("~/Documents/Master UWA/thesis/Results/GitHub/Analysis-Asunsolo_FID")

####Librarys####
library(dplyr)
library(tidyr)
#fid<- read.csv("FID_all.var.imp.csv")
#speed.fid<- read.csv("FID_all.var.imp.speed.fid.csv")

#data<-rbind(fid,speed.fid)

data<- read.csv("ALL_imp_var_fid_speedfid_SchoolMean.csv")%>%
  dplyr::rename(resp.var= X)%>%
  tidyr::gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

# Part 2 - custom plot of importance scores----


# Plotting defaults----
library(ggplot2)
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=8),
    legend.title = element_text(size=8, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,angle = 90, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10,face="italic"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


# colour ramps-
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

# Labels-
legend_title<-"Importance"

# Annotations-
data.label<-data%>%
  mutate(label=NA)%>%
  mutate(label=ifelse((predictor=="log.length"&resp.var=="fid"),"X",
               ifelse((predictor=="Treatment"&resp.var=="fid"),"X",label)))%>%
  mutate(label=ifelse(predictor=="log.length"&resp.var=="speed.fid","X",ifelse(predictor=="sqrt.speed.priorAvg"&resp.var=="speed.fid","X",label)))%>%
  glimpse()

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(data.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "#878278",
                       limits = c(0, max(data.label$importance)))+
  scale_x_discrete(limits=c("log.length",
                            "Treatment",
                            "log.DFSAvg",
                            "sqrt.speed.priorAvg",
                            "genus",
                            "school_individual"),
                   labels=c(
                     "log.length",
                     "Treatment",
                     "log.DFSAvg",
                     "sqrt.speed.priorAvg",
                     "genus",
                     "school_individual"
                   ))+
  scale_y_discrete(limits = c("fid",
                              "speed.fid"),
                   labels=c("fid",
                            "speed.fid"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  geom_text(aes(label=label))
gg.importance.scores
