### Import Libraries ###
library(ggplot2)
library(scales)
library(dplyr)
library(extrafont)
library(ragg)
library(plotly)
library(ggallin)
library(ggfun)

file1="CNS_KL180615_003_abplane_001.dat"
file2="CNS_KL180615_003_abplane_002.dat"
data1 <- read.table(file1,sep=",",fill=TRUE,skip=28)
data2 <- read.table(file2,sep=",",fill=TRUE,skip=28)
Temp1 <- data1$V3
Mag1 <- data1$V61/500*1E6
Temp2 <- data2$V3
Mag2 <- data2$V61/500*1E6

data3 <- data.frame(Temp1,Mag1)
data4 <- data.frame(Temp2,Mag2)

### Plot Fraunhofer Pattern on a Continuous Color Scale, Color Filled, Color Rescaled, and Rastered ###
base_plot <- ggplot(data4) +
  #geom_point(data=Loop_T_2,aes(x=HField,y=Mag,color='Data'),size=2) +
  geom_path(data=data3,aes(x=Temp1,y=Mag1,color='ZFC'),linewidth=1.5) +
  geom_path(data=data4,aes(x=Temp2,y=Mag2,color='FC'),linewidth=1.5) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        line=element_line(linewidth=1.5,color="#222222"),
        plot.title = element_text(size = 26,color="#222222"),
        text=element_text(size=30,family="Segoe UI Semibold",color="#222222"),
        panel.border = element_rect(colour = "#222222", fill="NA", linewidth=3),
        axis.text.x=element_text(margin=unit(c(.5,0,0,0),"cm"),color="#222222"),
        axis.text.y=element_text(margin=unit(c(0,0.5,0,0),"cm"),color="#222222"),
        axis.ticks=element_line(color="#222222"),
        axis.ticks.length=unit(-0.25,"cm"), 
        legend.title=element_blank(),
        legend.text = element_text(hjust=0.5),
        legend.position="inside",
        legend.position.inside=c(.7,.9),
        legend.justification=c('left','top'),
        legend.background=element_roundrect(size=1.5,,color='lightgray',r=unit(0.1,'snpc')),
        legend.margin=margin(t=7,r=7,b=7,l=7,unit="pt"),
        legend.key=element_rect(fill=NA,color=NA),
        panel.background=element_blank(),
        panel.grid.major=element_line(linewidth=1,linetype='solid',colour = "lightgray")) +
  coord_cartesian(xlim=c(min(data4$Temp2),max(data4$Temp2)),
                  ylim=c(min(data4$Mag2),max(data4$Mag2)),expand=TRUE) +
  scale_x_continuous(breaks=breaks_extended(n=5,Q=c(1,2,5,10,2.5),w=c(0.3, .9, 0.5, 0.05)),
                     labels = function(x) round(as.numeric(x), digits=5),
                     sec.axis = dup_axis(name=NULL,labels=NULL)) +
  scale_y_continuous(breaks=breaks_extended(n=5,w=c(0.3, .9, 0.5, 0.05)),
                     labels = function(x) round(as.numeric(x), digits=15),
                     sec.axis = dup_axis(name=NULL,labels=NULL)) +
  scale_color_manual(values = c("#377EB8","#E41A1C"),
                     breaks=c('FC','ZFC'),
                     labels=c("FC","ZFC")) +
  labs(x='Temperature (K)',
       y=paste('\UAB55 (emu/Oe)','\U2A2F 10\U207B\U2076'))

ggsave(filename=paste(strsplit(file1, ".", fixed = TRUE)[[1]][1],'.png',sep=""),
       device = ragg::agg_png,unit="in",width=431/45,height=55/9,dpi=300,plot=base_plot)
ggsave(filename=paste(strsplit(file1, ".", fixed = TRUE)[[1]][1],'.pdf',sep=""),
       device = cairo_pdf,unit="in",width=431/45,height=55/9,plot=base_plot)

base_plot

#bruh <- ggplotly(base_plot)
#
#htmlwidgets::saveWidget(as_widget(bruh),
#                        paste(strsplit(file1, ".", fixed = TRUE)[[1]][1],'.html',sep=""))