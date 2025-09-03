### Import Libraries ###
library(ggplot2)
library(scales)
library(dplyr)
library(extrafont)
library(ragg)
library(plotly)
library(ggallin)
library(ggfun)

file1="CNS_KL180615_003_abplane_Hscan002.dat"
data1 <- read.table(file1,sep=",",fill=TRUE,skip=28)
Temp <- round(data1$V3)
HField <- data1$V4
Mag <- data1$V61

data2 <- data.frame(Temp,HField,Mag)
data3 <- split(data2, f=data2$Temp)
T_List <- unique(data2$Temp)

for (T in T_List) {
  assign(paste("Loop_T_",T,sep=""),data.frame(data3[[sprintf("%s",T)]][2:3]))
}

### Plot Fraunhofer Pattern on a Continuous Color Scale, Color Filled, Color Rescaled, and Rastered ###
base_plot <- ggplot(data2[2:3]) +
  #geom_point(data=Loop_T_2,aes(x=HField,y=Mag,color='Data'),size=2) +
  geom_path(data=Loop_T_2,aes(x=HField,y=Mag,color='T_2'),linewidth=1.5) +
  geom_path(data=Loop_T_110,aes(x=HField,y=Mag,color='T_110'),linewidth=1.5) +
  geom_path(data=Loop_T_120,aes(x=HField,y=Mag,color='T_120'),linewidth=1.5) +
  geom_path(data=Loop_T_130,aes(x=HField,y=Mag,color='T_130'),linewidth=1.5) +
  geom_path(data=Loop_T_140,aes(x=HField,y=Mag,color='T_140'),linewidth=1.5) +
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
        legend.text.align = 0.5,
        legend.position=c(.1,.9),
        legend.justification=c('left','top'),
        legend.background=element_roundrect(size=1.5,,color='lightgray',r=unit(0.1,'snpc')),
        legend.margin=margin(t=7,r=7,b=7,l=7,unit="pt"),
        legend.key=element_rect(fill=NA, color=NA),
        panel.background=element_blank(),
        panel.grid.major=element_line(linewidth=0.75,linetype='solid',colour = "lightgray")) +
  coord_cartesian(xlim=c(min(data2$HField),max(data2$HField)),
                  ylim=c(min(data2$Mag),max(data2$Mag)),expand=TRUE) +
  scale_x_continuous(breaks=breaks_extended(n=5,Q=c(1,2,5,10,2.5),w=c(0.3, .9, 0.5, 0.05)),
                     labels = function(x) round(as.numeric(x), digits=3),
                     sec.axis = dup_axis(name=NULL,labels=NULL)) +
  scale_y_continuous(breaks=breaks_extended(n=5,w=c(0.3, .9, 0.5, 0.05)),
                     labels = function(x) round(as.numeric(x), digits=4),
                     sec.axis = dup_axis(name=NULL,labels=NULL)) +
  scale_color_manual(values = c("#377EB8","#E41A1C","#4DAF4A","#984EA3","#FF7F00"),
                     breaks=c('T_2','T_110','T_120','T_130','T_140'),
                     labels=c("2 K","110 K","120 K","130 K","140 K")) +
  labs(x='Applied Field (Oe)',
       y='Magnetization (emu)')

ggsave(filename=paste(strsplit(file1, ".", fixed = TRUE)[[1]][1],'.png',sep=""),
       device = ragg::agg_png,unit="in",width=431/45,height=55/9,dpi=300,plot=base_plot)
ggsave(filename=paste(strsplit(file1, ".", fixed = TRUE)[[1]][1],'.pdf',sep=""),
       device = cairo_pdf,unit="in",width=431/45,height=55/9,plot=base_plot)

base_plot

#bruh <- ggplotly(base_plot)
#
#htmlwidgets::saveWidget(as_widget(bruh),
#                        paste(strsplit(file1, ".", fixed = TRUE)[[1]][1],'.html',sep=""))