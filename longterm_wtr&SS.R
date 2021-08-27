library(ggplot2) #nice plots
library (TimeSeries) #for timeseries
library(lubridate) #for timeseries
library(scales) #labels and axis
library(cowplot) #nice figures
library(gridExtra) #to plot more than 1 plot in the same figure
library(zoo) #for timeseries
library(gridGraphics) #to plot more than 1 plot in the same figure
library(corrplot) #correlograms
library(tidyverse) #manipulate datasets
library(reshape2)
library(metR)

#set working directory
setwd("C:/Users/calderom/OneDrive - Dundalk Institute of Technology/LongtermData/2004_2020_datasets")

#input file
metadata <- read.csv("dailymetadata_2004to2020.csv", stringsAsFactors = T)
metadata$Date <- as.Date(metadata$Date,"%d/%m/%Y")
names(metadata)

#select wtr data
wtr <- metadata %>% 
  select(Date,wtr_0.9, wtr_2.5, wtr_5, wtr_8, wtr_11, wtr_14, wtr_16, wtr_18, wtr_20, wtr_22, wtr_27, wtr_32, wtr_42)
names(wtr)
# Convert wide water temp to long
wtr_long <- melt(wtr, id.vars = 'Date')
colnames(wtr_long) <- c('date', 'depths', 'temp')
wtr_long$depths <- gsub('wtr_', '', wtr_long$depths) # remove 'wtr_'
wtr_long$depths <- as.numeric(wtr_long$depths)
head(wtr_long)
str(wtr_long)
wtr_long$date <- as.Date(wtr_long$date)
#Heatmap wtr
my.cols = rev(RColorBrewer::brewer.pal(11, 'Spectral')) #Colours for temp plot
binwidth = 0.5
p1 <- ggplot(wtr_long, aes(date, depths)) +
  geom_contour_fill(aes(z = temp), na.fill = T, binwidth = binwidth) +    # filled contours!
  scale_fill_gradientn(colours=my.cols, na.value = 'white', name="Water\nTemp \nºC")+
  scale_y_reverse()+
  ylab('Depths (m)')+
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  coord_cartesian(ylim = c(42,0))+
  theme(legend.key.height = unit(0.5, 'cm'), legend.key.width = unit(0.5, 'cm'),  axis.title.x = element_blank())
p1
#SS 
ss <- metadata %>% 
  select(Date,SS)
p2 <- ggplot(ss, aes(Date, SS)) +
  geom_line(colour='black', size=1)+
  ylab(~paste("Schmidt stability ", "(J", "·m"^-2, ")")) +
  scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%Y")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_blank())+
  #blue (-change < -100 = towards fast mixing)
  geom_vline(xintercept=as.numeric(as.Date("2018-06-30")), linetype=4, size=1, colour="blue")+
  geom_vline(xintercept=as.numeric(as.Date("2018-06-13")), linetype=4, size=1, colour="blue")+
  geom_vline(xintercept=as.numeric(as.Date("2011-07-16")), linetype=4, size=1, colour="blue")+
  geom_vline(xintercept=as.numeric(as.Date("2012-06-07")), linetype=4, size=1, colour="blue")+
  geom_vline(xintercept=as.numeric(as.Date("2013-08-02")), linetype=4, size=1, colour="blue")+
  geom_vline(xintercept=as.numeric(as.Date("2007-08-11")), linetype=4, size=1, colour="blue")+
  geom_vline(xintercept=as.numeric(as.Date("2016-06-14")), linetype=4, size=1, colour="blue")+
  #red(+change > 100 = towards fast stratification)
  #geom_vline(xintercept=as.numeric(as.Date("2018-05-31")), linetype=1, size=1, colour="red")+
  #geom_vline(xintercept=as.numeric(as.Date("2007-08-10")), linetype=1, size=1, colour="red")+
  #geom_vline(xintercept=as.numeric(as.Date("2018-06-29")), linetype=1, size=1, colour="red")+
  #geom_vline(xintercept=as.numeric(as.Date("2013-07-19")), linetype=1, size=1, colour="red")+
  #red rectangle when > 4 consecutive days with ss > 400
  annotate("rect",
           xmin = as.Date("2005-07-11"), xmax = as.Date("2005-07-19"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2006-07-17"), xmax = as.Date("2006-07-31"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2007-06-10"), xmax = as.Date("2007-06-14"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2009-06-25"), xmax = as.Date("2009-07-11"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2010-06-21"), xmax = as.Date("2010-07-04"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2013-07-12"), xmax = as.Date("2013-08-11"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2014-06-20"), xmax = as.Date("2014-08-15"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2016-06-05"), xmax = as.Date("2016-06-15"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2018-06-01"), xmax = as.Date("2018-06-13"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) +
  annotate("rect",
           xmin = as.Date("2018-06-29"), xmax = as.Date("2018-08-10"), 
           ymin = -Inf, ymax = Inf,  fill = "red", alpha=.5) 
    #geom_hline(yintercept=400, linetype=2, size=0.5, colour="black")
p2
#wtr_mean
meanwtr <- metadata %>% 
  select(Year, DOY, wtr_0.9, wtr_2.5, wtr_5, wtr_8, wtr_11, wtr_14, wtr_16, wtr_18, wtr_20, wtr_22, wtr_27, wtr_32, wtr_42)
#add wtr_mean variable
meanwtr$wtr_mean <- rowMeans(meanwtr[,3:15], na.rm=TRUE)
names(meanwtr)
p3 <- ggplot(meanwtr, aes(Year, DOY, fill= wtr_mean)) + 
  geom_tile()+
  scale_fill_gradientn(colours=my.cols, na.value = 'white', name="Mean\nWater\nTemp \nºC")+
  ylab("DOY")+
  scale_y_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 30))+
  scale_x_continuous(limits = c(2003, 2020), breaks = seq(2004, 2020, by = 1))+
  theme(text = element_text(size = 12),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")+
  theme(text = element_text(size = 12), axis.title.x = element_blank())

p3

p4 <- plot_grid(p1, p2, p3,
                align="hv", axis="tblr", ncol = 1)
title <- ggdraw() + 
  draw_label("Lake physical data (2004-2020)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))
plot_grid(title, p4, ncol=1, rel_heights = c(0.1, 1))



