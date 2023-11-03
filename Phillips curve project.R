
# Time-Varying Phillips Curve for Rwanda 

# Libraries 
library(haven)
library(haven)
library(dplyr) 
library(magrittr)
library(readr)
library(foreign)
library(rlang)
library(readxl)
library(readstata13)
library(tidyr)
library(ggplot2)
library(tidyverse)
library (rdbnomics)
library(data.table)

# The data 
pc<-read_dta("all required data (1).dta")
View(pc)

  pc<-transform(pc,gaplag2=as.numeric(gaplag2))
# creating smaller subsets for the temporal analysis 

p1<-pc[1:20,]
p2<-pc[21:40,]
p3<-pc[41:66,]

# phillips curve 2006-2010


ph1<-p1%>%
ggplot(aes(x=gaplag2,y=inflation))+
  geom_point(color="blue")+
  geom_smooth(alpha=0.2)+
  geom_line(color="black",linetype=5,alpha=2)+
   theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "dark blue"))
ph1+
  labs(title = "Shape of the Rwandan Phillips Curve (2006-2010)",subtitle = "Lagged Relationship between the Output Gap and Inflation", x=" Output Gap",y="Inflation (%)",
       fontface="italic",caption = "Two quarters-lag on Output gap")+
  annotate("text",x=26,y=3,label="Relatively flat slope in this period",fontface="italic",angle=360)
  ggsave("2006-2010.png")
 
  # phillips curve 2011-2015
  
  ph2<-p2%>%
    ggplot(aes(x=gaplag2,y=inflation))+
    geom_point(color="blue")+
    geom_smooth(alpha=0.2)+
    geom_line(color="black",linetype=5,alpha=2)+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "dark blue"))
  ph2+
    labs(title = "Shape of the Rwandan Phillips Curve (2011-2015)",subtitle = "Lagged Relationship between the Output Gap and Inflation", x=" Output Gap",y="Inflation (%)",
         fontface="italic",caption = "Two quarters-lag on Output gap")+
    annotate("text",x=8,y=9,label="Slight downwards slope becoming apparent",fontface="italic",angle=360)
  ggsave("2011-2015.png")

  
  # phillips curve 2016-2022
  ph3<-p3%>%
    ggplot(aes(x=gaplag2,y=inflation))+
    geom_point(color="blue")+
    geom_smooth(alpha=0.2)+
    geom_line(color="black",linetype=5,alpha=2)+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "dark blue"))
  ph3+
    labs(title = "Shape of the Rwandan Phillips Curve (2016-2022)",subtitle = "Lagged Relationship between the Output Gap and Inflation", x=" Output Gap",y="Inflation (%)",
         fontface="italic",caption = "Two quarters-lag on Output gap")+
    annotate("text",x=-100,y=-4,label=" Inflationary pressures in 2022 certainly weakened the link",fontface="italic",angle=360,size=4)
  ggsave("2016-2022.png")
  
  
  # Overall Curve 
  ph4<-pc%>%
    ggplot(aes(x=gaplag2,y=inflation))+
    geom_point(color="blue")+
    geom_smooth(alpha=0.2)+
    geom_line(color="black",linetype=5,alpha=2)+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "dark blue"))
  ph4+
    labs(title = "Shape of the Rwandan Phillips Curve (2006-2022)",subtitle = "Lagged Relationship between the Output Gap and Inflation", x=" Output Gap",y="Inflation (%)",
         fontface="italic",caption = "Two quarters-lag on Output gap")+
    annotate("text",x=-100,y=-4,label=" Long-term Relationship",fontface="italic",angle=360,size=4)
  ggsave("2006-2022.png")
  