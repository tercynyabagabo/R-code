################################################################################################
# DBNOMICS: fetching Rwandan Public debt data   
# Installing packages 
#######################################################
install.packages("rdbnomics")

# loading packages 

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

# dbnomics 
vignette("rdbnomics")

# Fetching Rwandan public sector debt indicators 

   # Joint External Debt Hub (WB/JED)#

tot_mult<-rdb(ids ="WB/JED/Q-Q.1E0.1E0.C.9B.IFI.LMTT.1.ALL.NV.TO1.ALL-RWA")%>%
  select(period,value)

  # Data cleaning 

  tot_mult<-tot_mult[(1:80),]
 
  tot_mult1<-tot_mult%>%
    mutate(value_1=value/1000)
  
  # Data Visualization
  
    # Multilateral loans 
  tot_mult1<-tot_mult1[-(49:51),]%>%
    mutate(value_2=value_1/1000)
  
    plot_1<-ggplot(tot_mult1)+
    geom_bar(stat="identity",fill="#4E6888",aes(x=period,y=value_2), alpha=3,position = position_dodge(width = 0.3))+
      geom_line(aes(x = period, y = value_2), color = "orange",size=1.2)+
      labs(title=" Growth of multilateral loans issued to Rwanda", subtitle="All institutions",x="Period",y="Multilateral loans (Billions of USD)",fontface="Italic")+
 theme(text=element_text(size=10,family="EB Garamond",face="italic"),panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "black"))
    plot_1
    
   # International reserves
    
    int_r<-rdb(ids="WB/JED/Q-Q.1C0.1C0.C.9A.MOA.RXGT.1.ALL.MV.TO1.ALL-RWA")%>%
      select(period, value)%>%
      mutate(value_l=value/1000)
     # plot 
    
    plot_2<-ggplot(int_r)+
      geom_bar(stat="identity",fill="#FFD700",aes(x=period,y=value_l), alpha=3,position = position_dodge(width = 0.5))+
      theme(text=element_text(size=10,family="EB Garamond",face="italic"),panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),axis.line = element_line(colour = "black"))
    plot_2
    # erroneous serie. 
    
    ##########
    