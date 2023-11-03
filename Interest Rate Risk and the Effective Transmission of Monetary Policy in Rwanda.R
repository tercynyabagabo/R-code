#################################################################################################
# BSC Dissertation Script 
# Dissertation Title: Interest Rate Risk and the Transmission of Monetary Policy in Rwanda 
# Created By: Tercy T.Nyabagabo
# November 2022- March 2023
#################################################################################################
# Activity 1: Data Scraping 
#################################################################################################
# DBNOMICS  
# Installing packages 

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

######################################################################################################
# Practicing Data Scraping 
######################################################################################################
# Exploring the database 
vignette("rdbnomics")
# An example on fetching a dataset
Ameco<-rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN")

# World Bank available datasets. 

rdb_datasets(provider_code = "WB")

WB_commodityprices<- rdb_dimensions(provider_code = "WB", dataset_code = "commodity_prices")

# fetching the series codes 
rdb_series(provider_code = "WB", dataset_code = "commodity_prices", simplify = FALSE)

# fetching CRUDE Oil prices from the data frame 
crudeoil<-rdb(ids = "WB/commodity_prices/FCRUDE_PETRO-1W")
# Visualizing crude oil prices 

plot1<-crudeoil%>%
  select(period,value)%>%
  
p1<-ggplot(data=plot1,aes(x=period,y=value))+
  geom_point(color="blue",show.legend = F)+
  geom_line(color="red")
p1
rm(list = ls())
##########################################################################################################
# Activity 2: Scraping and Analyzing IMF IFS DATA 
##########################################################################################################

# Fetching Rwandan Interest rate time series 

deposit_rates<-rdb(ids="IMF/IFS/M.RW.FIDR_PA")
lending_rates<-rdb(ids = "IMF/IFS/M.RW.FILR_PA")
moneymarket_rates<-rdb(ids= "IMF/IFS/M.RW.FIMM_PA")
repo_rates<- rdb(ids = "IMF/IFS/M.RW.FIRA_PA")
discount_rates<- rdb(ids = "IMF/IFS/M.RW.FID_PA")
m3_rwanda<-rdb(ids = "IMF/IFS/Q.RW.FMB_XDC")
#######################################################
# Activity 3: plotting the data 
#######################################################

depos<- deposit_rates %>%
  select(period,value)%>%
  ggplot(aes(x=period,y=value))+
  geom_point(color="blue",show.legend = F)+
  geom_line(color="red")+
  labs(title = "The Evolution of Rwandan Bank Deposit Rates", subtitle = "Monthly Data from 1996-MO1 to 2022-M08"
       ,x="Period", y=" Deposit Rate in percentage")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line())
depos

lend<-lending_rates%>%
select(period,value)%>%
  ggplot(aes(x=period,y=value))+
  geom_point(color="blue",show.legend = F)+
  geom_line(color="red")+
  labs(title = "The Evolution of Rwandan Bank Lending Rates", subtitle = "Monthly Data from 1996-MO1 to 2022-M08"
       ,x="Period", y=" Lending Rate in percentage")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line())
lend

#######################################################################
# Activity 4: Exporting Lending and deposit rates data to a spreadsheet
#######################################################################
lendr<-lending_rates%>%
  select(period,value)%>%
  write.xlsx("lr.xlsx")

IR_DATA<-deposit_rates %>%
  select(period,value)%>%
  write.xlsx("IR1.xlsx")
######################################################################
# Activity 5: The plots 
#####################################################################

install.packages("hexView")
library(hexView)

library(haven)
IRR_data <- read_dta("the data in Stata.dta")
View(the_data_in_Stata)



IRR_data<-IRR_data[-(201:238),]  

 plot1<-ggplot(data=IRR_data)+
   geom_bar(stat="identity",fill="#008080",aes(x=month, y= lendspread), alpha=3,position = position_dodge(width = 0.3))+
   geom_line(aes(x=month, y= lendspread), color = "orange",size=1.2)+
      labs(title="Lending Spread Evolution",subtitle = "Lending Rates minus Interbank Rate",y="Lending spread (in %)")+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line())
 plot1
 
 # Correlation graphs
 
plot2<-ggplot(data = IRR_data, aes(x=vardepos, y=difflendspread))+
  geom_point(color="blue")+
   labs( x= "Variance(Average Deposit Rates)",
       y=" Differenced Lending Spread (in %)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(), panel.border = element_rect(fill=NA))
plot2

plot3<-ggplot(data = IRR_data,aes(x=varlend, y=difflendspread))+
  geom_point(color="dark green")+
  labs( x= "Variance(Average Lending Rates)",
        y="Differenced Lending Spread (in %)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(), panel.border = element_rect(fill=NA))
plot3

# Their quarter-lagged counterparts

plot4<-ggplot(data=IRR_data, aes(x=tlagdep,y=difflendspread))+
  geom_point(color="orange")+
  labs(x="Quarter-lagged variance (Average Deposit Rates)", y="Differenced Lending Spread (in %)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(), panel.border = element_rect(fill=NA))
plot4


plot5<-ggplot(data=IRR_data, aes(x=tlaglend,y=spread))+
  geom_point(color="black")+
  labs(x="Quarter-lagged variance (Average Lending Rates)", y="Differenced Lending Spread (in %)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(), panel.border = element_rect(fill=NA))
plot5

########################################################################################
# Activity 6: A data frame of all the key variables- For the Appendix 
#######################################################################################
library(haven)
the_data_in_Stata <- read_dta("the data in Stata.dta")
View(the_data_in_Stata)

my_data<-the_data_in_Stata%>%
  select(month,depos,lend,vardepos,varlend,ibr,lendspread,difflendspread,
         sddepos,sdlend,qlsddepos,qlsdlend,tlagdep,tlaglend)

#############################
# Extract the data as a table
#############################
write.table(my_data, file = "variables.txt", sep = ",", quote = FALSE, row.names = F)
