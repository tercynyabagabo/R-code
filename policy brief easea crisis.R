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

# plots 

# credit 

library(readxl)
cred <- read_excel("data/cred.xlsx", col_types = c("date", 
                                                   "numeric", "numeric", "numeric", "numeric"))
View(cred)


library(ggplot2)

library(ggplot2)

plot_1 <- ggplot(cred, aes(x=Period)) +
  geom_line(aes(y=Thailand, color="Thailand"), size=1, alpha=1) +
  geom_line(aes(y=Malasia, color="Malaysia"), size=1, alpha=1) +
  geom_line(aes(y=Indonesia, color="Indonesia"), size=1, alpha=1) +
  geom_line(aes(y=Korea, color="Korea"), size=1, alpha=1) +
  theme(text=element_text(family="Lato", size=10, face="bold"),
        axis.line=element_line(colour="black"),
        panel.background=element_blank(),
        legend.background=element_blank()) +
  labs(y="Expressed as a % of GDP", color=NULL) +
  scale_color_manual(values=c("black", "orange", "dark green", "#8B0000"),
                     labels=c("Thailand", "Malaysia", "Indonesia", "Korea"))

plot_1

# xrates 

library(readxl)
xrate <- read_excel("data/xrate.xlsx")
View(xrate)

plot_2 <- ggplot(xrate) +
  geom_line(aes(x=period, y=amount, color=country), size=1, alpha=0.8) +
  theme(text=element_text(size=10, family="Calibri", face="bold.italic"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  facet_wrap(~country, scales = "free_y") +
  theme(strip.background = element_blank()) +
  labs(y="US dollar per currency", color= "Country",x=NULL) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0001))+
  scale_color_manual(values = c("dark blue", "dark red", "dark green", "#000000"))


plot_2

# Asian recoveries 

# loading the data 

library(readxl)
recov <- read_excel("recov.xlsx")
View(recov)

plot_3 <- ggplot(recov) +
  geom_line(aes(x=period, y=value, color=country), size=1, alpha=1.2, show.legend = FALSE) +
  theme(text=element_text(size=10, family="Calibri", face="bold.italic"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  facet_wrap(~country, scales = "free_y") +
  theme(strip.background = element_blank()) +
  labs(y="GDP growth rate (%)", x=NULL) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  scale_color_manual(values = c("#b37e00","#b37e00","#b37e00","#b37e00","#b37e00")) +
  geom_text(aes(x = period, y = value, label = ifelse(period %in% c(1998,2020), paste0(sprintf("%.1f", value), "%"), "")),
            size = 2.7, color = "black",fontface="bold.italic", family = "Arial",hjust=-0.0)+
  annotate("rect", xmin = 1996.5, xmax = 1998.5, ymin = -Inf, ymax = Inf,
           fill = "#8c8c8c", alpha = 0.3, linetype = 0)+
  annotate("rect", xmin = 2019.5, xmax = 2021, ymin = -Inf, ymax = Inf,
           fill = "#8c8c8c", alpha = 0.3, linetype = 3)
plot_3

# total reserves excludin gold 

res<-rdb(ids="IMF/IFS/Q.RW.RAFA_MV_USD")

view(res)
res<-res%>%
  select(period,value)

plot_4<-ggplot(res)+
  geom_line(aes(x=period,y=value),color="#b37e00")+
  theme(text=element_text(size=10, family="Calibri", face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  labs(y=" Total Foreign Currency Reserves ( million USD)",x="Period")
plot_4
