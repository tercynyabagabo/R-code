########################################

#  commodity price shocks and Pass-through  brief 

##########################################
 # packages 


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

# Visualizing the evolution of select global commodity prices

library(readxl)
data_<- read_excel("Global comodity prices Data/CMO-Historical-Data-Monthly (1).xlsx", 
                                             sheet = "data_for_plot", col_types = c("text", 
        view(data_)                                                                            "numeric", "numeric", "numeric"))

# data cleaning 

library(readxl)
data <- read_excel("data.xlsx", col_types = c("date", 
                                              "numeric", "numeric", "numeric"))
View(data)

# Visualization 

plot_1 <- ggplot(data) +
  geom_line(aes(x = as.Date(Period), y = food_index, group = 1), color = "green") +
  geom_ribbon(aes(x = as.Date(Period), ymin = food_index, ymax = food_index), fill = "green", alpha = 1.2) +
  geom_line(aes(x = as.Date(Period), y = energy_index, group = 1), color = "purple") +
  geom_ribbon(aes(x = as.Date(Period), ymin = energy_index, ymax = energy_index), fill = "purple", alpha = 1.2) +
  geom_line(aes(x = as.Date(Period), y = fertilizer_index, group = 1), color = "orange") +
  geom_ribbon(aes(x = as.Date(Period), ymin = fertilizer_index, ymax = fertilizer_index), fill = "orange", alpha = 1.2) +
  labs(x = "Period", y = "Index",
       color = "Legend Title") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_color_manual(values = c("green" = "green", "purple" = "purple", "orange" = "orange"),
                     labels = c("green" = "Fertilizer Index", "purple" = "Energy Index", "orange" = "Food Index")) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
        panel.grid.minor = element_blank(),panel.border = element_blank(), legend.position = "bottom")

plot_1



