# Practicing ggplots. 
library(haven)
library(dplyr)
library(magrittr)
library(readr)
library(foreign)
library(rlang)
library(readxl)
library(openxlsx)
library(readstata13)
library(tidyr)
library(ggplot2)
library(data.table)
library(officer)
library(spatstat)
library(corrplot)
library(sjmisc)

# dbnomics 
vignette("rdbnomics")

# loading dataframe for paper .
library(haven)
IDCP_DATA <- read_dta("ALL STATA VARIABLES SO FAR.dta")
View(ALL_STATA_VARIABLES_SO_FAR)
  select(-"_est_estimation1",-"_est_estimation2")

# Data visualization

  # Inverted J-curve : Rwandan Reer vs Rwandan exports. 
  
graph<-ggplot(data =IDCP_DATA,aes(x=lnreerrw,y=lnrwrealexports,color=quarter))+
geom_abline()+
geom_smooth()+
labs(title="Rwandan Reer Vs Rwandan Exports",subtitle="Inverted J-curve (2006:1 to 2020:4)",x="Rwandan Reer", y="Rwandan real exports")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line())
          graph
          
# J-curve : US Reer vs Rwandan exports.           
graph<-ggplot(data =IDCP_DATA,aes(x=lnreerrw,y=lnrwrealexports))+
geom_abline()+
geom_smooth()+
labs(title="US Reer Vs Rwandan Exports",subtitle="J-curve (2006:1 to 2020:4)",x="Rwandan Reer", y="Rwandan real exports")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line())
          graph
   bar<-ggplot(idcp_data, aes(quarter))                
       bar
  # plot of EMDES REER vs US REER and terms of trade, supported by correlations. 
       
       # importing global export merchandise volumes 
       
       merch<-rdb(ids = "WTO/ITS_MTP_QXV/000.TO.000.Q")
       # importing US real effective exchange rate 
        
       # check in excel before plotting 
       
      dta<-merch %>%
        select(period,value)
       
      write.xlsx(dta,"dta_.xlsx")
#      re-importing the data 
      
      library(readxl)
      dta_ <- read_excel("C:/Users/hp1030/Desktop/Research/Papers/In-progress/Investigating the dominant currency paradigm on Rwandan Terms of Trade/R/dta_.xlsx")
      View(dta_)
      
      # Load the necessary library
      library(ggplot2)
      loadfonts(device = "win")
      font <- "Arial"
      
      # Split the "period" variable into separate year and quarter columns
      dta_$year <- substr(dta_$period, 1, 4)
      dta_$quarter <- substr(dta_$period, 6, 6)
      
      # Create a new variable "date" by combining year and quarter
      dta_$date <- as.Date(paste0(dta_$year, "-", dta_$quarter, "-01"))
      
      # Create the plot with dual y-axes and a legend at the bottom
      plot_1 <- ggplot(dta_) +
        geom_line(aes(x = date, y = us_reer, group = 1, color = "U.S. REER Index"), linewidth = 1) +
        geom_line(aes(x = date, y = merch_exp, group = 1, color = "World Merchandise Exports Index"), linewidth = 1) +
        scale_y_continuous(
          sec.axis = sec_axis(~ ., name = "World Merchandise Exports Index")
        ) +
        labs(y = "U.S. REER Index", color = "Legend:") +
        theme_bw() +
        theme(
          panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_text("Year", face = "italic"),
          axis.title.y.left = element_text("U.S. REER Index", face = "italic"),
          axis.title.y.right = element_text("World Merchandise Exports Index", face = "italic"),
          axis.line = element_line(color = "black")
        ) +
        scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
        scale_color_manual(
          values = c("U.S. REER Index" = "blue", "World Merchandise Exports Index" = "red"),
          labels = c("U.S. REER Index", "World Merchandise Exports Index")
        ) 
      
      # Print the plot
      print(plot_1)
      

# Correlation Analysis 
      install.packages("corrplot")
      
      
      # Example: Creating a synthetic time series dataset
      set.seed(123)  # for reproducibility
      n <- 100  # Number of data points
      date <- seq(as.Date("2022-01-01"), by = "days", length.out = n)
      variable1 <- rnorm(n)
      variable2 <- rnorm(n)
      variable3 <- rnorm(n)
      
      # Create a data frame
      time_series_data <- data.frame(Date = date, Variable1 = variable1, Variable2 = variable2, Variable3 = variable3)
      # Calculate the correlation matrix
      cor_matrix <- cor(time_series_data[, -1])  # Exclude the Date column
      # Create the correlation heatmap
      corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
               tl.col = "black", tl.srt = 45, tl.pos = "lt")
      
      # Add a legend
      legend("bottomleft", legend = c("Low Correlation", "High Correlation"), 
             fill = colorRampPalette(c("blue", "red"))(5), title = "Correlation")
      
      # Extract correlation coefficient between Variable1 and Variable2
      cor_coefficient <- cor_matrix[1, 2]  # Replace 1 and 2 with the corresponding column indices
      
      # Save the plot as an image (e.g., PNG)
      ggsave("correlation_heatmap.png", width = 10, height = 8)
      
     # loading my data 
      library(haven)
      idcp_data <- read_dta("ALL STATA VARIABLES SO FAR.dta")
      View(idcp_data)
# ensuring the first column is a date variable 
     idcp_data<-idcp_data%>%
       mutate(quarter=as.Date(paste0(substr(quarter, 1, 4), "-", substr(quarter, 6, 6), "-01")))
     # Creating a sub-data frame 
      df1<-idcp_data%>%
       select(reerrw,reerus,rwrealexports)
      # Removing NA's 
      df1<-df1[-(61:64),]
     # The correlations 
   cor_mat<-cor(df1)
   # The cor plot heat map
   corrplot(cor_mat,
     type = "upper",    # Display upper triangular
     order = "hclust",  # Order by hierarchical clustering
     tl.col = "white",  # Label color
     tl.srt = 45,       # Label rotation angle
     tl.pos = "lt",     # Label position
     addCoef.col = "dark red",  # Coefficient text color
     number.cex = 1,  # Coefficient text size
     mar = c(1, 1, 1, 1),  # Margin
     diag = TRUE,      # Exclude diagonal
     insig = "label_sig",  # Show significant level as text
     sig.level = 0.05,  # Set significance level
     p.mat = NULL       # Not displaying p-values
   )
  
   # Load necessary libraries
   library(dplyr)
   library(ggplot2)
   
   