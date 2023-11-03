# Loading relevant packages

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

library(sjmisc)


# EICV 1

      # Demographics 

eicv1_demographics <- read.dta("s1_demographics.dta")
eicv1demoaslabels<-data.frame(attr(eicv1_demographics,"names"),attr(eicv1_demographics,"var.labels"))





      # household variables 
hhc<-as_factor(read_dta("hhvariablesNov11.dta"))

      # Number of residents in EACH Rwandan districts

distpop<-hhc%>%
filter(id2_n=="kirehe")

 # Number of households in each Rwandan district. 

disthh<-hhc%>%
  filter(id2_n=="bugesera")
# dis aggregating households by gender of the head of household

ghh_1<-left_join(eicv1_demographics,hhche)
 
HH1<-ghh_1%>%
  filter(S1Q1=="Male" & S1Q2=="Head")



EICV1_cons <- read_dta("EICV1_cons_aggr_Nov11.dta")
View(EICV1_cons)

eicv1_femaleheadedhousehols<-540667
eicv1_maleheadedhouseholds<- 1144882

# EICV 2 

      # HH Variables 
  eicv2_hhv <-as_factor(read_dta("eng_eicv2_s0_id.dta"))

      # demography

  eicv2_demo <-as_factor(read_dta("eng_eicv2_s1_demo.dta"))



  
    # NUMBER OF HOUSEHOLDS IN EACH DISTRICT 
  eicv2_HHDIS<-eicv2_demo%>%
    group_by(id2_n)%>%
    summarize(total=sum(hh_wt))
 

   # gender of head of households 
eicv2_gend<-eicv2_demo%>%
   group_by(id2_n, s1q1, s1q2)%>%
   summarize(total=sum(hh_wt))%>%
  filter(s1q1=="Male"& s1q2=="Head of household")
  
  
  
  
  
  
# EICV 3
  
  
  eicv_3 <-as_factor(read.dta("S01_hhmembers_S02_education_S03_health_S04_migration.dta"))
  eicv3aslabels<-data.frame(attr(eicv_3,"names"),attr(eicv_3,"var.labels"))
  
  # Educational attainment
  edatt<-eicv_3%>%
    filter(S1Q3Y> 6)%>%
  group_by(S2B1Q11)%>%
  summarize(TOT=sum(HH_WT, na.rm=TRUE))
  options(scipen=999)
      # adding districts in EICV 3  
   
  districts<-read_excel('districts.xlsx')
  districts<-select(districts, subset=-c('total'))
  eicv_3<-left_join(eicv_3, districts)
  eicv_3<-move_columns(eicv_3, districts,.after='DISTRICT')
  
      # Eicv 3 housing 
  
  
  eicv3_housing <- as_factor(read_dta("S05ABCD_housing.dta"))
  
# NUMBER OF HOUSEHOLDS 
  
  hh3<-left_join(eicv3_housing, districts)%>%
    select(-total)%>%
    group_by(district)%>%
    summarize(tot=sum(HH_WT))

# Male and Female-headed households
  
  ghh3<-eicv_3%>%
    filter(S1Q1=="Female"& S1Q2==1)%>%
    group_by(HHID)%>%
    summarize(total=sum(HH_WT, na.rm=TRUE))

  # EICV 4

# loading households and persons data frames
  
  
  eicv4_household <- read.dta("cs_s0_s5_household.dta")
  eicv4hhaslabels<-data.frame(attr(eicv4_household,"names"),attr(eicv4_household,"var.labels"))

  
  eicv4_person <-read.dta("cs_s1_s2_s3_s4_s6a_s6e_s6f_person.dta")
  eicv4peraslabels<-data.frame(attr(eicv4_person,"names"),attr(eicv4_person,"var.labels"))

# residents in each district of Rwanda- EICV 4
  
  dispop4<-eicv4_person%>%
    filter(district=="Kicukiro")
  
    
  alldis<-eicv4_person%>%
  group_by(district)%>%
    summarize(total=sum(weight, na.rm=TRUE))
  
# HOUSEHOLD POPULATION IN EACH DISTRICT 
  
  dishhpop<-eicv4_household%>%
    group_by(district)%>%
    summarize(total=sum(weight, na.rm=TRUE))

  # Gender of head of household
  
  merged_eicv4<-left_join(eicv4_household, eicv4_person)
  
   ghh4<-merged_eicv4%>%
     filter(s1q1=="Female"& s1q2=="Household head (HH)")%>%
     group_by(hhid)%>%
     summarize(total=sum(weight, na.rm=TRUE))
   
# EICV  5
   
   
   EICV5_Household <- read.dta("cs_S0_S5_Household.dta")
   EICV5_Person <-read.dta("cs_S1_S2_S3_S4_S6A_S6E_Person.dta")
   
   eicv5hhaslabels<-data.frame(attr(EICV5_Household,"names"),attr(EICV5_Household,"var.labels"))
   eicv5peraslabels<-data.frame(attr(EICV5_Person,"names"),attr(EICV5_Person,"var.labels"))
     eicvedf<-left_join(EICV5_Household,EICV5_Person)
# NUMBER OF HOUSEHOLDS IN EACH DISTRICT
   
dishh5<- EICV5_Household %>%
  group_by(district)%>%
  summarise(total=sum(weight, na.rm = TRUE))
  
# GENDER OF THE HEAD OF HOUSEHOLD
eicv5gend_hh<-EICV5_Person%>%
  filter(s1q1=="Female"& s1q2=="Household head _HH_")%>%
  group_by(hhid)%>%
  summarise(total=sum(weight, na.rm=TRUE))

   
.........................................
# UPDATING THE SECTION ON EDUCATION

# EICV 1


EICV1_education <-read.sav("s2_education.sav")
eicv1eduaslaba<-data.frame(attr(EICV1_education,"names"),attr(EICV1_education,"var.labels"))

eduachiv<-EICV1_education%>%
  filter(s2aq4=="Certificate (Completed Primary)")

new2<-eicv1_demographics%>%
  group_by(key)%>%
  summarize(hhsize=n())
hhc$key<-format(hhc$key, scientific = FALSE)
new2$key<-format(new2$key, scientific = FALSE)
new3<-left_join(hhc, new2)
write.xlsx(new3, "new.xlsx")
new3<-read.xlsx('new.xlsx')
EICV1_education$key<-format(EICV1_education$key, scientific=FALSE)
new<-left_join(EICV1_education, new3)

# Education attainment- EICV 4

# Finding Male and Female populations aged above 6

gendd<-eicv4_person %>%
  filter(s1q3y>6)%>%
  group_by(s1q1)%>%
  summarise(total=sum(weight))

male_pop_abv6<-4340841
female_pop_abv6<-4857517

# Finding the population aged above 6 by region. 

pop<-eicv4_person %>%
  filter(s1q3y>6)%>%
  group_by(ur2_2012)%>%
  summarise(total=sum(weight))

urbanPo<-1627310
ruralpop<-7571047

ed4_att<-eicv4_person %>%
  filter(s1q3y > 6)%>%
  group_by(s4aq3, s1q1)%>%
  summarise(total=sum(weight))  


ed4_attr<-eicv4_person %>%
  filter(s1q3y > 6)%>%
  group_by(s4aq3,ur2_2012)%>%
  summarise(total=sum(weight)) 

# Educational attainment - EICV 5

ed5_att<-EICV5_Person%>%
filter(s1q3y> 6)%>%
group_by(s4aq3,s1q1)%>%
summarize(total=sum(weight))

gend5<-EICV5_Person %>%
  filter(s1q3y > 6)%>%
  group_by(s1q1)%>%
  summarise(total=sum(weight, na.rm = TRUE))

maleasabv6<-	4564304
femalesabv6<-5028094

pop<-EICV5_Person %>%
  filter(s1q3y > 6)%>%
  group_by(ur)%>%
  summarise(total=sum(weight))

rural<-7794025
urban<-1798374

edatt5u<-EICV5_Person %>%
  filter(s1q3y > 6)%>%
  group_by(s4aq3,ur)%>%
  summarise(total=sum(weight))
rm(list=ls())

  
#  Other Sections of the Data Bank
  
#  Loading Establishment Census 2017
  
EC17 <- as_factor(read_dta("establishments2017.dta"))

ec17_aslabels<-data.frame(attr(EC17,"names"),attr(EC17,"var.labels")) 

#  ESTABLISHMENTS DISAGGREGATED BY GENDER OF THE OWNER AND SIZE

gend_and_siz0<-EC17%>%
  filter(q12=="Yes")%>%
  group_by(q13,q4_2,size2)%>%
  summarize(total=n(),na.rm=TRUE)

gend_and_siz1<-EC17%>%
  group_by(q13,size2)%>%
  summarize(total=n(),na.rm=TRUE)

write.xlsx(gend_and_siz0, "withna.xlsx")

write.xlsx(gend_and_siz1, "withoutna.xlsx")

# Establishments disaggregated by size and institutional sector

inst_sector_size<-EC17%>%
  group_by(size2,q7)%>%
  summarize(total=n())

# Grouping Enterprises by size and turnover 

size_turnov<-EC17%>%
  group_by(q20,size2)%>%
  summarize(total=n())

write.xlsx(size_turnov,"turnov.xlsx")

# Grouping establishments by district 

est_dis<-EC17%>%
  group_by(q1_2)%>%
  summarise(total=n())
write.xlsx(est_dis,"estdis.xlsx")

# Grouping establishments by province
est_prov<-EC17%>%
  group_by(q1_1)%>%
  summarise(total=n())
write.xlsx(est_prov,"estprov.xlsx")

# Grouping Establishments by district and gender of the owner

gend_prov1 <- EC17%>%
  filter(q12=="Yes")%>%
  group_by(q1_1, q4_2)%>%
  summarize(total=n())

gend_prov2<-EC17 %>%
group_by(q1_1,q13)%>%
  summarize(total=n())

write.xlsx(gend_prov1, "manag.xlsx")
write.xlsx(gend_prov2, "owne.xlsx")

# Grouping establishments by gender of the owner,size,and total turnover 
siz_turnov_ownergend1<-EC17%>%
  filter(q12=="Yes")%>%
  group_by(q20,size2,q4_2)%>%
  summarise(total=n())

siz_turnov_ownergend2<-EC17%>%
  group_by(q20,size2,q13)%>%
  summarise(total=n())
....................................................................................................

# Updating the section on financial inclusion 

# Financial Access Strand 

# Finscope gender AND REGIONAL VALUES 

fin_reg<-finscope%>%
  group_by(A3)%>%
  summarise(total=sum(Individuals_Weight))

Fin_gENDER<-finscope%>%
  group_by(B2)%>%
  summarise(total=sum(Individuals_Weight))


Fas<-finscope%>%
  group_by(FAS)%>%
  summarise(total=sum(Individuals_Weight))

Fasd<-finscope %>%
  group_by(FAS,B2,A3)%>%
  summarise(total=sum(Individuals_Weight))

write.xlsx(Fas, "fas.xlsx")




MM_uptake<-finscope%>%
  group_by(Mobile_money_Uptake,B2,A3)%>%
  summarise(total=sum(Individuals_Weight))
 

 # FINDING THE NUMBER OF BORROWERS 

borr<-finscope%>%
  filter(Credit_Strand!= "Not borrowing")%>%
  group_by(B2,A3)%>%
  summarize(total=sum(Individuals_Weight, na.rm = TRUE))


# Finding the number of individuals who have received remittances 

remitt<-finscope%>%
  filter(Remit_Strand!="Not remitting")%>%
  group_by(Remit_Strand, B2,A3)%>%
  summarise(TOT=sum(Individuals_Weight))%>%
  filter(A3=="Rural")

# Finding the number of insured individuals 

ins<-finscope%>%
  filter(Insurance_strand != "Not insured")%>%
  group_by(Insurance_strand, A3, B2)%>%
  summarise(total=sum(Individuals_Weight))

Grouped_finscope<-finscope%>%
  group_by(A3,B2)%>%
  summarize(total=sum(Individuals_Weight))

# Finding the number of individuals who save : Saving strand
 Saving_fs<-finscope%>%
   filter(Saving_strand!="Not saving")%>%
   group_by(A3,B2)%>%
   summarise(total=sum(Individuals_Weight))

#   Finding the number of 