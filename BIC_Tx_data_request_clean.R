#Create datasets based on following critera: 
# 1. Visits that occurred since Jan 1, 2020 to current day for patients who were RX'ed with bicillin at least twice during that period and whose most recent address on file (based on visdatetime) contains a PHL zip code
# 2. Demographic data for individual patients from the above visits table 
# 3. Summary/figures of demographic info of BIC treated patients 

#Packages 
library(tidyverse) #data manipulation
library(kableExtra) #formatting tables
library(knitr) #formatting tables
library(lubridate) #date/time variable manipulation
library(haven) #used to read in SAS data files 
library(forcats) #used to recode factor levels
library(DBI) #database interface package 
library(odbc) #provides a DBI-compliant interface to odbc driver (used in combo with DBI package) 

#Create connection to EMR database (dummy server address and name)
con <- DBI::dbConnect(odbc::odbc(),

                      Driver = "ODBC Driver 17 for SQL Server",

                      Encrypt = "yes",

                      Trusted_Connection = "yes",

                      TrustServerCertificate = "yes",
                      
                      Server = "111.222.333.44",

                      Database ="EMR")


#Query to pull in relevant info from the clinic visit table, treatment table, medical record table, demographics, etc. and do preliminary filtering of data based on BIC Tx, visit date, and PHL ZIP as per inclusion criteria
sql1 <-"SELECT A.patient_id, A.visit_datetime,A.visit_id,A.ds_clinic_visit_id,
B.ds_clinic_visit_reason_code_id, C.ds_clinic_visit_reason_desc,
D.treatment_comments,D.treatment_start_date,D.treatment_description,
E.ds_medical_record_id,G.city_code_id,G.zip_code,G.census_tract_code_id,G.audit_create_dttm,
G.audit_update_dttm, H.race_code_id,I.dob,I.ethnicity_code_id,
I.gender_code, J.race_code,J.race_desc,K.identified_gender_code_id
FROM ds_clinic_visits as A
LEFT JOIN ds_clinic_visit_reasons as B on A.ds_clinic_visit_id=B.ds_clinic_visit_id
LEFT JOIN ds_clinic_visit_reason_codes as C on B.ds_clinic_visit_reason_code_id = C.ds_clinic_visit_reason_code_id
LEFT JOIN ds_patient_treatments as D on A.ds_clinic_visit_id = D.ds_clinic_visit_id
LEFT JOIN ds_medical_records as E on A.ds_clinic_visit_id = E.ds_clinic_visit_id
LEFT JOIN patient_gis_address as G on A.patient_id = G.patient_id
LEFT JOIN patient_races as H on A.patient_id=H.patient_id
LEFT JOIN patients_vw as I on A.patient_id=I.patient_id
LEFT JOIN race_codes as J on H.race_code_id = J.race_code_id
LEFT JOIN patient_ds as K on A.patient_id = K.patient_id
WHERE year(A.visit_datetime) >= 2020 and
D.treatment_description LIKE '%BICILLIN%'
and G.zip_code LIKE '191%' and
visit_id LIKE 'C%"


#Execute above query and store result in an R dataframe called vis_plus 
vis_plus <-DBI::dbGetQuery(con, sql1)


#disconnect from SQL server after running query
dbDisconnect(con)

#Determine appropriate zip code and remove patients who were not Rx'ed with BIC at least twice
#Resulting table will have one row per visit ID and select visit/tx variables
Visit_Level_Data <-  vis_plus %>%
  
  #Calcultae date difference between when address was updated and visit date in days
  mutate(DateDiff = abs(difftime(ymd(as.Date(visit_datetime)),
                             ymd(as.Date(audit_update_dttm)),
                             units = "days"))) %>% 
  
  group_by(visit_id) %>% 
  #Recent address == address with audit update date closest in time to visit
  mutate(Recent_Address = min(DateDiff)) %>% 
  #Arrange (default ascending) recent address within group
  arrange(Recent_Address,.by_group = TRUE) %>% 
  #Retain first row within group (correct zip code after arranging)
  slice(1) %>% 
  ungroup() %>% 
  
  #Keep relevant columns, drop the rest
  select(patient_id,visit_id,ds_clinic_visit_id,visit_datetime,zip_code,treatment_description,ds_clinic_visit_reason_desc,treatment_comments) %>%
  
  #Retain patients who were Rx'ed with BIC at least twice at different visits
  group_by(patient_id) %>% 
  filter(n()>=2)

#Bic prescription summary stats by patient - mean, median, min, max 
bic_summ <- Visit_Level_Data %>% 
  group_by(patient_id) %>% 
  summarise(Total_Rx = n()) %>% 
  ungroup() %>% 
  summarise(Avg_Bic_Rx = mean(Total_Rx),
            Med_Bic_Rx = median(Total_Rx),
            Min_Bic_Rx = min(Total_Rx),
            Max_Bic_Rx = max(Total_Rx))

#Bic Rx by year table 
bic_by_yr <- Visit_Level_Data %>% 
  mutate(year = as.factor(year(visit_datetime))) %>% 
  group_by(year) %>% 
  summarise(Total_Rx_by_Yr = n()) %>% 
  ungroup()

#Barplot depicting Bic Rx Totals by year since Jan 1, 2020, excluding 2024 since we don't have a full year of data yet
Bic_By_Yr_Fig <- ggplot(
  
  #get rid of '24 data since we're not plotting that
  bic_by_yr %>% filter(year!=2024), 
  
  #x axis = Year, y axis = Total Rx's for each year, color bars based on year
       aes(x = year, y = Total_Rx_by_Yr,fill = year))+
 
  #bar height == Total provided
   geom_bar(stat = "identity")+
  
  #Clean up x,y labels
  labs(x = "Year",
       y = "Total Bicillin Prescriptions")+
 
   #remove legend since it's redundant w/ x axis
  theme(legend.position = "none")

  
#Demographics table contains one row per patient with select demographic variables
Demographics <- vis_plus %>% 
  
  #Drop some columns
  select(patient_id,visit_id,visit_datetime,ds_medical_record_id,zip_code,race_code_id,dob,ethnicity_code_id,race_code,race_desc,gender_code,identified_gender_code_id) %>% 
  
  #Retain patients in visit table only
  filter(patient_id %in% unlist(unique(Visit_Level_Data$patient_id))) %>% 
  
  group_by(patient_id) %>% 
  #Create explicit race/ethnicity columns
  mutate(white = ifelse(any(race_code==10),"Y",NA_character_),
         black = ifelse(any(race_code==20),"Y",NA_character_),
         aian= ifelse(any(race_code==30),"Y",NA_character_),
         asian = ifelse(any(race_code==40),"Y",NA_character_),
         NHOPI = ifelse(any(race_code==50),"Y",NA_character_),
         UNKNOWN = ifelse(any(race_code==99 | race_code==25),"Y",NA_character_),
         hispyn = case_when(any(ethnicity_code_id %in% c(9,12,13,14,15,18,19)) ~ 1,
                            TRUE ~0),
         #create general race/eth column based on above columns for study purposes
         RaceEth = case_when(hispyn == 1 ~ "Hispanic",
                             black == "Y" & is.na(white) & is.na(aian) &
                               is.na(asian) & is.na(NHOPI)  & hispyn !=1 ~"Black Not Hispanic",
                             white == "Y" & is.na(black) & is.na(aian) &
                               is.na(asian) & is.na(NHOPI)  & hispyn !=1 ~"White Not Hispanic",
                             TRUE ~"Other")) %>%
  
  #Calculate age at most recent vis/most recent BIC Rx and create age group variable
  arrange(desc(visit_datetime),.by_group = TRUE) %>%
  slice(1) %>% 
  mutate(Age_recent_vis = abs(interval((as.Date(visit_datetime)),
                                       as.Date(dob)) %/% years(1)),
         AgeGrp = case_when(Age_recent_vis < 15 ~ 1,
                             Age_recent_vis >=15 & Age_recent_vis <20 ~ 2,
                             Age_recent_vis >=20 & Age_recent_vis < 30 ~ 3,
                             Age_recent_vis >= 30 & Age_recent_vis < 66 ~ 4,
                             Age_recent_vis >65 ~ 5,
                            TRUE ~ NA_real_)) %>% 
  ungroup()
  
  
# #Check to make sure patients in demog table == # of patients in vis level table
# setdiff(unique(Demographics$patient_id),unique(Visit_Level_Data$patient_id))
# sum(duplicated(Demographics$patient_id))


#Create a boxplot depicting distribution of ages of patients prescribed Bic
age_fig <- ggplot(
  #no x axis (boxplot), y = Age of patients at recent visit
  Demographics,aes(x = factor(0),y = Age_recent_vis))+
  #Color outliers red
  geom_boxplot(outlier.color = "red")+
  #Remove all x axis labels/tick marks
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Clean up Y title
  ylab("Age of Patient At Most Recent Visit")+
  #Compute 5 num summary and annotate boxplot with these numbers
  stat_summary(geom="text", 
               fun=quantile,
               aes(label=sprintf("%1.1f", after_stat(y))),
               position=position_nudge(x=0.5), size=3.5)


#Create a barplot depicting distribution of race/ethnicity of people Rx'ed with Bic
Race_Eth_Fig <- ggplot(
  #x axis = Race/Eth variable, fill color based on Race/Eth var
  Demographics,aes(x = RaceEth,fill=RaceEth))+
  #stat = "count" (default, R aggregates the data for me)
  #set width of bars
  geom_bar(width = .6)+
  #Angle x axis labels so they don't overlap
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))+
  #Manually set y axis limits so plot looks clean and text is readable
  ylim(c(0,400))+
  #color palette used for bar colors
  scale_fill_brewer(palette = "Spectral")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  labs(x = "Race/Ethnicity",
       y = "Total")+
  #remove legend since it's redundant w/ x axis
  theme(legend.position = "none")




