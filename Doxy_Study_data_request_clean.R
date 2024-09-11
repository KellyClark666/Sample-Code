#This is an older data request which showcases a different (and less efficient) way of pulling data into R from our EMR using a different function to run individual SQL queries for each table of interest but it shows off more joins/data manipulation in R once you get past all the queries

# 1. Create a table of all visit level data for people within our population of interest
      #Inclusion: Age >=18 , MSM/MSMW, has been Rx'ed  with PrEP at least 2x during study period
      #Create variable indicating treatment group (PrEP use only vs DoxyPEP/PrEP use)
# 2. Create a demographics table for each patient that meets inclusion criteria
# 3. Create a labs table with all CT & GC tests performed within study period for each patient that meets inclusion criteria

#Packages 
library(tidyverse) #for everything
library(kableExtra) #formatting tables
library(knitr) #formatting tables
library(lubridate) #deal with date/time variables
library(haven) #read in sas files 
library(pdftools) #work with pdfs
library(forcats) #use to recode factor levels
library(sqldf) #use to run sql in R (idk how to do that yet)
library(RODBC) #used to connect to SQL server via ADB link
library(ggpubr) #figures
library(ggvenn) #venn diagrams

#connect to EMR SQL Server via Access Data Base link
conn <-  odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=Path\\To\\EMR.accdb",rows_at_time = 1)


#Query to pull in visits
query <- paste('SELECT * ',
               'FROM dbo_ds_clinic_visits',
               # paste(("WHERE visit_datetime >= #2019-01-01# "),sep=''),
               sep='\n')

#Query to pull in treatment
query2 <- paste('SELECT * ',
                'FROM dbo_ds_patient_treatments',
                #paste(("WHERE treatment_start_date >= #2019-01-01# "),sep=''),
                sep='\n')


#Query tp pull in clinic visit reasons
query3 <- paste('SELECT * ',
                'FROM dbo_ds_clinic_visit_reasons',
                sep='\n')

#Query tp pull in clinic visit reason codes
query4 <- paste('SELECT * ',
                'FROM dbo_ds_clinic_visit_reason_codes',
                sep='\n')

#More queries
query5 <-paste('SELECT * ',
               'FROM dbo_ds_med_rec_prep',
               sep='\n')

query6 <-paste('SELECT * ',
               'FROM dbo_ds_medical_records',
               sep='\n')

query7 <-paste('SELECT * ',
               'FROM dbo_ds_patient_lab_tests',
               sep='\n')

query8 <-paste('SELECT * ',
               'FROM dbo_ds_lab_test_result_codes',
               sep='\n')

query9 <-paste('SELECT * ',
               'FROM dbo_ds_lab_test_type_codes',
               sep='\n')

query10  <-paste('SELECT * ',
                 'FROM dbo_ds_patient_field_records',
                 sep='\n')

query11  <-paste('SELECT * ',
                 'FROM dbo_ds_patient_fr_diseases',
                 sep='\n')

query12  <-paste('SELECT * ',
                 'FROM dbo_patient_races',
                 sep='\n')


query13  <-paste('SELECT * ',
                 'FROM dbo_race_codes',
                 sep='\n')

query14  <-paste('SELECT * ',
                 'FROM dbo_patients_vw',
                 sep='\n')

query15  <-paste('SELECT * ',
                 'FROM dbo_patient_ds',
                 sep='\n')

query16 <- paste('SELECT * ',
                 'FROM dbo_ds_med_rec_sexual_history',
                 sep='\n')

query17 <- paste('SELECT * ',
                 'FROM dbo_ds_lab_test_specimen_source_codes',
                 sep='\n')

query18 <- paste('SELECT patient_id, zip_code, city_code_id',
                 'FROM dbo_patient_gis_address',
                 sep='\n')


#Run all the queries and store data as individual R dataframes
visits <- sqlQuery(conn,query)
reasons <-  sqlQuery(conn,query3)
reason_codes <-   sqlQuery(conn,query4)
prep_mod <-  sqlQuery(conn, query5)
med_rec <-  sqlQuery(conn, query6)
labs <-  sqlQuery(conn, query7)
lab_result_code <-  sqlQuery(conn, query8)
lab_type_code <-  sqlQuery(conn, query9)
tx <-  sqlQuery(conn,query2)
pat_race <-  sqlQuery(conn,query12)
race_codes <-  sqlQuery(conn,query13)
pat_vw <-  sqlQuery(conn,query14)
pat_ds <-  sqlQuery(conn,query15)
pat_sex_hist <-  sqlQuery(conn,query16)
pat_sex_hist <-  sqlQuery(conn,query16)
spec_src <- sqlQuery(conn,query17)
pat_gis <- sqlQuery(conn,query18)

#Close connection
odbcClose(conn)

######################################

vis_plus <-  
  #visits is my base table
  visits %>%
  #join a bunch of tables to visits
  left_join(reasons,by = "ds_clinic_visit_id") %>%
  left_join(reason_codes,by = "ds_clinic_visit_reason_code_id") %>%
  left_join(tx,by = "ds_clinic_visit_id")%>%
  left_join(med_rec,by = "ds_clinic_visit_id")%>%
  left_join(prep_mod,by = "ds_medical_record_id")%>%
  left_join(pat_sex_hist %>% 
              select(ds_medical_record_id,male_partners,female_partners),
            by="ds_medical_record_id") %>% 
  left_join(pat_vw %>% select(patient_id,gender_code),by = "patient_id") %>% 
  
  #Keep these columns, and drop everything else
  select(patient_id, visit_datetime,visit_id,gender_code,comments,treatment_comments,treatment_start_date,treatment_description,
         ds_medical_record_id, ds_clinic_visit_id,ds_clinic_visit_reason_code,prescribing_institution_id,
         doxy_pep_offered,
         doxy_pep_last_visit, doxy_pep_doses_taken,
         doxy_pep_doses_remaining,male_partners, female_partners) 


#Vis_study contains all visits for any person who had at least two visits where PrEP was prescribed and indicates treatment category with Tx_Cat variable
Vis_study <-  vis_plus %>%
  
  #filter on start of study period and removed deleted visit ids
  filter(visit_datetime >= as.Date("2015-01-01") & !str_detect(visit_id, "^D")) %>% 

  
  #Combine all tx's for a given visit into one multi-treatment var
  group_by(patient_id,ds_clinic_visit_id) %>%
  #sort alphanumerically 
  arrange(treatment_description) %>% 
  #concat all treatments for a given visit, separated by ~
  mutate(mult_treatments = paste0(treatment_description, collapse = " ~ ")) %>% 
  ungroup() %>% 
 
   #flag doxypep, nPEP, and PrEP visits. 1(yes, given) 0(no, not given) 
  mutate(
    #Create binary doxy flag
    DOXYPEP=ifelse((treatment_description) != "48 - DoxyPEP" |
                          is.na(treatment_description),0,1),
     
    #Create binary nPEP flag (combo of Isentress & Truvada is nPEP)
     nPEP = ifelse(grepl('*33 -.*44 -',mult_treatments),1,0),
    
    #Create binary PrEP flag (if truvada or descoy given AND npep flag not 1 then PrEP = 1)
     PrEP = ifelse(grepl('*33 -|*50 -',mult_treatments) & nPEP!=1,1,0)) %>% 
  
  group_by(patient_id) %>%
  #Retain patients who were given PrEP or DoxyPEP ever
  filter(any(PrEP==1)|any(DOXYPEP==1)) %>% 
  
  #Create treatment category varibale where 1 = PrEP Only, 2 = DoxyPEP + PrEP, 3 = Doxy only
  mutate(Tx_Cat = case_when(sum(PrEP)>0 & sum(DOXYPEP)>0 ~ 2,
                            sum(PrEP)>0 & sum(DOXYPEP)==0 ~ 1,
                            sum(PrEP)==0 & sum(DOXYPEP)>0 ~ 3,
                          TRUE ~ NA_real_)) %>% 
  
  #Create first PrEP visit variable
  #sort desc PrEP var, but ascending visit date
  arrange(desc(PrEP),visit_datetime) %>% 
  mutate(first_prep_vis = as.Date(visit_datetime[1])) %>% 
  
  #Create first DoxyPEP visit variable
  #sort desc DoxyPEP var, but ascending visit date
  arrange(desc(DOXYPEP),visit_datetime) %>% 

  mutate(
    #Convert date to chr string (ifelse doesn't handle DT variables properly)
    visit_datetime2 = as.character(visit_datetime), 
    
    #If Tx_Cat 1 (never Rx'ed doxy), first doxy date = NA, else first doxy date = first visit date
    first_doxypep_vis = as.Date(ifelse(Tx_Cat==1,
                                            NA_Date_,
                                            as.character(visit_datetime2[1])))) %>% 

  #Keep one row per person per unique visit
  distinct(patient_id,visit_id,.keep_all = TRUE) %>% 
  group_by(patient_id) %>%
  
  #Retain only patients who had at least two PrEP visits
  filter(sum(PrEP)>1) %>%
  ungroup %>%
  
  #Drop a bunch of columns
  select(patient_id,ds_medical_record_id,Tx_Cat,visit_datetime,visit_id,ds_clinic_visit_id,doxy_pep_offered,
         doxy_pep_last_visit, doxy_pep_doses_taken,
         doxy_pep_doses_remaining,DOXYPEP,PrEP,first_prep_vis,
         first_doxypep_vis,last_doxypep_vis,male_partners, female_partners,treatment_comments,comments) %>%
  
  #Calculate time from first to last clinic visit in days
  group_by(patient_id) %>% 
  mutate(Clinic_duration_days = abs((interval(ymd(max(as.Date(visit_datetime))),
                                              ymd(min(as.Date(visit_datetime)))) %/% days(1)))) %>% 
  ungroup() %>% 
  
  mutate(
    #create a visit year column from visit date
    year = year(visit_datetime),
    
        #create a period variable. Usage will be determined by Rx's within 1 year periods, starting with a patient's         first doxypep visit date 
         period = case_when(visit_datetime > first_doxypep_vis & 
                              visit_datetime <= (ymd(first_doxypep_vis) +years(1)) ~ 1,
                            visit_datetime > (ymd(first_doxypep_vis) +years(1)) & 
                              visit_datetime <= (ymd(first_doxypep_vis) +years(2)) ~ 2,
                            visit_datetime > (ymd(first_doxypep_vis) +years(2)) & 
                              visit_datetime <= (ymd(first_doxypep_vis) +years(3))~3,
                            visit_datetime > (ymd(first_doxypep_vis) +years(3)) & 
                              visit_datetime <= (ymd(first_doxypep_vis) +years(4))~4,
                            visit_datetime > (ymd(first_doxypep_vis) +years(4)) & 
                              visit_datetime <= (ymd(first_doxypep_vis) +years(5))~5,
                            visit_datetime > (ymd(first_doxypep_vis) +years(5)) & 
                              visit_datetime <= (ymd(first_doxypep_vis) +years(6))~6,
                            TRUE ~ NA_real_)) %>% 
  group_by(patient_id,period) %>%
  
  #Determine usage based on Rx'es within period
  mutate(usage = case_when(Tx_Cat==2 & sum(DOXYPEP)>3 & !is.na(period)~ "High",
                           Tx_Cat ==2 & sum(DOXYPEP)>1 & sum(DOXYPEP) <=3 & !is.na(period) ~ "Intermediate",
                           Tx_Cat ==2 & sum(DOXYPEP)==1 & !is.na(period) ~ "Low",
                           (Tx_Cat ==2 & sum(DOXYPEP)==0 & !is.na(period))~ "None",
                           TRUE ~ NA_character_)) %>% 
  ungroup()


#Distinct patient ids for all prep and doxypep/prep patients
All_patIDs <- Vis_study %>%
  select(patient_id) %>%
  distinct()


Demographics_Final <-  
  #Join a bunch of demographic info to patient ids of interest
  All_patIDs %>%
  left_join(pat_race %>% select(patient_id,race_code_id),by = "patient_id") %>% 
  left_join(pat_vw %>% select(patient_id,dob,ethnicity_code_id,gender_code),by = "patient_id") %>% 
  left_join(race_codes %>% select(race_code_id,race_code,race_desc), by = "race_code_id") %>% 
  left_join(pat_ds %>% select(patient_id,identified_gender_code_id),by = "patient_id") %>% 
  left_join(med_rec %>% select(patient_id,ds_medical_record_id), by ="patient_id") %>% 
  left_join(pat_sex_hist %>% select(ds_medical_record_id,sexual_practice, sexuality,male_partners,female_partners),
            by="ds_medical_record_id") %>% 
  
  #Determine patient race/ethnicity
  group_by(patient_id) %>% 
  mutate(white = ifelse(any(race_code==10),"Y",NA_character_),
         black = ifelse(any(race_code==20),"Y",NA_character_),
         aian= ifelse(any(race_code==30),"Y",NA_character_),
         asian = ifelse(any(race_code==40),"Y",NA_character_),
         NHOPI = ifelse(any(race_code==50),"Y",NA_character_),
         UNKNOWN = ifelse(any(race_code==99 | race_code==25),"Y",NA_character_),
         hispyn = case_when(any(ethnicity_code_id %in% c(9,12,13,14,15,18,19)) ~ 1,
                            TRUE ~0)) %>% 
  
  group_by(patient_id) %>%
  
  mutate(
    #General Race/Eth categories per study design
    RaceEth = case_when(hispyn == 1 ~ "Hispanic",
                             black == "Y" & is.na(white) & is.na(aian) &
                               is.na(asian) & is.na(NHOPI)  & hispyn !=1 ~"Black Not Hispanic",
                             white == "Y" & is.na(black) & is.na(aian) &
                               is.na(asian) & is.na(NHOPI)  & hispyn !=1 ~"White Not Hispanic",
                             TRUE ~"Other"),
      
         #Patient sexuality flags
         Gay = ifelse(any(sexuality == 11),1,0),
         Hetero = ifelse(any(sexuality_n == 10),1,0),
         Bisexual = ifelse(any(sexuality_n == 12),1,0),
         Sexuality_NA = ifelse(all(is.na(sexuality_n)),1,0),
        
         #Male partners flag    
         Male_partners_ever = case_when(male_partners > 0 ~ 1,
                                        is.na(male_partners) & is.na(female_partners) ~ NA_real_,
                                        is.na(male_partners) & !is.na(female_partners) ~ 0,
                                        male_partners==0 ~ 0),
         
         #Female partners flag
         Female_partners_ever = case_when(female_partners > 0 ~ 1,
                                          is.na(female_partners) & is.na(male_partners) ~ NA_real_,
                                          is.na(female_partners) & !is.na(male_partners) ~ 0,
                                          female_partners==0 ~ 0),
         
         Gay_ever = case_when(Sexuality_NA==1 & is.na(Gay) ~ NA_real_,
                              Sexuality_NA==0 & is.na(Gay)~ 0,
                              TRUE~Gay),
         Bisexual_ever = case_when(Sexuality_NA==1 & is.na(Bisexual) ~ NA_real_,
                                   Sexuality_NA==0 & is.na(Bisexual) ~ 0,
                                   TRUE ~ Bisexual),
         Hetero_ever = case_when(Sexuality_NA==1 & is.na(Hetero) ~ NA_real_,
                                 Sexuality_NA==0 & is.na(Hetero) ~ 0,
                                 TRUE ~ Hetero)) %>% 
  #One row of data per patient
  slice(c(1,n())) %>% 
  ungroup(patient_id) %>%
  
  #Drop variables
  select(-c(race_code_id,ethnicity_code_id,sexual_practice, sexuality,male_partners,female_partners)) %>% 
  
  #Join select variables from visit table
  left_join(Vis_study %>% select(patient_id,first_prep_vis,Tx_Cat),by = "patient_id") %>% 
  
  #Calculate age at first prep visit and age when Doxy became available in clinic
  mutate(Age_at_first_prep_vis = abs(interval((as.Date(first_prep_vis)),
                                              as.Date(dob)) %/% years(1)),
         Age_at_doxypep_availability = abs(interval((as.Date("2019-09-01")),
                                                    as.Date(dob)) %/% years(1))) %>% 
  #Select distinct rows by patient id
  distinct(patient_id,.keep_all = TRUE) %>% 
  
  #Determine gender identity of patient
  mutate(Gender_Ident = case_when(gender_code == "M" & (identified_gender_code_id =="M" | 
                                                   is.na(identified_gender_code_id) |
                                                   identified_gender_code_id=="U") ~ "M",
                           gender_code == "M" & identified_gender_code_id =="F" ~ "MTF",
                           gender_code == "M" & identified_gender_code_id =="M" ~ "M",
                           gender_code == "F" & identified_gender_code_id =="M" ~ "FTM",
                           gender_code == "F" & (identified_gender_code_id =="F" | is.na(identified_gender_code_id)) ~ "F")) %>% 
  
  #Determine MSM/MSMW
  mutate(Sex_With_Males = case_when(gender_code =="M" & (Gay_ever==1 | Bisexual_ever == 1 | Male_partners_ever == 1) ~ 1,
                                    Gender_Ident=="FTM" ~ 9,
                                    TRUE ~ 0)) %>% 
  
  #Retain Cis Male and Trans Female patients
  filter(Gender_Ident %in% c("M","MTF")) %>% 
  
  #Drop a bunch of variables
  select(patient_id,Tx_Cat,Age_at_first_prep_vis,Age_at_doxypep_availability,gender_code,identified_gender_code_id  ,Gender_Ident,Gay_ever,
         Hetero_ever,Gay_ever,Hetero_ever,Bisexual_ever,
         Male_partners_ever,Female_partners_ever,RaceEth,Sex_With_Males) %>% 
  
  #Create explicit flag indicating if a patient is trans
  mutate(Trans = ifelse(Gender_Ident=="M",0,1)) %>% 
  
  #Remove Cis males who have not reported any male partners
  filter(!(Gender_Ident == 0 & Sex_With_Males==0)) %>% 

  #remove patients who are under 18
  filter(Age_at_first_prep_vis >= 18) %>%
  
  #select distinct rows on patient id
  distinct(patient_id,.keep_all = TRUE)

#Final patient list for study after filtering on demographic inclusion criteria
patid2 <- unlist(unique(Demographics_Final$patient_id))


#VISIT LEVEL DATA FINAL 
visit_level_data_final <- 
  #remove any patients exlcuded during demographic data filtering
  Vis_study %>% filter(patient_id %in% patid2) %>% 
  
  #format date as ymd for CDC sas data sets
  mutate(visit_datetime = ymd(as.Date(visit_datetime))) %>% 
  select(-c(year,period,usage,comments,treatment_comments, last_doxypep_vis)) 


#CT/GC LABS

Labs_Final <- 
  #filter labs data based on patients list established as per inclusion criteria
  labs %>% filter(patient_id %in% patid2) %>%
  
  #Drop some variables
  select(patient_id,ds_clinic_visit_id,specimen_collection_date,
         ds_disease_code_id,ds_lab_test_type_code_id,ds_lab_test_specimen_source_code_id,ds_lab_test_result_code_id,ds_patient_lab_test_id) %>% 

  #Join lab result descriptions
  left_join(lab_result_code %>% select(ds_lab_test_result_code_id,ds_lab_test_result_code,ds_lab_test_result_desc),by = "ds_lab_test_result_code_id") %>%
  
  #Recode results so they are more general and readable
  mutate(Result = case_when(ds_lab_test_result_code %in% c('N','ND','NI','NR')~"Negative",
                            ds_lab_test_result_code %in% c('NS','D','P','R')~"Positive",
                            is.na(ds_lab_test_result_code) ~ NA_character_,
                            ds_lab_test_result_code %in% c('U','I', 'IND','') ~ "Eq/IND",
                            TRUE ~"Other"),
         
         #Create Disease variable based on test codes (what's being tested for)
         Disease = case_when(ds_lab_test_type_code_id %in% c(4 ,10, 12, 13 ,14 ,17 ,18 , 21, 34,39,45,86,92,96,97,100,111,113,117,119,120,166) ~ "CT",
                             ds_lab_test_type_code_id %in% c(3, 6, 11, 15, 19, 20, 31,40,85,91,99,102,112,116,165) ~ "GC",
                             is.na(ds_patient_lab_test_id) ~ NA_character_ ,
                             TRUE ~ "Other")) %>% 
  #Join specimen source (anatmoical site of testing)
  left_join(spec_src %>% select(ds_lab_test_specimen_source_code_id,ds_lab_test_specimen_source_desc),by = "ds_lab_test_specimen_source_code_id") %>% 
  
  #drop some variables
  select(patient_id,ds_clinic_visit_id,ds_patient_lab_test_id,specimen_collection_date,Disease,ds_lab_test_specimen_source_desc,Result) %>% 
  
  #Keep tests performed on or after date
  filter(specimen_collection_date >= as.Date("2015-01-01")) %>% 
  
  #Get rid of tests that aren't CT/GC
  filter(Disease != "Other") 


