
####Import libraries and datasets----
library("dplyr")
library("tidyr")
library("stringr")
library("readr")
library("lubridate")

## load the spreadsheets : ##
dir =""
clinical_data <- read_csv(paste0(dir,"progressa_clinical_data_2021_11_22.csv")) #load clinical df
demographic_details<- read_csv(paste0(dir,"progressa_demographic_details_2021_11_22.csv")) #used for demographic data and IDs

## List generic medication names and their drug names:  ##
Timolol<- c( "TENOPT", "COMBIGAN", "TIMOLOL","XALACOM", "COSOPT", "NYOGEL", "DUOTRAV", "GANFORT", "TIMOPTOL","TIMOPTIC","AZARGA", "AZAGA", "AZARGRA")
Brimodine <-c( "SIMBRINZA", "ALPHAGAN")
Betaxolol <-c("BETOPIC","BETOPTIC")

Acetazolamide <-c( "DIAMOX")
Brinzolamide <-c("SIMBRINZA", "AZOPT", "AZARGA", "AZAGA", "AZARGRA")
Dorzolamide <-c( "COSOPT", "TRUSOPT")

Brimatoprost <-c( "GANFORT", "LUMIGAN")
Travoprost <-c( "DUOTRAV","TRAVATAN")
Latanoprost <-c( "LATANOPROST", "LANTANOPROST","XALATAN", "XALANTAN", "XALACOM")
Tafluprost <-("SALFLUTAN")


## Prepare baseline demographic data ##
demographics<- demographic_details%>%
  mutate(prog_id =as.numeric(str_remove(ID, "PROG")))%>%
  select(prog_id, Gender, DOB,Ethnicity,FamilyHx)%>%
  mutate(FamilyHx=toupper(FamilyHx),Ethnicity=as.factor(Ethnicity),
         FamilyHx = ifelse(str_detect(FamilyHx, "RECORD")==TRUE, NA,
                           ifelse(FamilyHx=="NO", FALSE,  ifelse(FamilyHx=="YES", TRUE, NA))))

## Prepare relevant baseline covariates : ##
baseline_covariates<-clinical_data%>%
  select(VisitID, IOP_OD, IOP_OS, MD_OD, MD_OS, VCDR_OD, VCDR_OS)%>% #select IOP, MD, VCDR
  na.omit()%>%
  mutate(prog_id = as.numeric(str_extract(VisitID, "\\d{4}")))%>% #Convert Visit ID to PROGRESSA ID
  select(-VisitID)%>%
  filter(duplicated(prog_id)==FALSE)%>% #remove duplicated rows - i.e. second visit etc
  gather(parameter, measurement, -prog_id)%>%
  mutate(parameter = str_remove(parameter, "\\_O\\w"))%>%
  arrange(prog_id, parameter)%>%
  group_by(prog_id, parameter)%>%
  mutate(md = min(measurement),value = max(measurement))%>%
  mutate(value= ifelse(parameter=="MD", md, value))%>%
  select(prog_id, parameter, value)%>%
  filter(duplicated(prog_id)==FALSE)%>%
  spread(parameter, value)

covariates_compiled<-clinical_data%>%
  mutate(prog_id = as.numeric(str_extract(VisitID, "\\d{4}(?=\\.)")))%>%
  select(prog_id)%>%
  filter(duplicated(prog_id)==FALSE)%>%
  left_join(x=., y= demographics, by= "prog_id")%>%
  left_join(x=., y= baseline_covariates, by= "prog_id")%>%
  arrange(prog_id)


## Construct spreadsheet for summary of treatment data : ##
colnames(clinical_data) = gsub("[[:space:]]", "\\_", colnames(clinical_data))
medication_spreadsheet<-clinical_data%>%

  #Prepare spreadsheet :
  #topical data was inputed into either a column called 'glc medication' or into several columns called 'RE medication1'. Could not uniformly use one of another, so have to concatenate them together
  mutate(GLC_Medication= ifelse(is.na(GLC_Medication)==TRUE, paste0(`RE_Medctn_1`,", ",`RE_Medctn_2`,", ", `RE_Medctn_3`,", ", `RE_Medctn_4`,", ", `LE_Medctn_1`,", ", `LE_Medctn_2`, `LE_Medctn_3`,", ", `LE_Medctn_4`), GLC_Medication),
         GLC_Medication= ifelse(str_detect(GLC_Medication, "NA")==TRUE, "NIL", GLC_Medication), #If blank, indicate no treatment

         GLC_Medication= toupper(GLC_Medication), #convert to upper case for uniformity
         SLTOD_pd=toupper(SLTOD_pd),SLTOS_pd=toupper(SLTOS_pd),TRABOD_pd = toupper(TRABOD_pd),TRABOS_pd = toupper(TRABOS_pd), #convert SLT and Trab data to upper case

         prog_id = as.numeric(str_extract(VisitID, "\\d{4}(?=\\.)")))%>% #convert visit ids to PROGRESSA IDs (Visit IDs are of the form: paste0("PROG ID Number",".","Visit Number") - i.e. visit 1 PROGRESSA ID 1 = 0001.01)

  filter(str_detect(VisitID, "\\.00")==FALSE)%>% #sometimes a row is inputted at the time of ? enrolment- but no other parameters are included - remove these

  #select relevant column names
  select(prog_id,VisitID,Visit_Date,GLC_Medication,SLTOD_pd,SLTOS_pd,TRABOD_pd,TRABOS_pd)%>% #I've never known what the '_pd' on the PROGRESSA df means

  #Extract medication data :
  #Use stringr to detect whether a patient was on a medication at a given time point. Medications were inputted as generic names, so I've converted them to drug names.
  mutate(Brimodine = as.logical(str_detect(string = GLC_Medication,pattern= paste(Brimodine,collapse="|"))),

         Timolol = as.logical(str_detect(string = GLC_Medication,pattern= paste(Timolol,collapse="|"))),
         Betaxolol  = as.logical(str_detect(string = GLC_Medication,pattern= paste(Betaxolol,collapse="|"))),

         Acetazolamide= as.logical(str_detect(string = GLC_Medication,pattern= paste(Acetazolamide,collapse="|"))),
         Brinzolamide = as.logical(str_detect(string = GLC_Medication,pattern= paste(Brinzolamide,collapse="|"))),
         Dorzolamide = as.logical(str_detect(string = GLC_Medication,pattern= paste(Dorzolamide,collapse="|"))),

         Brimatoprost = as.logical(str_detect(string = GLC_Medication,pattern= paste(Brimatoprost,collapse="|"))),
         Travoprost = as.logical(str_detect(string = GLC_Medication,pattern= paste(Travoprost,collapse="|"))),
         Latanoprost = as.logical(str_detect(string = GLC_Medication,pattern= paste(Latanoprost,collapse="|"))),
         Tafluprost = as.logical(str_detect(string = GLC_Medication,pattern= paste(Tafluprost,collapse="|"))),

         #SLT data is of the form : Yes or No:
         SLT = as.logical(SLTOD_pd=="YES"|SLTOS_pd=="YES"),
         Trab = as.logical(TRABOD_pd=="YES"|TRABOS_pd=="YES"),

         #Calculate number of agents that a patient is on at a given timepoint.
         agent_number = Brimodine+ Timolol+ Betaxolol+Acetazolamide+Brinzolamide+Dorzolamide+Brimatoprost+Travoprost+Latanoprost+Tafluprost+SLT+Trab,

         #Determine if someone is one treatment at a time point
         #---***** SHOULD THIS BE 'if someone is on any treatment at a time point'??? *******---#
         # as it is, as.logical(agent_number==0) gives TRUE for those not on treament!
         #treatment = as.logical(agent_number==0))%>%
         treatment = as.logical(agent_number!=0))%>%

  #Calculate some of the treatment summary statistics:
  group_by(prog_id)%>%
  mutate(max_agents = max(agent_number), #maximum number of agents (including SLT/Trab) at any time point
         #--- NOT SURE WHY NOT JUST USE SLT + Trab vars created above???
         SLT_during_monitoring = ifelse(sum(SLT=="TRUE")>0|sum(SLTOS_pd=="YES")>0, TRUE, FALSE), #To know if patient had an SLT at all during monitoring
         TRAB_during_monitoring = ifelse(sum(TRABOD_pd=="YES")>0|sum(TRABOS_pd=="YES")>0,TRUE, #To know if patient had a Trab at all during monitoring
                                         ifelse(sum(TRABOD_pd=="YES")==0&sum(TRABOS_pd=="YES")==0&sum(TRABOD_pd=="NO")==0&sum(TRABOS_pd=="NO")==0,NA,FALSE)))%>%

  #Then assess if a patient had treatment increase during monitoring of not:
  mutate(baseline_treated=dplyr::first(treatment),#?treated at enrolment
        increase_rx = agent_number> dplyr::first(agent_number),
        increase_rx_during_monitoring =ifelse(sum((agent_number >  dplyr::first(agent_number))==TRUE) > 0,TRUE, FALSE))%>% #determines if at any time point, the patient had an increase in treatment.
  ungroup()

## calculate date for commencement of treatment ----
treatment_start<-medication_spreadsheet%>%
  filter(treatment ==TRUE)%>%
  group_by(prog_id)%>%
  mutate(treatment_start =  dplyr::first(Visit_Date))%>%
  select(prog_id, baseline_treated,Visit_Date,treatment_start )%>%
  filter(duplicated(prog_id)==FALSE)%>%
  select(-Visit_Date,-baseline_treated)

## calculate date for increase in treatment ----
treatment_increase_date<-medication_spreadsheet%>%
  filter(increase_rx ==TRUE)%>%
  group_by(prog_id)%>%
  mutate(increase_date = dplyr::first(Visit_Date))%>%
  select(prog_id, baseline_treated,Visit_Date,increase_date)%>%
  filter(duplicated(prog_id)==FALSE)%>%
  select(-Visit_Date,-baseline_treated)

## Compile the final  medication spreadsheet
complete_medication_spreadsheet<-medication_spreadsheet%>%
  left_join(x=., y = treatment_start, by= "prog_id")%>%
  left_join(x=., y = treatment_increase_date, by= "prog_id")%>%
  filter(str_detect(VisitID, "\\.01$")==TRUE)%>% # tricky some VisitID are .015, be more explicit in string
  mutate(treatment_start = ifelse(is.na(treatment_start)==TRUE, NA, treatment_start))%>%
  rename(baseline_date= Visit_Date,baseline_agent_number=agent_number)%>%
  mutate(time_to_increase= ifelse(increase_rx_during_monitoring ==TRUE, as.numeric(dmy(increase_date) - dmy(baseline_date))/365.25,
         as.numeric(dmy("12/11/2021") - dmy(baseline_date))/365.25))%>%

  #Stipulate that treatment initiation must occur within 5 years of enrolment
  mutate(increase_rx_during_monitoring_censored=ifelse(time_to_increase> 5,FALSE,increase_rx_during_monitoring ),
         time_to_increase_censored=ifelse(time_to_increase>5,5,time_to_increase))%>%
  select(prog_id, baseline_date, baseline_treated, baseline_agent_number,max_agents,increase_rx_during_monitoring,time_to_increase,increase_rx_during_monitoring_censored,time_to_increase_censored,increase_date,SLT_during_monitoring,TRAB_during_monitoring )

write_csv(complete_medication_spreadsheet,paste0(dir,'glaucoma_prs_treatment_study_2023_01_08.csv')
