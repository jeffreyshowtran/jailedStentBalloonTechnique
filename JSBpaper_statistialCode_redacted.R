#jailed balloon bifurcationl stenting data 

library(dplyr)
library(tidyverse)
library(Hmisc)
library(lubridate)
library(survival) 
library(ggplot2)
library(ggsurvfit)
library(survminer)
library(tidycmprsk)

setwd('/Users/Desktop/Projects/jailedBalloonStenting/data2')

#### read and label data #### 
#Read Data
data=read.csv('JailedStentBalloonSt_DATA.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$patient_id)="Patient ID"
label(data$date_of_birth)="Date of Birth"
label(data$sex)="Sex"
label(data$race)="Race"
label(data$height_cm)="Height (cm)"
label(data$weight_kg)="Weight (kg)"
label(data$smoker)="Smoker"
label(data$hypertension)="Hypertension"
label(data$dyslipidemia)="Dyslipidemia"
label(data$diabetes)="Diabetes"
label(data$diabetes_management)="Diabetes Management"
label(data$kidney_disease)="Kidney Disease"
label(data$copd)="COPD"
label(data$family_history_of_cad)="Family history of CAD"
label(data$prior_cad)="Prior CVA"
label(data$prior_pad)="Prior PAD"
label(data$prior_mi)="Prior MI"
label(data$prior_heart_failure)="Prior Heart Failure"
label(data$estimated_ejection_fractio)="Estimated Ejection Fraction (%)"
label(data$prior_valve_surgery)="Prior Valve Surgery"
label(data$prior_pci)="Prior PCI"
label(data$prior_pci_date)="Prior PCI Date"
label(data$prior_cabg)="Prior CABG"
label(data$prior_cabg_date)="Prior CABG Date"
label(data$prior_cardiac_arrest)="Prior Cardiac Arrest"
label(data$demographics_complete)="Complete?"
label(data$symptoms_at_presentation)="Symptoms at Presentation"
label(data$angina_class)="Angina Class"
label(data$pre_operative_evaluation)="Pre-operative Evaluation"
label(data$anti_anginal_medications_p)="Anti-anginal medications pre-procedure"
label(data$anti_anginal_meds___1)="Anti-anginal Medications Pre-Procedure (choice=Beta Blockers)"
label(data$anti_anginal_meds___2)="Anti-anginal Medications Pre-Procedure (choice=Calcium Channel Blockers)"
label(data$anti_anginal_meds___3)="Anti-anginal Medications Pre-Procedure (choice=Long Acting Nitrates)"
label(data$anti_anginal_meds___4)="Anti-anginal Medications Pre-Procedure (choice=Ranolazine)"
label(data$anti_anginal_meds___5)="Anti-anginal Medications Pre-Procedure (choice=Other)"
label(data$heart_failure_2_weeks_prio)="Heart Failure symptoms 2 weeks prior to presentation"
label(data$heart_failure_class)="Heart Failure Class"
label(data$preprocedural_characteristics_complete)="Complete?"
label(data$date_of_procedure)="Date of Procedure"
label(data$acuity_of_case)="Acuity of case"
label(data$access)="Access"
label(data$coronary_dominance)="Coronary Dominance"
label(data$complexity)="Complexity"
label(data$syntax_score)="Syntax Score"
label(data$guide_size_fr)="Guide Size (Fr)"
label(data$procedural_anticoagulation)="Procedural Anticoagulation"
label(data$coronary_physiology)="Coronary Physiology"
label(data$intracoronary_imaging)="Intracoronary Imaging"
label(data$atherectomy_lithotripsy)="Atherectomy/Lithotripsy"
label(data$intervention_for_isr)="Intervention for ISR"
label(data$mechanical_support)="Mechanical Support"
label(data$medina_class)="Medina Class"
label(data$main_branch)="Main Branch"
label(data$side_branch)="Side Branch"
label(data$main_branch_stent_type)="Main Branch Stent Type"
label(data$main_branch_stent_size)="Main Branch Stent Diameter"
label(data$main_branch_pre_stenosis)="Main Branch Pre-Stenosis (%)"
label(data$main_branch_post_stenosis)="Main Branch Post Stenosis (%)"
label(data$side_branch_stent_type)="Side Branch Stent Type"
label(data$side_branch_stent_diameter)="Side Branch Stent Diameter"
label(data$side_branch_pre_stenosis)="Side Branch Pre Stenosis (%)"
label(data$side_branch_post_stenosis)="Side Branch Post Stenosis (%)"
label(data$access_closure)="Access Closure"
label(data$fluoro_time)="Fluoro Time"
label(data$contrast_used)="Contrast Used"
label(data$coronary_dissection_from_i)="Coronary Dissection from Intervention"
label(data$no_reflow)="No Reflow"
label(data$side_branch_occlusion)="Side Branch Occlusion"
label(data$procedural_characteristics_complete)="Complete?"
label(data$discharge_date)="Discharge Date"
label(data$vital_status_at_discharge)="Vital status at discharge"
label(data$post_procedure_bleeding_co)="Post-Procedure Bleeding Complications"
label(data$post_procedure_aki)="Post-Procedure AKI"
label(data$target_lesion_revasculariz)="Target Lesion Revascularization"
label(data$site_of_target_lesion_reva)="Site of Target Lesion Revascularization"
label(data$target_lesion_date)="Date of Target Lesion Revascularization"
label(data$target_vessel_revasculariz)="Target Vessel Revascularization"
label(data$date_vessel_revasc)="Date of Target Vessel Revascularization"
label(data$target_lesion_mi)="Target Vessel MI"
label(data$date_of_target_vessel_mi)="Date of Target Vessel MI"
label(data$cardiac_death)="Cardiac Death"
label(data$death_from_any_cause)="Death from any cause"
label(data$death_date)="Death date"
label(data$last_encounter_date_in_the)="Last encounter (date) in the chart"
label(data$outcomes_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$sex.factor = factor(data$sex,levels=c("1","2"))
data$race.factor = factor(data$race,levels=c("1","2","3","4","5"))
data$smoker.factor = factor(data$smoker,levels=c("1","2","3"))
data$hypertension.factor = factor(data$hypertension,levels=c("1","0"))
data$dyslipidemia.factor = factor(data$dyslipidemia,levels=c("1","0"))
data$diabetes.factor = factor(data$diabetes,levels=c("1","0"))
data$diabetes_management.factor = factor(data$diabetes_management,levels=c("1","2"))
data$kidney_disease.factor = factor(data$kidney_disease,levels=c("1","2","3"))
data$copd.factor = factor(data$copd,levels=c("1","0"))
data$family_history_of_cad.factor = factor(data$family_history_of_cad,levels=c("1","0"))
data$prior_cad.factor = factor(data$prior_cad,levels=c("1","0"))
data$prior_pad.factor = factor(data$prior_pad,levels=c("1","0"))
data$prior_mi.factor = factor(data$prior_mi,levels=c("1","0"))
data$prior_heart_failure.factor = factor(data$prior_heart_failure,levels=c("1","0"))
data$prior_valve_surgery.factor = factor(data$prior_valve_surgery,levels=c("1","0"))
data$prior_pci.factor = factor(data$prior_pci,levels=c("1","0"))
data$prior_cabg.factor = factor(data$prior_cabg,levels=c("1","0"))
data$prior_cardiac_arrest.factor = factor(data$prior_cardiac_arrest,levels=c("1","0"))
data$demographics_complete.factor = factor(data$demographics_complete,levels=c("0","1","2"))
data$symptoms_at_presentation.factor = factor(data$symptoms_at_presentation,levels=c("1","2","3","4","5"))
data$angina_class.factor = factor(data$angina_class,levels=c("1","2","3","4"))
data$pre_operative_evaluation.factor = factor(data$pre_operative_evaluation,levels=c("1","0"))
data$anti_anginal_medications_p.factor = factor(data$anti_anginal_medications_p,levels=c("1","0"))
data$anti_anginal_meds___1.factor = factor(data$anti_anginal_meds___1,levels=c("0","1"))
data$anti_anginal_meds___2.factor = factor(data$anti_anginal_meds___2,levels=c("0","1"))
data$anti_anginal_meds___3.factor = factor(data$anti_anginal_meds___3,levels=c("0","1"))
data$anti_anginal_meds___4.factor = factor(data$anti_anginal_meds___4,levels=c("0","1"))
data$anti_anginal_meds___5.factor = factor(data$anti_anginal_meds___5,levels=c("0","1"))
data$heart_failure_2_weeks_prio.factor = factor(data$heart_failure_2_weeks_prio,levels=c("1","0"))
data$heart_failure_class.factor = factor(data$heart_failure_class,levels=c("1","2","3","4"))
data$preprocedural_characteristics_complete.factor = factor(data$preprocedural_characteristics_complete,levels=c("0","1","2"))
data$acuity_of_case.factor = factor(data$acuity_of_case,levels=c("1","2"))
data$access.factor = factor(data$access,levels=c("1","2","3"))
data$coronary_dominance.factor = factor(data$coronary_dominance,levels=c("1","2","3"))
data$complexity.factor = factor(data$complexity,levels=c("1","2","3","4"))
data$syntax_score.factor = factor(data$syntax_score,levels=c("1","2","3"))
data$guide_size_fr.factor = factor(data$guide_size_fr,levels=c("6","7","8"))
data$procedural_anticoagulation.factor = factor(data$procedural_anticoagulation,levels=c("1","2"))
data$coronary_physiology.factor = factor(data$coronary_physiology,levels=c("1","0"))
data$intracoronary_imaging.factor = factor(data$intracoronary_imaging,levels=c("1","0"))
data$atherectomy_lithotripsy.factor = factor(data$atherectomy_lithotripsy,levels=c("1","0"))
data$intervention_for_isr.factor = factor(data$intervention_for_isr,levels=c("1","0"))
data$mechanical_support.factor = factor(data$mechanical_support,levels=c("1","0"))
data$main_branch.factor = factor(data$main_branch,levels=c("1","2","3","4","5"))
data$side_branch.factor = factor(data$side_branch,levels=c("1","2","3","4","5","6"))
data$main_branch_stent_type.factor = factor(data$main_branch_stent_type,levels=c("1","2","3","4"))
data$side_branch_stent_type.factor = factor(data$side_branch_stent_type,levels=c("1","2","3","4"))
data$access_closure.factor = factor(data$access_closure,levels=c("1","2","3","4"))
data$coronary_dissection_from_i.factor = factor(data$coronary_dissection_from_i,levels=c("1","0"))
data$no_reflow.factor = factor(data$no_reflow,levels=c("1","0"))
data$side_branch_occlusion.factor = factor(data$side_branch_occlusion,levels=c("1","0"))
data$procedural_characteristics_complete.factor = factor(data$procedural_characteristics_complete,levels=c("0","1","2"))
data$vital_status_at_discharge.factor = factor(data$vital_status_at_discharge,levels=c("1","2"))
data$post_procedure_bleeding_co.factor = factor(data$post_procedure_bleeding_co,levels=c("1","0"))
data$post_procedure_aki.factor = factor(data$post_procedure_aki,levels=c("1","0"))
data$target_lesion_revasculariz.factor = factor(data$target_lesion_revasculariz,levels=c("1","0"))
data$site_of_target_lesion_reva.factor = factor(data$site_of_target_lesion_reva,levels=c("1","2","3"))
data$target_vessel_revasculariz.factor = factor(data$target_vessel_revasculariz,levels=c("1","0"))
data$target_lesion_mi.factor = factor(data$target_lesion_mi,levels=c("1","0"))
data$cardiac_death.factor = factor(data$cardiac_death,levels=c("1","0"))
data$death_from_any_cause.factor = factor(data$death_from_any_cause,levels=c("1","0"))
data$outcomes_complete.factor = factor(data$outcomes_complete,levels=c("0","1","2"))

levels(data$sex.factor)=c("Female","Male")
levels(data$race.factor)=c("White","Black","Asian","American Indian","Other")
levels(data$smoker.factor)=c("Non-Smoker","Current","Prior")
levels(data$hypertension.factor)=c("Yes","No")
levels(data$dyslipidemia.factor)=c("Yes","No")
levels(data$diabetes.factor)=c("Yes","No")
levels(data$diabetes_management.factor)=c("Oral","Insulin")
levels(data$kidney_disease.factor)=c("None","CKD","ESRD")
levels(data$copd.factor)=c("Yes","No")
levels(data$family_history_of_cad.factor)=c("Yes","No")
levels(data$prior_cad.factor)=c("Yes","No")
levels(data$prior_pad.factor)=c("Yes","No")
levels(data$prior_mi.factor)=c("Yes","No")
levels(data$prior_heart_failure.factor)=c("Yes","No")
levels(data$prior_valve_surgery.factor)=c("Yes","No")
levels(data$prior_pci.factor)=c("Yes","No")
levels(data$prior_cabg.factor)=c("Yes","No")
levels(data$prior_cardiac_arrest.factor)=c("Yes","No")
levels(data$demographics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$symptoms_at_presentation.factor)=c("Asymptomatic","Stable Angina","Unstable Angina","NSTEMI","STEMI")
levels(data$angina_class.factor)=c("Class I","Class II","Class III","Class IV")
levels(data$pre_operative_evaluation.factor)=c("Yes","No")
levels(data$anti_anginal_medications_p.factor)=c("Yes","No")
levels(data$anti_anginal_meds___1.factor)=c("Unchecked","Checked")
levels(data$anti_anginal_meds___2.factor)=c("Unchecked","Checked")
levels(data$anti_anginal_meds___3.factor)=c("Unchecked","Checked")
levels(data$anti_anginal_meds___4.factor)=c("Unchecked","Checked")
levels(data$anti_anginal_meds___5.factor)=c("Unchecked","Checked")
levels(data$heart_failure_2_weeks_prio.factor)=c("Yes","No")
levels(data$heart_failure_class.factor)=c("Class I","Class II","Class III","Class IV")
levels(data$preprocedural_characteristics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$acuity_of_case.factor)=c("Urgent","Elective")
levels(data$access.factor)=c("Femoral","Radial","Brachial")
levels(data$coronary_dominance.factor)=c("Right","Left","Co-Dominant")
levels(data$complexity.factor)=c("A","B1","B2","C")
levels(data$syntax_score.factor)=c("Low (< 16)","Intermediate (17-22)","High (>22)")
levels(data$guide_size_fr.factor)=c("6","7","8")
levels(data$procedural_anticoagulation.factor)=c("Heparin","Bivalirudin")
levels(data$coronary_physiology.factor)=c("Yes","No")
levels(data$intracoronary_imaging.factor)=c("Yes","No")
levels(data$atherectomy_lithotripsy.factor)=c("Yes","No")
levels(data$intervention_for_isr.factor)=c("Yes","No")
levels(data$mechanical_support.factor)=c("Yes","No")
levels(data$main_branch.factor)=c("Left Main to LAD","Left Main to LCX","LAD","LCX","RCA")
levels(data$side_branch.factor)=c("LAD","Circumflex","Diagonal","OM","PDA","PLV")
levels(data$main_branch_stent_type.factor)=c("Promus/Synergy","Xience","Resolute","Orsiro")
levels(data$side_branch_stent_type.factor)=c("Promus/Synergy","Xience","Resolute","Orsiro")
levels(data$access_closure.factor)=c("Manual/TR Band","Angioseal","ProGlide","Celt")
levels(data$coronary_dissection_from_i.factor)=c("Yes","No")
levels(data$no_reflow.factor)=c("Yes","No")
levels(data$side_branch_occlusion.factor)=c("Yes","No")
levels(data$procedural_characteristics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$vital_status_at_discharge.factor)=c("Alive","Dead")
levels(data$post_procedure_bleeding_co.factor)=c("Yes","No")
levels(data$post_procedure_aki.factor)=c("Yes","No")
levels(data$target_lesion_revasculariz.factor)=c("Yes","No")
levels(data$site_of_target_lesion_reva.factor)=c("Main Branch","Side Branch","Both Branches")
levels(data$target_vessel_revasculariz.factor)=c("Yes","No")
levels(data$target_lesion_mi.factor)=c("Yes","No")
levels(data$cardiac_death.factor)=c("Yes","No")
levels(data$death_from_any_cause.factor)=c("Yes","No")
levels(data$outcomes_complete.factor)=c("Incomplete","Unverified","Complete")



#### tables / descriptive stats ####
##age at time of procedure

#convert character dates to datetime format 
data$date_of_birth <- as.POSIXct(data$date_of_birth, format="%Y-%m-%d")

data$date_of_procedure <- as.POSIXct(data$date_of_procedure, format="%Y-%m-%d")
#calculate age 
data$age <- time_length(difftime(data$date_of_procedure, data$date_of_birth), "years")
data <- data %>% relocate(date_of_procedure, age, .after = date_of_birth)
#age desc stats 
summary(data$age)
#hist(data$age)

#sex
data <- data %>% 
  mutate(gender = ifelse(sex.factor == "Female", 0, 
                         ifelse(sex.factor == "Male", 1, 999))) %>% 
  relocate(gender, .after = sex)
table(data$sex.factor, exclude=NA)

#race 
table(data$race.factor, exclude=NA)

#bmi 
data <- data %>% 
  mutate(bmi = weight_kg/(height_cm/100)^2) %>% 
  relocate(bmi, .after = weight_kg)
summary(data$bmi)
#hist(data$bmi)

#smoker 
table(data$smoker.factor, exclude = NA)

#hypertension
table(data$hypertension.factor)

#hyperlipidemia 
table(data$dyslipidemia.factor)

#dm2 
table(data$diabetes.factor)
table(data$diabetes_management.factor)

#CKD 
table(data$kidney_disease.factor)

#COPD 
table(data$copd.factor)

#prior data 
table(data$prior_cad.factor)
table(data$prior_pad.factor)
table(data$prior_mi.factor)
table(data$prior_heart_failure.factor)
table(data$prior_valve_surgery.factor)
table(data$prior_pci.factor)
table(data$prior_cabg.factor)
table(data$prior_cardiac_arrest.factor)

data$prior_pci_date <- as.POSIXct(data$prior_pci_date, format="%Y-%m-%d")
data$prior_cabg_date <- as.POSIXct(data$prior_cabg_date, format="%Y-%m-%d")
data <- data %>% 
  mutate(prior_pci_dur = difftime(date_of_procedure, prior_pci_date), units="days") %>%
  mutate(prior_cabg_dur = difftime(date_of_procedure, prior_cabg_date), units="days") %>%
  relocate(prior_pci_date, prior_pci_dur, .after = prior_pci) %>% 
  relocate(prior_cabg_date, prior_cabg_dur, .after = prior_cabg)

summary(data$prior_pci_dur)
summary(data$prior_cabg_dur)

#what is the demographics complete var? 
table(data$demographics_complete.factor)
#nvm it's just a data quality check var 

#symptoms at presentation 
table(data$symptoms_at_presentation.factor)

#CCS class 
table(data$angina_class.factor)

#preop eval 
table(data$pre_operative_evaluation.factor)

#antianginals 
table(data$anti_anginal_medications_p.factor)
table(data$anti_anginal_meds___1.factor) #BB
table(data$anti_anginal_meds___2.factor) #CCB
table(data$anti_anginal_meds___3.factor) #organic nitrates
table(data$anti_anginal_meds___4.factor) #ranolazine
table(data$anti_anginal_meds___5.factor) #other


#heart failure stuff
#ejection fraction (in those with heart failure)
summary(data$estimated_ejection_fractio)

#recent prior HFH 
table(data$heart_failure_2_weeks_prio.factor)
table(data$heart_failure_class.factor)


#### procedural details ####
table(data$acuity_of_case.factor)
table(data$access.factor)
table(data$coronary_dominance.factor)
table(data$complexity.factor)
table(data$syntax_score.factor)
table(data$guide_size_fr)
table(data$procedural_anticoagulation.factor)
table(data$coronary_physiology.factor)
table(data$intracoronary_imaging.factor)
table(data$atherectomy_lithotripsy.factor)
table(data$intervention_for_isr.factor)
table(data$mechanical_support.factor)
table(data$medina_class)
table(data$main_branch.factor)
table(data$side_branch.factor)
table(data$main_branch_stent_type.factor)
table(data$main_branch_stent_size)
table(data$main_branch_pre_stenosis)
table(data$main_branch_post_stenosis)
table(data$side_branch_stent_type.factor)
table(data$side_branch_stent_diameter)
table(data$side_branch_pre_stenosis)
table(data$side_branch_post_stenosis)
table(data$access_closure.factor)

data$fluoro_time <- gsub(' min', '', data$fluoro_time)
data$fluoro_time <- as.numeric(data$fluoro_time)
summary(data$fluoro_time)

data$contrast_used <- str_trim(data$contrast_used)
data$contrast_used <- gsub(' .*', '', data$contrast_used)
data$contrast_used <- as.numeric(data$contrast_used)
summary(data$contrast_used)

table(data$coronary_dissection_from_i.factor)
table(data$no_reflow.factor)
table(data$side_branch_occlusion.factor)

####outcomes ####

#create LoS variable
data$discharge_date <- as.POSIXct(data$discharge_date, format="%Y-%m-%d")
data <- data %>% 
  mutate(length_of_stay = time_length(difftime(discharge_date, date_of_procedure), "days")) %>% 
  relocate(date_of_procedure, length_of_stay, .after = discharge_date)
summary(data$length_of_stay)
  
table(data$vital_status_at_discharge.factor)

#complications
table(data$post_procedure_bleeding_co.factor)
table(data$post_procedure_aki.factor)

#TLR
table(data$target_lesion_revasculariz.factor)
table(data$site_of_target_lesion_reva.factor)

#how long after the index procedure did TLR occur? 
data$target_lesion_date <- as.POSIXct(data$target_lesion_date, format="%Y-%m-%d")
data <- data %>% 
  mutate(TLR_dur = time_length(difftime(target_lesion_date, date_of_procedure), "days")) %>% 
  relocate(TLR_dur, .after = target_lesion_date)
summary(data$TLR_dur)

#TLMI
table(data$target_lesion_mi.factor)

#how long after the index procedure did TLMI occur? 
data$date_of_target_vessel_mi <- as.POSIXct(data$date_of_target_vessel_mi, format="%Y-%m-%d")
data <- data %>% 
  mutate(TLMI_dur = time_length(difftime(date_of_target_vessel_mi, date_of_procedure), "days")) %>% 
  relocate(TLMI_dur, .after = date_of_target_vessel_mi)
summary(data$TLMI_dur)
#data[data$target_lesion_mi.factor == "Yes", c('patient_id', 'date_of_target_vessel_mi', 'date_of_procedure')]

#TVR 
table(data$target_vessel_revasculariz.factor)

data$date_vessel_revasc <- as.POSIXct(data$date_vessel_revasc, format="%Y-%m-%d")
data <- data %>% 
  mutate(TVR_dur = time_length(difftime(date_vessel_revasc, date_of_procedure), "days")) %>% 
  relocate(TVR_dur, .after = date_vessel_revasc)
summary(data$TVR_dur)

#death 
table(data$cardiac_death.factor)
table(data$death_from_any_cause.factor)

#how long since procedure did patient die (any cause)?
data$death_date <- as.POSIXct(data$death_date, format="%Y-%m-%d")
data <- data %>% 
  mutate(death_dur = time_length(difftime(death_date, date_of_procedure), "days")) %>% 
  relocate(death_dur, .after = death_date)
summary(data$death_dur)

#when was the patient last followed? (e.g. left censor time)
data$last_encounter_date_in_the <-as.POSIXct(data$last_encounter_date_in_the, format="%Y-%m-%d")
data <- data %>% 
  mutate(censor_time = time_length(difftime(last_encounter_date_in_the, date_of_procedure), "days")) %>% 
  relocate(censor_time, .after = last_encounter_date_in_the)
summary(data$censor_time)

####<365 follow up#### 
#need list of all patients followed for <365 days to call and complete data 
list <- subset(data, data$censor_time < 365, select = c(record_id, patient_id))
#write.csv(list, "callList.csv")

####primary outcomes for the paper: ####
#analytical outline: 
#1 year events (TLF, mortality) 
#kaplan meyer by medina class

#create variable indicating incidence of TLR at 1 and 3 years 
data <- data %>% 
  mutate(TLR_1yr = ifelse(target_lesion_revasculariz.factor == 'Yes' & #TLR happened 
                            TLR_dur <= 365, 1, 0)) %>% #TLR occurred within 1yr 
  relocate(TLR_1yr, .after = TLR_dur)
# data$TLR_1yr <- ifelse(data$censor_time < 365 & 
#                          data$target_lesion_revasculariz.factor == 'No', 
#                        NA, 
#                        data$TLR_1yr)
table(data$target_lesion_revasculariz.factor)
addmargins(table(data$TLR_1yr, exclude=NA))

#time to TLR in 1yr (safety check) - it checks out 
#summary(subset(data$TLR_dur, data$TLR_1yr==1))
#hist(subset(data$TLR_dur, data$TLR_1yr==1))

data <- data %>% 
  mutate(TLR_3yr = ifelse(target_lesion_revasculariz.factor == 'Yes' & #TLR happened 
                            TLR_dur <= 1095, 1, 0)) %>% #TLR occurred within 1yr 
  relocate(TLR_3yr, .after = TLR_1yr)
# data$TLR_3yr <- ifelse(data$censor_time < 1095 &
#                          data$target_lesion_revasculariz.factor == 'No',
#                        NA,
#                        data$TLR_3yr)
addmargins(table(data$TLR_3yr, exclude=NA))

#create variable indicating incidence of TLMI at 1 and 3 years
data <- data %>% 
  mutate(TLMI_1yr = ifelse(target_lesion_mi.factor == 'Yes' & #TLMI happened 
                            TLMI_dur <= 365, 1, 0)) %>% #TLMI occurred within 1yr 
  relocate(TLMI_1yr, .after = TLMI_dur)
addmargins(table(data$TLMI_1yr, exclude=NA))

data <- data %>% 
  mutate(TLMI_3yr = ifelse(target_lesion_revasculariz.factor == 'Yes' & #TLR happened 
                             TLMI_dur <= 1095, 1, 0)) %>% #TLR occurred within 1yr 
  relocate(TLMI_3yr, .after = TLR_1yr)
addmargins(table(data$TLMI_3yr, exclude=NA))

#create variable indicating cardiac death at 1 and 3 years 
#for some odd reason, values for cardiac death are coded as NA if death is 
#deemed to be from another cause. Need to recode these values
table(data$cardiac_death.factor, useNA = "ifany")
table(data$cardiac_death, useNA = "ifany")
data$cardiac_death2 <- ifelse(is.na(data$cardiac_death) & 
                                       data$death_from_any_cause == 0, 
                                     0, 
                                     data$cardiac_death)
data$cardiac_death2.factor = factor(data$cardiac_death2,levels=c("1","0"))
levels(data$cardiac_death2.factor)=c("Yes","No")
table(data$cardiac_death2.factor, useNA = "ifany")
data <- data %>% relocate(cardiac_death2.factor, .after=cardiac_death)


data <- data %>% 
  mutate(cdeath_1yr = ifelse(cardiac_death2.factor == 'Yes' & 
                               death_dur <= 365, 1, 0)) %>%
  relocate(cdeath_1yr, .after=death_dur)
data$cdeath_1yr <- ifelse(data$death_dur <= 365 & is.na(data$cdeath_1yr), 
                          1, 
                          data$cdeath_1yr) #assume all deaths are cardiac if not otherwise stated

addmargins(table(data$cdeath_1yr, exclude=NA))

data <- data %>% 
  mutate(cdeath_3yr = ifelse(cardiac_death2.factor == 'Yes' & 
                               death_dur <= 1095, 1, 0)) %>%
  relocate(cdeath_3yr, .after=cdeath_1yr)
data$cdeath_3yr <- ifelse(data$death_dur <= 1095 & is.na(data$cdeath_3yr), 
                          1, 
                          data$cdeath_3yr) #assume all deaths are cardiac if not otherwise stated
addmargins(table(data$cdeath_3yr, exclude=NA))

#create composite variable 
#1 yr composite
data <- data %>% 
  mutate(outcome_1yr = ifelse(TLR_1yr == 1 | 
                            TLMI_1yr == 1 | 
                            cdeath_1yr == 1, 
                          1, 
                          0)) %>% 
  relocate(outcome_1yr, .after=censor_time)
addmargins(table(data$outcome_1yr, exclude = NA))

#3yr composite 
data <- data %>% 
  mutate(outcome_3yr = ifelse(TLR_3yr == 1 | 
                                TLMI_3yr == 1 | 
                                cdeath_3yr == 1, 
                              1, 
                              0)) %>% 
  relocate(outcome_3yr, .after=outcome_1yr)
addmargins(table(data$outcome_3yr, exclude = NA))

#patients with <365 have been called, update time to last follow up#
data$last_encounter_date_in_the2 <- ifelse(data$censor_time < 365 & data$outcome_1yr == 0,
                                           as.POSIXct("2025-01-02", format="%Y-%m-%d"), 
                                           data$last_encounter_date_in_the)
data$last_encounter_date_in_the2 <- as.POSIXct(data$last_encounter_date_in_the2, format="%Y-%m-%d")
data <- data %>% relocate(last_encounter_date_in_the2, .after=last_encounter_date_in_the)

#update censor time
data <- data %>% 
  mutate(censor_time2 = time_length(difftime(last_encounter_date_in_the2, date_of_procedure), "days")) %>% 
  relocate(censor_time2, .after = censor_time)

#if outcomes occur before last chart check, update censortime to be time to first occurring outcome 
data$censor_time2 <- ifelse(!is.na(data$death_dur) & 
                              data$death_dur < data$censor_time2, 
                            data$death_dur, 
                            data$censor_time2)

data$censor_time2 <- ifelse(!is.na(data$TLR_dur) & 
                              data$TLR_dur < data$censor_time2, 
                            data$TLR_dur, 
                            data$censor_time2)

data$censor_time2 <- ifelse(!is.na(data$TLMI_dur) & 
                              data$TLMI_dur < data$censor_time2, 
                            data$TLMI_dur, 
                            data$censor_time2)
summary(data$censor_time2)



paste0("1yr composite: ", round(mean(subset(data$outcome_1yr, !is.na(data$outcome_1yr)))*100, 2), "%")
paste0("3yr composite: ", round(mean(subset(data$outcome_3yr, !is.na(data$outcome_3yr)))*100, 2), "%")
paste0("1yr TLR: ", round(mean(subset(data$TLR_1yr, !is.na(data$TLR_1yr)))*100, 2), "%")
paste0("3yr TLR: ", round(mean(subset(data$TLR_3yr, !is.na(data$TLR_3yr)))*100, 2), "%")
paste0("1yr TLMI: ", round(mean(subset(data$TLMI_1yr, !is.na(data$TLMI_1yr)))*100, 2), "%")
paste0("3yr TLMI: ", round(mean(subset(data$TLMI_3yr, !is.na(data$TLMI_3yr)))*100, 2), "%")
paste0("1yr cardiac death: ", round(mean(subset(data$cdeath_1yr, !is.na(data$cdeath_1yr)))*100, 2), "%")
paste0("3yr cardiac death: ", round(mean(subset(data$cdeath_3yr, !is.na(data$cdeath_3yr)))*100, 2), "%")

paste("outcomes at 1 and 3yrs in allcomers receiving bifurcation stenting")
paste0("1yr composite: ", round(mean(subset(data$outcome_1yr, !is.na(data$outcome_1yr)))*100, 2), "%")
paste0("3yr composite: ", round(mean(subset(data$outcome_3yr, !is.na(data$outcome_3yr)))*100, 2), "%")
paste0("1yr TLR: ", round(mean(subset(data$TLR_1yr, !is.na(data$TLR_1yr)))*100, 2), "%")
paste0("3yr TLR: ", round(mean(subset(data$TLR_3yr, !is.na(data$TLR_3yr)))*100, 2), "%")
paste0("1yr TLMI: ", round(mean(subset(data$TLMI_1yr, !is.na(data$TLMI_1yr)))*100, 2), "%")
paste0("3yr TLMI: ", round(mean(subset(data$TLMI_3yr, !is.na(data$TLMI_3yr)))*100, 2), "%")
paste0("1yr cardiac death: ", round(mean(subset(data$cdeath_1yr, !is.na(data$cdeath_1yr)))*100, 2), "%")
paste0("3yr cardiac death: ", round(mean(subset(data$cdeath_3yr, !is.na(data$cdeath_3yr)))*100, 2), "%")

table(data$main_branch.factor, data$site_of_target_lesion_reva.factor)
table(data$side_branch.factor, data$site_of_target_lesion_reva.factor)

#### secondary analyses by LM disease and DEFINITION criteria #### 
#define what is a true bifurcation and what is not
table(data$medina_class)
data <- data %>% 
  mutate(medina = ifelse(medina_class == "1-1-1" | 
                           medina_class == "1,1,1", 1, #true bif lesion
                         ifelse(medina_class == "0-1-1" | 
                           medina_class == "0,1,1", 2, #true bif lesion
                         ifelse(medina_class == "1-0-1" | 
                           medina_class == "1,0,1", 3, #true bif lesion
                         ifelse(medina_class == "1-1-0" | 
                           medina_class == "1,1,0", 4, #main branch only
                         ifelse(medina_class == "0,1,0" | 
                           medina_class == "0-1-0", 5, #main branch only
                         ifelse(medina_class == "1,0,0" | 
                           medina_class == "1-0-0", 6, #main branch only
                         ifelse(medina_class == "0,0,1" | 
                           medina_class == "0-0-1", 7, #side branch only 
                           999)))))))) %>% 
  relocate(medina, .after=medina_class)

subset(data$main_branch.factor, data$patient_id == 102688736)
subset(data$side_branch.factor, data$patient_id == 102688736)
data$medina[data$patient_id == 102688736] <- 1 #impute the missing data

subset(data$main_branch.factor, data$patient_id == 106922882)
subset(data$side_branch.factor, data$patient_id == 106922882)
data$medina[data$patient_id == 106922882] <- 1 
 
#convert medina to factor
data$medina = factor(data$medina)
levels(data$medina)=c("1,1,1", "0,1,1", "1,0,1", "1,1,0", "0,1,0", "1,0,0", 
                      "0,0,1")
label(data$medina) ="Medina classification"

#identify true bifurcational lesions 
data <- data %>% 
  mutate(bif_true = ifelse(medina == "1,1,1" | 
                             medina == "0,1,1" | 
                             medina == "1,0,1", 
                           1, 0)) %>% 
  relocate(bif_true, .after=medina)
#table(data$medina, data$bif_true) #data validity check 

#which lesions involve the left main? 
table(data$main_branch.factor)
table(data$side_branch.factor)
table(data$main_branch.factor, data$side_branch.factor)

data <- data %>% 
  mutate(bif_LM = ifelse((main_branch.factor == "Left Main to LAD" & side_branch.factor == "Circumflex") | 
                           (main_branch.factor == "Left Main to LAD" & side_branch.factor == "OM") |
                           (main_branch.factor == "Left Main to LCX" & side_branch.factor == "LAD") |
                           (main_branch.factor == "Left Main to LCX" & side_branch.factor == "Diagonal"), 
                         1, 0)) %>% 
  relocate(bif_LM, .after=side_branch.factor)
label(data$bif_LM) ="LM bifurcation disease"
#validity checks
#table(data$main_branch.factor, data$bif_LM)
#table(data$medina, data$bif_LM)

#how many patients with LM disease have a true bif lesion? 
data <- data %>% 
  mutate(bif_trueLM = ifelse(bif_LM == 1 & bif_true == 1, 1, 0)) %>% 
  relocate(bif_trueLM, .after = bif_LM)
label(data$bif_trueLM)="true LM bifurcation disease"

#subanalsys 1 results: what are the outcomes in patients with true LM bif dz? 
paste("outcomes at 1yr in patients with true LM bifurcation lesions")
paste0("1yr composite: ", 
       sum(subset(data$outcome_1yr, !is.na(data$outcome_1yr) & data$bif_trueLM == 1)), " (",
       round(mean(subset(data$outcome_1yr, !is.na(data$outcome_1yr) & data$bif_trueLM == 1))*100, 2), "%)")

paste0("1yr TLR: ", 
       sum(subset(data$TLR_1yr, !is.na(data$TLR_1yr) & data$bif_trueLM == 1)), " (",
       round(mean(subset(data$TLR_1yr, !is.na(data$TLR_1yr) & data$bif_trueLM == 1))*100, 2), "%)")

paste0("1yr TLMI: ", 
       sum(subset(data$TLMI_1yr, !is.na(data$TLMI_1yr) & data$bif_trueLM == 1)), " (",
       round(mean(subset(data$TLMI_1yr, !is.na(data$TLMI_1yr) & data$bif_trueLM == 1))*100, 2), "%)")

paste0("1yr cardiac death: ", 
       sum(subset(data$cdeath_1yr, !is.na(data$cdeath_1yr) & data$bif_trueLM == 1)), " (",
       round(mean(subset(data$cdeath_1yr, !is.na(data$cdeath_1yr) & data$bif_trueLM == 1))*100, 2), "%)")


#subanalsys 1 results: what are the outcomes in patients with true bif dz?                           
paste("outcomes at 1yr in patients with true bifurcation lesions")
paste0("1yr composite: ", 
       sum(subset(data$outcome_1yr, !is.na(data$outcome_1yr) & data$bif_true == 1)), " (",
       round(mean(subset(data$outcome_1yr, !is.na(data$outcome_1yr) & data$bif_true == 1))*100, 2), "%)")

paste0("1yr TLR: ", 
       sum(subset(data$TLR_1yr, !is.na(data$TLR_1yr) & data$bif_true == 1)), " (",
       round(mean(subset(data$TLR_1yr, !is.na(data$TLR_1yr) & data$bif_true == 1))*100, 2), "%)")

paste0("1yr TLMI: ", 
       sum(subset(data$TLMI_1yr, !is.na(data$TLMI_1yr) & data$bif_true == 1)), " (",
       round(mean(subset(data$TLMI_1yr, !is.na(data$TLMI_1yr) & data$bif_true == 1))*100, 2), "%)")

paste0("1yr cardiac death: ", 
       sum(subset(data$cdeath_1yr, !is.na(data$cdeath_1yr) & data$bif_true == 1)), " (",
       round(mean(subset(data$cdeath_1yr, !is.na(data$cdeath_1yr) & data$bif_true == 1))*100, 2), "%)")


#subanalsys 1 results: what are the outcomes in patients without true bif dz?                           
paste("outcomes at 1 and 3yrs in patients without true bifurcation lesions")
paste0("1yr composite: ", round(mean(subset(data$outcome_1yr, !is.na(data$outcome_1yr) & data$bif_true == 0))*100, 2), "%")
paste0("3yr composite: ", round(mean(subset(data$outcome_3yr, !is.na(data$outcome_3yr) & data$bif_true == 0))*100, 2), "%")
paste0("1yr TLR: ", round(mean(subset(data$TLR_1yr, !is.na(data$TLR_1yr) & data$bif_true == 0))*100, 2), "%")
paste0("3yr TLR: ", round(mean(subset(data$TLR_3yr, !is.na(data$TLR_3yr) & data$bif_true == 0))*100, 2), "%")
paste0("1yr TLMI: ", round(mean(subset(data$TLMI_1yr, !is.na(data$TLMI_1yr) & data$bif_true == 0))*100, 2), "%")
paste0("3yr TLMI: ", round(mean(subset(data$TLMI_3yr, !is.na(data$TLMI_3yr) & data$bif_true == 0))*100, 2), "%")
paste0("1yr cardiac death: ", round(mean(subset(data$cdeath_1yr, !is.na(data$cdeath_1yr) & data$bif_true == 0))*100, 2), "%")
paste0("3yr cardiac death: ", round(mean(subset(data$cdeath_3yr, !is.na(data$cdeath_3yr) & data$bif_true == 0))*100, 2), "%")

####create table 1#### 
data <- subset(data, !is.na(data$outcome_1yr))
#N 
paste("N")
paste0("allcomers: ", 
       sum(!is.na(data$outcome_1yr)))
paste0("true bifurcation: ", 
       sum(!is.na(data$outcome_1yr) & data$bif_true == 1))
paste0("true LM bifurcation: ", 
       sum(!is.na(data$outcome_1yr) & data$bif_trueLM == 1))
paste0("non-true bifurcation: ", 
       sum(!is.na(data$outcome_1yr) & data$bif_true == 0))
#age 
paste("demographics")
paste0("allcomers: age mean/sd: ", 
       round(mean(data$age), 2), 
       "/", 
       round(sd(data$age), 2))
paste0("true bifurcation: age mean/sd: ", 
       round(mean(subset(data$age, data$bif_true == 1)), 2), 
       "/", 
       round(sd(subset(data$age, data$bif_true == 1)), 2))
paste0("true LM bifurcation: age mean/sd: ", 
       round(mean(subset(data$age, data$bif_trueLM == 1)), 2), 
       "/", 
       round(sd(subset(data$age, data$bif_trueLM == 1)), 2))
paste0("without true bifurcation: age mean/sd: ", 
       round(mean(subset(data$age, data$bif_true == 0)), 2), 
       "/", 
       round(sd(subset(data$age, data$bif_true == 0)), 2))
#gender: % who are male 
paste0("allcomers: #(%) male: ", 
       sum(data$sex.factor=="Male"), " (",
       round(mean(data$gender)*100, 2), "%)")
paste0("true bifurcation: #(%) male: ", 
       sum(data$sex.factor=="Male" &  data$bif_true == 1), " (",
       round(mean(subset(data$gender, data$bif_true == 1))*100, 2), "%)")
paste0("true LM bifurcation: #(%) male: ", 
       sum(data$sex.factor=="Male" &  data$bif_trueLM == 1), " (",
       round(mean(subset(data$gender, data$bif_trueLM == 1))*100, 2), "%)")
paste0("without true bifurcation: #(%) male: ", 
       sum(data$sex.factor=="Male" &  data$bif_true == 0), " (",
       round(mean(subset(data$gender, data$bif_true == 0))*100, 2), "%)")
#BMI
paste0("allcomers: bmi mean/sd: ", 
       round(mean(data$bmi), 2), 
       "/", 
       round(sd(data$bmi), 2))
paste0("true bifurcation: bmi mean/sd: ", 
       round(mean(subset(data$bmi, data$bif_true == 1)), 2), 
       "/", 
       round(sd(subset(data$bmi, data$bif_true == 1)), 2))
paste0("true LM bifurcation: bmi mean/sd: ", 
       round(mean(subset(data$bmi, data$bif_trueLM == 1)), 2), 
       "/", 
       round(sd(subset(data$bmi, data$bif_trueLM == 1)), 2))
paste0("without true bifurcation: bmi mean/sd: ", 
       round(mean(subset(data$bmi, data$bif_trueLM == 0)), 2), 
       "/", 
       round(sd(subset(data$bmi, data$bif_trueLM == 0)), 2))
#smoking 
#allcomers
paste0("allcomers: non-smoker, #(%): ", 
       sum(data$smoker.factor=="Non-Smoker", na.rm = TRUE), " (", 
       round(mean(data$smoker.factor=="Non-Smoker", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: current, #(%): ", 
       sum(data$smoker.factor=="Current", na.rm = TRUE), " (", 
       round(mean(data$smoker.factor=="Current", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: prior, #(%): ", 
       sum(data$smoker.factor=="Prior", na.rm = TRUE), " (", 
       round(mean(data$smoker.factor=="Prior", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: non-smoker, #(%): ", 
       sum(data$smoker.factor=="Non-Smoker" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_true==1) == "Non-Smoker", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: current, #(%): ", 
       sum(data$smoker.factor=="Current" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_true==1) == "Current", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: prior, #(%): ", 
       sum(data$smoker.factor=="Prior" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_true==1) == "Prior", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: non-smoker, #(%): ", 
       sum(data$smoker.factor=="Non-Smoker" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_trueLM==1) == "Non-Smoker", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: current, #(%): ", 
       sum(data$smoker.factor=="Current" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_trueLM==1) == "Current", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: prior, #(%): ",
       sum(data$smoker.factor=="Prior" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_trueLM==1) == "Prior", na.rm=TRUE)*100, 2), 
       "%)")
#non-true bifurcation
paste0("without true bifurcation: non-smoker, #(%): ", 
       sum(data$smoker.factor=="Non-Smoker" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_true==0) == "Non-Smoker", na.rm=TRUE)*100, 2),  
       "%)")
paste0("without true bifurcation: current, #(%): ", 
       sum(data$smoker.factor=="Current" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_true==0) == "Current", na.rm=TRUE)*100, 2), 
       "%)")
paste0("without true bifurcation: prior, #(%): ", 
       sum(data$smoker.factor=="Prior" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$smoker.factor, data$bif_true==0) == "Prior", na.rm=TRUE)*100, 2), 
       "%)")
#hypertension
paste0("allcomers: hypertension #(%): ", 
       sum(data$hypertension.factor=="Yes"), " (",
       round(mean(data$hypertension.factor=="Yes")*100, 2), "%)")
paste0("true bifurcation: hypertension #(%): ", 
       sum(data$hypertension.factor=="Yes" &  data$bif_true == 1), " (",
       round(mean(subset(data$hypertension.factor, data$bif_true == 1) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: hypertension #(%): ", 
       sum(data$hypertension.factor=="Yes" &  data$bif_trueLM == 1), " (",
       round(mean(subset(data$hypertension.factor, data$bif_trueLM == 1) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: hypertension #(%): ", 
       sum(data$hypertension.factor=="Yes" &  data$bif_true == 0), " (",
       round(mean(subset(data$hypertension.factor, data$bif_true == 0) == "Yes")*100, 2), "%)")
#hyperlipidemia
paste0("allcomers: dyslipidemia #(%): ", 
       sum(data$dyslipidemia.factor=="Yes"), " (",
       round(mean(data$dyslipidemia.factor=="Yes")*100, 2), "%)")
paste0("true bifurcation: dyslipidemia #(%): ", 
       sum(data$dyslipidemia.factor=="Yes" &  data$bif_true == 1), " (",
       round(mean(subset(data$dyslipidemia.factor, data$bif_true == 1) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: dyslipidemia #(%): ", 
       sum(data$dyslipidemia.factor=="Yes" &  data$bif_trueLM == 1), " (",
       round(mean(subset(data$dyslipidemia.factor, data$bif_trueLM == 1) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: dyslipidemia #(%): ", 
       sum(data$dyslipidemia.factor=="Yes" &  data$bif_true == 0), " (",
       round(mean(subset(data$dyslipidemia.factor, data$bif_true == 0) == "Yes")*100, 2), "%)")
#diabetes
paste0("allcomers: diabetes #(%): ", 
       sum(data$diabetes.factor=="Yes"), " (",
       round(mean(data$diabetes.factor=="Yes")*100, 2), "%)")
paste0("true bifurcation: diabetes #(%): ", 
       sum(data$diabetes.factor=="Yes" &  data$bif_true == 1), " (",
       round(mean(subset(data$diabetes.factor, data$bif_true == 1) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: diabetes #(%): ", 
       sum(data$diabetes.factor=="Yes" &  data$bif_trueLM == 1), " (",
       round(mean(subset(data$diabetes.factor, data$bif_trueLM == 1) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: diabetes #(%): ", 
       sum(data$diabetes.factor=="Yes" &  data$bif_true == 0), " (",
       round(mean(subset(data$diabetes.factor, data$bif_true == 0) == "Yes")*100, 2), "%)")
#insulin use 
paste0("allcomers: insulin #: ", 
       sum(data$diabetes_management.factor=="Insulin", na.rm=TRUE))
paste0("true bifurcation: insulin #: ", 
       sum(data$diabetes_management.factor=="Insulin" &  data$bif_true == 1, na.rm=TRUE))
paste0("true LM bifurcation: insulin #: ", 
       sum(data$diabetes_management.factor=="Insulin" &  data$bif_trueLM == 1, na.rm=TRUE))
paste0("without true bifurcation: dinsulin #: ", 
       sum(data$diabetes_management.factor=="Insulin" &  data$bif_true == 0, na.rm=TRUE))
#CKD 
#allcomers
paste0("allcomers: no renal disease, #(%): ", 
       sum(data$kidney_disease.factor=="None", na.rm = TRUE), " (", 
       round(mean(data$kidney_disease.factor=="None", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: CKD, #(%): ", 
       sum(data$kidney_disease.factor=="CKD", na.rm = TRUE), " (", 
       round(mean(data$kidney_disease.factor=="CKD", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: ESRD, #(%): ", 
       sum(data$kidney_disease.factor=="ESRD", na.rm = TRUE), " (", 
       round(mean(data$kidney_disease.factor=="ESRD", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: no renal disease, #(%): ", 
       sum(data$kidney_disease.factor=="None" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_true==1) == "None", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: CKD, #(%): ", 
       sum(data$kidney_disease.factor=="CKD" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_true==1) == "CKD", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: ESRD, #(%): ", 
       sum(data$kidney_disease.factor=="ESRD" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_true==1) == "ESRD", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: no renal disease, #(%): ", 
       sum(data$kidney_disease.factor=="None" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_trueLM==1) == "None", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: CKD, #(%): ", 
       sum(data$kidney_disease.factor=="CKD" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_trueLM==1) == "CKD", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: ESRD, #(%): ",
       sum(data$kidney_disease.factor=="ESRD" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_trueLM==1) == "ESRD", na.rm=TRUE)*100, 2), 
       "%)")
#non-true bifurcation
paste0("without true bifurcation: no renal disease, #(%): ", 
       sum(data$kidney_disease.factor=="None" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_true==0) == "None", na.rm=TRUE)*100, 2),  
       "%)")
paste0("without true bifurcation: CKD, #(%): ", 
       sum(data$kidney_disease.factor=="CKD" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_true==0) == "CKD", na.rm=TRUE)*100, 2), 
       "%)")
paste0("without true bifurcation: ESRD, #(%): ", 
       sum(data$kidney_disease.factor=="ESRD" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$kidney_disease.factor, data$bif_true==0) == "ESRD", na.rm=TRUE)*100, 2), 
       "%)")
#COPD 
paste0("allcomers: COPD #(%): ", 
       sum(data$copd.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$copd.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: COPD #(%): ", 
       sum(data$copd.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$copd.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: COPD #(%): ", 
       sum(data$copd.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$copd.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: COPD #(%): ", 
       sum(data$copd.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$copd.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#family history of CAD 
paste0("allcomers: fam Hx of CAD #(%): ", 
       sum(data$family_history_of_cad.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$family_history_of_cad.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: fam Hx of CAD #(%): ", 
       sum(data$family_history_of_cad.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$family_history_of_cad.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: fam Hx of CAD #(%): ", 
       sum(data$family_history_of_cad.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$family_history_of_cad.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: fam Hx of CAD #(%): ", 
       sum(data$family_history_of_cad.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$family_history_of_cad.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#prior CAD
paste0("allcomers: prior CAD #(%): ", 
       sum(data$prior_cad.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$prior_cad.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: prior CAD #(%): ", 
       sum(data$prior_cad.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cad.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: prior CAD #(%): ", 
       sum(data$prior_cad.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cad.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: prior CAD #(%): ", 
       sum(data$prior_cad.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cad.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#prior PAD
paste0("allcomers: prior PAD #(%): ", 
       sum(data$prior_pad.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$prior_pad.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: prior PAD #(%): ", 
       sum(data$prior_pad.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_pad.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: prior PAD #(%): ", 
       sum(data$prior_pad.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_pad.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: prior PAD #(%): ", 
       sum(data$prior_pad.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$prior_pad.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#prior MI
paste0("allcomers: prior MI #(%): ", 
       sum(data$prior_mi.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$prior_mi.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: prior MI #(%): ", 
       sum(data$prior_mi.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_mi.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: prior MI #(%): ", 
       sum(data$prior_mi.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_mi.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: prior MI #(%): ", 
       sum(data$prior_mi.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$prior_mi.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#prior HF
paste0("allcomers: prior HF #(%): ", 
       sum(data$prior_heart_failure.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$prior_heart_failure.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: prior HF #(%): ", 
       sum(data$prior_heart_failure.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_heart_failure.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: prior HF #(%): ", 
       sum(data$prior_heart_failure.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_heart_failure.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: prior HF #(%): ", 
       sum(data$prior_heart_failure.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$prior_heart_failure.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#ejection fraction in patients with heart failure
summary(subset(data$estimated_ejection_fractio, data$bif_true == 1)) 
summary(subset(data$estimated_ejection_fractio, data$bif_trueLM == 1))
#prior valve surgery
paste0("allcomers: prior valve surgery #(%): ", 
       sum(data$prior_valve_surgery.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$prior_valve_surgery.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: prior valve surgery #(%): ", 
       sum(data$prior_valve_surgery.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_valve_surgery.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: prior valve surgery #(%): ", 
       sum(data$prior_valve_surgery.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_valve_surgery.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: prior valve surgery #(%): ", 
       sum(data$prior_valve_surgery.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$prior_valve_surgery.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#prior PCI 
paste0("allcomers: prior PCI #(%): ", 
       sum(data$prior_pci.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$prior_pci.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: prior PCI #(%): ", 
       sum(data$prior_pci.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_pci.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: prior PCI #(%): ", 
       sum(data$prior_pci.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_pci.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: prior PCI #(%): ", 
       sum(data$prior_pci.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$prior_pci.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#prior CABG 
paste0("allcomers: prior CABG #(%): ", 
       sum(data$prior_cabg.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$prior_cabg.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: prior CABG #(%): ", 
       sum(data$prior_cabg.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cabg.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: prior CABG #(%): ", 
       sum(data$prior_cabg.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cabg.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: prior CABG #(%): ", 
       sum(data$prior_cabg.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cabg.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#prior cardiac arrest 
paste0("allcomers: prior cardiac arrest #(%): ", 
       sum(data$prior_cardiac_arrest.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$prior_cardiac_arrest.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: prior cardiac arrest #(%): ", 
       sum(data$prior_cardiac_arrest.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cardiac_arrest.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: prior cardiac arrest #(%): ", 
       sum(data$prior_cardiac_arrest.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cardiac_arrest.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: prior cardiac arrest #(%): ", 
       sum(data$prior_cardiac_arrest.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$prior_cardiac_arrest.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")
#symptoms at presentation
#allcomers
paste0("allcomers: Asymptomatic, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Asymptomatic", na.rm = TRUE), " (", 
       round(mean(data$symptoms_at_presentation.factor=="Asymptomatic", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: Stable Angina, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Stable Angina", na.rm = TRUE), " (", 
       round(mean(data$symptoms_at_presentation.factor=="Stable Angina", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: Unstable Angina, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Unstable Angina", na.rm = TRUE), " (", 
       round(mean(data$symptoms_at_presentation.factor=="Unstable Angina", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: NSTEMI, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="NSTEMI", na.rm = TRUE), " (", 
       round(mean(data$symptoms_at_presentation.factor=="NSTEMI", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: STEMI, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="STEMI", na.rm = TRUE), " (", 
       round(mean(data$symptoms_at_presentation.factor=="STEMI", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: Asymptomatic, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Asymptomatic" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==1) == "Asymptomatic", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: Stable Angina, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Stable Angina" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==1) == "Stable Angina", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: Unstable Angina, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Unstable Angina" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==1) == "Unstable Angina", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: NSTEMI, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="NSTEMI" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==1) == "NSTEMI", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: STEMI, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="STEMI" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==1) == "STEMI", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: Asymptomatic, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Asymptomatic" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_trueLM==1) == "Asymptomatic", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: Stable Angina, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Stable Angina" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_trueLM==1) == "Stable Angina", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: Unstable Angina, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Unstable Angina" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_trueLM==1) == "Unstable Angina", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: NSTEMI, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="NSTEMI" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_trueLM==1) == "NSTEMI", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: STEMI, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="STEMI" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_trueLM==1) == "STEMI", na.rm=TRUE)*100, 2), 
       "%)")
#non-true bifurcation 
paste0("without true bifurcation: Asymptomatic, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Asymptomatic" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==0) == "Asymptomatic", na.rm=TRUE)*100, 2),  
       "%)")
paste0("without true bifurcation: Stable Angina, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Stable Angina" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==0) == "Stable Angina", na.rm=TRUE)*100, 2), 
       "%)")
paste0("without true bifurcation: Unstable Angina, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="Unstable Angina" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==0) == "Unstable Angina", na.rm=TRUE)*100, 2), 
       "%)")
paste0("without true bifurcation: NSTEMI, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="NSTEMI" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==0) == "NSTEMI", na.rm=TRUE)*100, 2), 
       "%)")
paste0("without true bifurcation: STEMI, #(%): ", 
       sum(data$symptoms_at_presentation.factor=="STEMI" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$symptoms_at_presentation.factor, data$bif_true==0) == "STEMI", na.rm=TRUE)*100, 2), 
       "%)")
#recnet HFH (within 2 wks)
paste0("allcomers: HFH within 2 weeks #(%): ", 
       sum(data$heart_failure_2_weeks_prio.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$heart_failure_2_weeks_prio.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: HFH within 2 weeks #(%): ", 
       sum(data$heart_failure_2_weeks_prio.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$heart_failure_2_weeks_prio.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: HFH within 2 weeks #(%): ", 
       sum(data$heart_failure_2_weeks_prio.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$heart_failure_2_weeks_prio.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: HFH within 2 weeks #(%): ", 
       sum(data$heart_failure_2_weeks_prio.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$heart_failure_2_weeks_prio.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")


####table 2: procedural/lesion characteristics#### 
#medina classification 
#true bifurcation 
paste0("true bifurcation: medina 1,1,1, #(%): ", 
       sum(data$medina=="1,1,1" & data$bif_true==1 & !is.na(data$outcome_1yr), na.rm=TRUE), 
       " (", 
       round(mean(subset(data$medina, data$bif_true==1 & !is.na(data$outcome_1yr)) == "1,1,1", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: medina 0,1,1, #(%): ", 
       sum(data$medina=="0,1,1" & data$bif_true==1 & !is.na(data$outcome_1yr), na.rm=TRUE), 
       " (", 
       round(mean(subset(data$medina, data$bif_true==1 & !is.na(data$outcome_1yr)) == "0,1,1", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: medina 1,0,1, #(%): ", 
       sum(data$medina=="1,0,1" & data$bif_true==1 & !is.na(data$outcome_1yr), na.rm=TRUE), 
       " (", 
       round(mean(subset(data$medina, data$bif_true==1 & !is.na(data$outcome_1yr)) == "1,0,1", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation 
paste0("true LM bifurcation: medina 1,1,1, #(%): ", 
       sum(data$medina=="1,1,1" & data$bif_trueLM==1 & !is.na(data$outcome_1yr), na.rm=TRUE), 
       " (", 
       round(mean(subset(data$medina, data$bif_trueLM==1 & !is.na(data$outcome_1yr)) == "1,1,1", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: medina 0,1,1, #(%): ", 
       sum(data$medina=="0,1,1" & data$bif_trueLM==1 & !is.na(data$outcome_1yr), na.rm=TRUE), 
       " (", 
       round(mean(subset(data$medina, data$bif_trueLM==1 & !is.na(data$outcome_1yr)) == "0,1,1", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: medina 1,0,1, #(%): ", 
       sum(data$medina=="1,0,1" & data$bif_trueLM==1 & !is.na(data$outcome_1yr), na.rm=TRUE), 
       " (", 
       round(mean(subset(data$medina, data$bif_trueLM==1 & !is.na(data$outcome_1yr)) == "1,0,1", na.rm=TRUE)*100, 2), 
       "%)")


table(data$medina)

#Access 
#allcomers
paste0("allcomers: femoral, #(%): ", 
       sum(data$access.factor=="Femoral", na.rm = TRUE), " (", 
       round(mean(data$access.factor=="Femoral", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: radial, #(%): ", 
       sum(data$access.factor=="Radial", na.rm = TRUE), " (", 
       round(mean(data$access.factor=="Radial", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: femoral, #(%): ", 
       sum(data$access.factor=="Femoral" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access.factor, data$bif_true==1) == "Femoral", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: radial, #(%): ", 
       sum(data$access.factor=="Radial" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access.factor, data$bif_true==1) == "Radial", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: femoral, #(%): ", 
       sum(data$access.factor=="Femoral" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access.factor, data$bif_trueLM==1) == "Femoral", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: radial, #(%): ", 
       sum(data$access.factor=="Radial" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access.factor, data$bif_trueLM==1) == "Radial", na.rm=TRUE)*100, 2), 
       "%)")
#non-true bifurcation
paste0("true bifurcation: femoral, #(%): ", 
       sum(data$access.factor=="Femoral" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access.factor, data$bif_true==0) == "Femoral", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: radial, #(%): ", 
       sum(data$access.factor=="Radial" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access.factor, data$bif_true==0) == "Radial", na.rm=TRUE)*100, 2), 
       "%)")

#syntax score categories 
#allcomers
paste0("allcomers: low (<16), #(%): ", 
       sum(data$syntax_score.factor=="Low (< 16)", na.rm = TRUE), " (", 
       round(mean(data$syntax_score.factor=="Low (< 16)", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: intermediate (17-22), #(%): ", 
       sum(data$syntax_score.factor=="Intermediate (17-22)", na.rm = TRUE), " (", 
       round(mean(data$syntax_score.factor=="Intermediate (17-22)", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: high (>22), #(%): ", 
       sum(data$syntax_score.factor=="High (>22)", na.rm = TRUE), " (", 
       round(mean(data$syntax_score.factor=="High (>22)", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: low (<16), #(%): ", 
       sum(data$syntax_score.factor=="Low (< 16)" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_true==1) == "Low (< 16)", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: Intermediate (17-22), #(%): ", 
       sum(data$syntax_score.factor=="Intermediate (17-22)" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_true==1) == "Intermediate (17-22)", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: High (>22), #(%): ", 
       sum(data$syntax_score.factor=="High (>22)" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_true==1) == "High (>22)", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: low (<16), #(%): ", 
       sum(data$syntax_score.factor=="Low (< 16)" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_trueLM==1) == "Low (< 16)", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: Intermediate (17-22), #(%): ", 
       sum(data$syntax_score.factor=="Intermediate (17-22)" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_trueLM==1) == "Intermediate (17-22)", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: High (>22), #(%): ", 
       sum(data$syntax_score.factor=="High (>22)" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_trueLM==1) == "High (>22)", na.rm=TRUE)*100, 2), 
       "%)")
#non-true bifurcation
paste0("non-true bifurcation: low (<16), #(%): ", 
       sum(data$syntax_score.factor=="Low (< 16)" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_true==0) == "Low (< 16)", na.rm=TRUE)*100, 2),  
       "%)")
paste0("non-true bifurcation: Intermediate (17-22), #(%): ", 
       sum(data$syntax_score.factor=="Intermediate (17-22)" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_true==0) == "Intermediate (17-22)", na.rm=TRUE)*100, 2), 
       "%)")
paste0("non-true bifurcation: High (>22), #(%): ", 
       sum(data$syntax_score.factor=="High (>22)" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$syntax_score.factor, data$bif_true==0) == "High (>22)", na.rm=TRUE)*100, 2), 
       "%)")

#guide size 
#allcomers
paste0("allcomers: 6Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="6", na.rm = TRUE), " (", 
       round(mean(data$guide_size_fr.factor=="6", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: 7Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="7", na.rm = TRUE), " (", 
       round(mean(data$guide_size_fr.factor=="7", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: 8Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="8", na.rm = TRUE), " (", 
       round(mean(data$guide_size_fr.factor=="8", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: 6Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="6" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_true==1) == "6", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: 7Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="7" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_true==1) == "7", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: 8Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="8" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_true==1) == "8", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: 6Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="6" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_trueLM==1) == "6", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: 7Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="7" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_trueLM==1) == "7", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: 8Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="8" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_trueLM==1) == "8", na.rm=TRUE)*100, 2), 
       "%)")
#non-true bifurcation
paste0("non-true bifurcation: 6Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="6" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_true==0) == "6", na.rm=TRUE)*100, 2),  
       "%)")
paste0("non-true bifurcation: 7Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="7" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_true==0) == "7", na.rm=TRUE)*100, 2), 
       "%)")
paste0("non-true bifurcation: 8Fr, #(%): ", 
       sum(data$guide_size_fr.factor=="8" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$guide_size_fr.factor, data$bif_true==0) == "8", na.rm=TRUE)*100, 2), 
       "%)")

#procedural anticoagulation
#allcomers
paste0("allcomers: Heparin, #(%): ", 
       sum(data$procedural_anticoagulation.factor=="Heparin", na.rm = TRUE), " (", 
       round(mean(data$procedural_anticoagulation.factor=="Heparin", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: Bivalirudin, #(%): ", 
       sum(data$procedural_anticoagulation.factor=="Bivalirudin", na.rm = TRUE), " (", 
       round(mean(data$procedural_anticoagulation.factor=="Bivalirudin", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: Heparin, #(%): ", 
       sum(data$procedural_anticoagulation.factor=="Heparin" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$procedural_anticoagulation.factor, data$bif_true==1) == "Heparin", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: Bivalirudin, #(%): ", 
       sum(data$procedural_anticoagulation.factor=="Bivalirudin" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$procedural_anticoagulation.factor, data$bif_true==1) == "Bivalirudin", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: Heparin, #(%): ", 
       sum(data$procedural_anticoagulation.factor=="Heparin" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$procedural_anticoagulation.factor, data$bif_trueLM==1) == "Heparin", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: Bivalirudin, #(%): ", 
       sum(data$procedural_anticoagulation.factor=="Bivalirudin" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$procedural_anticoagulation.factor, data$bif_trueLM==1) == "Bivalirudin", na.rm=TRUE)*100, 2), 
       "%)")
#non-true bifurcation
paste0("without true bifurcation: Heparin, #(%): ", 
       sum(data$procedural_anticoagulation.factor=="Heparin" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$procedural_anticoagulation.factor, data$bif_true==0) == "Heparin", na.rm=TRUE)*100, 2),  
       "%)")
paste0("without true bifurcation: Bivalirudin, #(%): ", 
       sum(data$procedural_anticoagulation.factor=="Bivalirudin" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$procedural_anticoagulation.factor, data$bif_true==0) == "Bivalirudin", na.rm=TRUE)*100, 2), 
       "%)")

#atherectomy or lithotripsy used 
paste0("allcomers: atherectomy or lithotripsy #(%): ", 
       sum(data$atherectomy_lithotripsy.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$atherectomy_lithotripsy.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: atherectomy or lithotripsy #(%): ", 
       sum(data$atherectomy_lithotripsy.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$atherectomy_lithotripsy.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: atherectomy or lithotripsy #(%): ", 
       sum(data$atherectomy_lithotripsy.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$atherectomy_lithotripsy.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: atherectomy or lithotripsy #(%): ", 
       sum(data$atherectomy_lithotripsy.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$atherectomy_lithotripsy.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")

#ISR intervention
paste0("allcomers: ISR intervention? #(%): ", 
       sum(data$intervention_for_isr.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$intervention_for_isr.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: ISR intervention? #(%): ", 
       sum(data$intervention_for_isr.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$intervention_for_isr.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: ISR intervention? #(%): ", 
       sum(data$intervention_for_isr.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$intervention_for_isr.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: ISR intervention? #(%): ", 
       sum(data$intervention_for_isr.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$intervention_for_isr.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")

#mechanical support?
paste0("allcomers: mechanical support? #(%): ", 
       sum(data$mechanical_support.factor=="Yes", na.rm=TRUE), " (",
       round(mean(data$mechanical_support.factor=="Yes", na.rm=TRUE)*100, 2), "%)")
paste0("true bifurcation: mechanical support? #(%): ", 
       sum(data$mechanical_support.factor=="Yes" &  data$bif_true == 1, na.rm=TRUE), " (",
       round(mean(subset(data$mechanical_support.factor, data$bif_true == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("true LM bifurcation: mechanical support? #(%): ", 
       sum(data$mechanical_support.factor=="Yes" &  data$bif_trueLM == 1, na.rm=TRUE), " (",
       round(mean(subset(data$mechanical_support.factor, data$bif_trueLM == 1, na.rm=TRUE) == "Yes")*100, 2), "%)")
paste0("without true bifurcation: mechanical support? #(%): ", 
       sum(data$mechanical_support.factor=="Yes" &  data$bif_true == 0, na.rm=TRUE), " (",
       round(mean(subset(data$mechanical_support.factor, data$bif_true == 0, na.rm=TRUE) == "Yes")*100, 2), "%)")

#main branch
#allcomers
paste0("allcomers: Left Main to LAD, #(%): ", 
       sum(data$main_branch.factor=="Left Main to LAD", na.rm = TRUE), " (", 
       round(mean(data$main_branch.factor=="Left Main to LAD", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: Left Main to LCX, #(%): ", 
       sum(data$main_branch.factor=="Left Main to LCX", na.rm = TRUE), " (", 
       round(mean(data$main_branch.factor=="Left Main to LCX", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: LAD, #(%): ", 
       sum(data$main_branch.factor=="LAD", na.rm = TRUE), " (", 
       round(mean(data$main_branch.factor=="LAD", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: LCX, #(%): ", 
       sum(data$main_branch.factor=="LCX", na.rm = TRUE), " (", 
       round(mean(data$main_branch.factor=="LCX", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: RCA, #(%): ", 
       sum(data$main_branch.factor=="RCA", na.rm = TRUE), " (", 
       round(mean(data$main_branch.factor=="RCA", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: Left Main to LAD, #(%): ", 
       sum(data$main_branch.factor=="Left Main to LAD" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==1) == "Left Main to LAD", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: Left Main to LCX, #(%): ", 
       sum(data$main_branch.factor=="Left Main to LCX" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==1) == "Left Main to LCX", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: LAD, #(%): ", 
       sum(data$main_branch.factor=="LAD" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==1) == "LAD", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: LCX, #(%): ", 
       sum(data$main_branch.factor=="LCX" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==1) == "LCX", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: RCA, #(%): ", 
       sum(data$main_branch.factor=="RCA" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==1) == "RCA", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: Left Main to LAD, #(%): ", 
       sum(data$main_branch.factor=="Left Main to LAD" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_trueLM==1) == "Left Main to LAD", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: Left Main to LCX, #(%): ", 
       sum(data$main_branch.factor=="Left Main to LCX" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_trueLM==1) == "Left Main to LCX", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: LAD, #(%): ", 
       sum(data$main_branch.factor=="LAD" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_trueLM==1) == "LAD", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: LCX, #(%): ", 
       sum(data$main_branch.factor=="LCX" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_trueLM==1) == "LCX", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: RCA, #(%): ", 
       sum(data$main_branch.factor=="RCA" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_trueLM==1) == "RCA", na.rm=TRUE)*100, 2), 
       "%)")
#non-true bifurcation
paste0("non-true bifurcation: Left Main to LAD, #(%): ", 
       sum(data$main_branch.factor=="Left Main to LAD" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==0) == "Left Main to LAD", na.rm=TRUE)*100, 2),  
       "%)")
paste0("non-true bifurcation: Left Main to LCX, #(%): ", 
       sum(data$main_branch.factor=="Left Main to LCX" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==0) == "Left Main to LCX", na.rm=TRUE)*100, 2), 
       "%)")
paste0("non-true bifurcation: LAD, #(%): ", 
       sum(data$main_branch.factor=="LAD" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==0) == "LAD", na.rm=TRUE)*100, 2), 
       "%)")
paste0("non-true bifurcation: LCX, #(%): ", 
       sum(data$main_branch.factor=="LCX" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==0) == "LCX", na.rm=TRUE)*100, 2), 
       "%)")
paste0("non-true bifurcation: RCA, #(%): ", 
       sum(data$main_branch.factor=="RCA" & data$bif_true==0, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$main_branch.factor, data$bif_true==0) == "RCA", na.rm=TRUE)*100, 2), 
       "%)")

#side branch
#allcomers
paste0("allcomers: LAD, #(%): ", 
       sum(data$side_branch.factor=="LAD", na.rm = TRUE), " (", 
       round(mean(data$side_branch.factor=="LAD", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: Circumflex, #(%): ", 
       sum(data$side_branch.factor=="Circumflex", na.rm = TRUE), " (", 
       round(mean(data$side_branch.factor=="Circumflex", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: Diagonal, #(%): ", 
       sum(data$side_branch.factor=="Diagonal", na.rm = TRUE), " (", 
       round(mean(data$side_branch.factor=="Diagonal", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: OM, #(%): ", 
       sum(data$side_branch.factor=="OM", na.rm = TRUE), " (", 
       round(mean(data$side_branch.factor=="OM", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: PDA, #(%): ", 
       sum(data$side_branch.factor=="PDA", na.rm = TRUE), " (", 
       round(mean(data$side_branch.factor=="PDA", na.rm = TRUE)*100,2), "%)")
paste0("allcomers: PLV, #(%): ", 
       sum(data$side_branch.factor=="PLV", na.rm = TRUE), " (", 
       round(mean(data$side_branch.factor=="PLV", na.rm = TRUE)*100,2), "%)")
#true bifurcation
paste0("true bifurcation: LAD, #(%): ", 
       sum(data$side_branch.factor=="LAD" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_true==1) == "LAD", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: Circumflex, #(%): ", 
       sum(data$side_branch.factor=="Circumflex" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_true==1) == "Circumflex", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: Diagonal, #(%): ", 
       sum(data$side_branch.factor=="Diagonal" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_true==1) == "Diagonal", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: OM, #(%): ", 
       sum(data$side_branch.factor=="OM" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_true==1) == "OM", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: PDA, #(%): ", 
       sum(data$side_branch.factor=="PDA" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_true==1) == "PDA", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: PLV, #(%): ", 
       sum(data$side_branch.factor=="PLV" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_true==1) == "PLV", na.rm=TRUE)*100, 2), 
       "%)")
#true LM bifurcation
paste0("true LM bifurcation: LAD, #(%): ", 
       sum(data$side_branch.factor=="LAD" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_trueLM==1) == "LAD", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: Circumflex, #(%): ", 
       sum(data$side_branch.factor=="Circumflex" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_trueLM==1) == "Circumflex", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: Diagonal, #(%): ", 
       sum(data$side_branch.factor=="Diagonal" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_trueLM==1) == "Diagonal", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: OM, #(%): ", 
       sum(data$side_branch.factor=="OM" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_trueLM==1) == "OM", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: PDA, #(%): ", 
       sum(data$side_branch.factor=="PDA" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_trueLM==1) == "PDA", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: PLV, #(%): ", 
       sum(data$side_branch.factor=="PLV" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$side_branch.factor, data$bif_trueLM==1) == "PLV", na.rm=TRUE)*100, 2), 
       "%)")

#main branch stent diameter 
data$main_branch_stent_size <- ifelse(data$main_branch_stent_size=="3.25x23", 
                                      "3.25", 
                               ifelse(data$main_branch_stent_size=="4.0 x 20 ", 
                                      "4.0", 
                                      data$main_branch_stent_size))
data$main_branch_stent_size <- as.numeric(data$main_branch_stent_size)
summary(subset(data$main_branch_stent_size, data$bif_true==1))
summary(subset(data$main_branch_stent_size, data$bif_trueLM==1))

#side branch stent diameter 
data$side_branch_stent_diameter <- ifelse(data$side_branch_stent_diameter=="2.25x12", 
                                          "2.25", 
                                   ifelse(data$side_branch_stent_diameter=="3.5 x 12 ", 
                                          "3.5", 
                                          data$side_branch_stent_diameter))
data$side_branch_stent_diameter <- as.numeric(data$side_branch_stent_diameter)
summary(subset(data$side_branch_stent_diameter, data$bif_true==1))
summary(subset(data$side_branch_stent_diameter, data$bif_trueLM==1))

#main branch pre-stenosis 
data$main_branch_pre_stenosis <- ifelse(data$main_branch_pre_stenosis=="60-70", 
                                      "65", 
                                 ifelse(data$main_branch_pre_stenosis=="80%", 
                                        "80", 
                                        data$main_branch_pre_stenosis))
data$main_branch_pre_stenosis <- as.numeric(data$main_branch_pre_stenosis)
summary(subset(data$main_branch_pre_stenosis, data$bif_true==1))
summary(subset(data$main_branch_pre_stenosis, data$bif_trueLM==1))

#side branch pre-stenosis 
data$side_branch_pre_stenosis <- ifelse(data$side_branch_pre_stenosis=="60-70", 
                                        "65", 
                                 ifelse(data$side_branch_pre_stenosis=="80-90", 
                                        "85", 
                                        data$side_branch_pre_stenosis))
data$side_branch_pre_stenosis <- as.numeric(data$side_branch_pre_stenosis)
summary(subset(data$side_branch_pre_stenosis, data$bif_true==1))
summary(subset(data$side_branch_pre_stenosis, data$bif_trueLM==1))

#closure
#true bifurcation
paste0("true bifurcation: closure - manual, #(%): ", 
       sum(data$access_closure.factor=="Manual/TR Band" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access_closure.factor, data$bif_true==1) == "Manual/TR Band", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true bifurcation: closure - angioseal, #(%): ", 
       sum(data$access_closure.factor=="Angioseal" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access_closure.factor, data$bif_true==1) == "Angioseal", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: closure - proglide, #(%): ", 
       sum(data$access_closure.factor=="ProGlide" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access_closure.factor, data$bif_true==1) == "ProGlide", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true bifurcation: closure - celt, #(%): ", 
       sum(data$access_closure.factor=="Celt" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access_closure.factor, data$bif_true==1) == "Celt", na.rm=TRUE)*100, 2), 
       "%)")
#LM 
paste0("true LM bifurcation: closure - manual, #(%): ", 
       sum(data$access_closure.factor=="Manual/TR Band" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access_closure.factor, data$bif_trueLM==1) == "Manual/TR Band", na.rm=TRUE)*100, 2),  
       "%)")
paste0("true LM bifurcation: closure - angioseal, #(%): ", 
       sum(data$access_closure.factor=="Angioseal" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access_closure.factor, data$bif_trueLM==1) == "Angioseal", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: closure - proglide, #(%): ", 
       sum(data$access_closure.factor=="ProGlide" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access_closure.factor, data$bif_trueLM==1) == "ProGlide", na.rm=TRUE)*100, 2), 
       "%)")
paste0("true LM bifurcation: closure - celt, #(%): ", 
       sum(data$access_closure.factor=="Celt" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$access_closure.factor, data$bif_trueLM==1) == "Celt", na.rm=TRUE)*100, 2), 
       "%)")

#intracoronary physiology used? 
#true bifurcation
paste0("true bifurcation: physiology, #(%): ", 
       sum(data$coronary_physiology.factor=="Yes" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$coronary_physiology.factor, data$bif_true==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")
#LM 
paste0("true LM bifurcation: physiology, #(%): ", 
       sum(data$coronary_physiology.factor=="Yes" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$coronary_physiology.factor, data$bif_trueLM==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")

#fluoro time 
summary(subset(data$fluoro_time, data$bif_true==1))
sd(subset(data$fluoro_time, data$bif_true==1), na.rm=TRUE)
summary(subset(data$fluoro_time, data$bif_trueLM==1))
sd(subset(data$fluoro_time, data$bif_trueLM==1), na.rm=TRUE)

#contrast
summary(subset(data$contrast_used, data$bif_true==1))
sd(subset(data$contrast_used, data$bif_true==1), na.rm=TRUE)
summary(subset(data$contrast_used, data$bif_trueLM==1))
sd(subset(data$contrast_used, data$bif_trueLM==1), na.rm=TRUE)

#coronary dissection 
#true bifurcation
paste0("true bifurcation: coronary dissection, #(%): ", 
       sum(data$coronary_dissection_from_i.factor=="Yes" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$coronary_dissection_from_i.factor, data$bif_true==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")
#LM 
paste0("true LM bifurcation: coronary dissection, #(%): ", 
       sum(data$coronary_dissection_from_i.factor=="Yes" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$coronary_dissection_from_i.factor, data$bif_trueLM==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")

#no reflow  
#true bifurcation
paste0("true bifurcation: no reflow, #(%): ", 
       sum(data$no_reflow.factor=="Yes" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$no_reflow.factor, data$bif_true==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")
#LM 
paste0("true LM bifurcation: no reflow, #(%): ", 
       sum(data$no_reflow.factor=="Yes" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$no_reflow.factor, data$bif_trueLM==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")

#post procedural bleeding  
#true bifurcation
paste0("true bifurcation: major bleeding, #(%): ", 
       sum(data$post_procedure_bleeding_co.factor=="Yes" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$post_procedure_bleeding_co.factor, data$bif_true==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")
#LM 
paste0("true LM bifurcation: major bleeding, #(%): ", 
       sum(data$post_procedure_bleeding_co.factor=="Yes" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$post_procedure_bleeding_co.factor, data$bif_trueLM==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")

#post procedural AKI  
#true bifurcation
paste0("true bifurcation: AKI, #(%): ", 
       sum(data$post_procedure_aki.factor=="Yes" & data$bif_true==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$post_procedure_aki.factor, data$bif_true==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")
#LM 
paste0("true LM bifurcation: AKI, #(%): ", 
       sum(data$post_procedure_aki.factor=="Yes" & data$bif_trueLM==1, na.rm=TRUE), 
       " (", 
       round(mean(subset(data$post_procedure_aki.factor, data$bif_trueLM==1) == "Yes", na.rm=TRUE)*100, 2),  
       "%)")

table(data$side_branch_occlusion.factor)

df <- subset(data, data$bif_true==1)
df2 <- subset(data, data$bif_trueLM==1)


####survival analysis####
km_fit <- survfit(Surv(censor_time2, outcome_1yr) ~ 1, data = df)
km_fitLM <- survfit(Surv(censor_time2, outcome_1yr) ~ 1, data = df2)
summary(km_fit, times = c(1,30,60,90*(1:10)))

#cumulative incidence curves 
par(mfrow = c(1, 2))
#all patients
plot(km_fit, fun = "event", 
     xlab="Days", xlim=c(0,365), 
     ylab="Patients", ylim=c(0,0.5), 
     lwd = 2, col = "blue", 
     break.time.by = 30, 
     risk.table=TRUE, 
     risk.table.height = 0.3, # Adjust risk table height
     risk.table.y.text.col = TRUE, # Color text in risk table
     risk.table.y.text = FALSE, 
     conf.int = FALSE,
     yaxt = "n", xaxt = "n", frame.plot = FALSE)  # Suppress default y-axis
polygon(c(km_fit$time, rev(km_fit$time)),  # X-coordinates for shading
        c(1 - km_fit$upper, rev(1 - km_fit$lower)),  # Y-coordinates for upper and lower CI
        col = adjustcolor("lightblue", alpha.f = 0.5),  # Light blue with transparency
        border = NA)  # No border
axis(1, at = seq(0, 365, by = 50)) #custom x-axis 
# Add custom y-axis with percentage labels
axis(2, at = seq(0, 0.5, by = 0.1), 
     labels = paste0(seq(0, 50, by = 10), "%"),  # Convert decimals to percentages
     las = 1) 
title("Composite Outcome in\nAll Patients")
box(bty = "l")

#LM patients 
plot(km_fitLM, fun = "event", 
     xlab="Days", xlim=c(0,365), 
     ylab="Patients", ylim=c(0,0.5), 
     lwd = 2, col = "red", 
     break.time.by = 30, 
     risk.table=TRUE, 
     risk.table.height = 0.3, # Adjust risk table height
     risk.table.y.text.col = TRUE, # Color text in risk table
     risk.table.y.text = FALSE, 
     conf.int = FALSE,
     yaxt = "n", xaxt = "n", frame.plot = FALSE)  # Suppress default y-axis
polygon(c(km_fitLM$time, rev(km_fitLM$time)),  # X-coordinates for shading
        c(1 - km_fitLM$upper, rev(1 - km_fitLM$lower)),  # Y-coordinates for upper and lower CI
        col = adjustcolor("pink", alpha.f = 0.5),  # Light red with transparency
        border = NA)  # No border
axis(1, at = seq(0, 365, by = 50)) #custom x-axis 
# Add custom y-axis with percentage labels
axis(2, at = seq(0, 0.5, by = 0.1), 
     labels = paste0(seq(0, 50, by = 10), "%"),  # Convert decimals to percentages
     las = 1) 
title("Composite Outcome in\nPatients with LM Disease")
box(bty = "l")


####build a PH model #### 
#trunate data to 365 day window
df_mod <- df
df_mod$censor_time2 <- ifelse(df_mod$censor_time2 > 365 & df_mod$outcome_1yr != 1, 
                           365, 
                           df_mod$censor_time2)
#set outcome to numeric 
df_mod$outcome_1yr_bi <- as.numeric(df_mod$outcome_1yr)

#set LM differentiator as factor 
df_mod$bif_trueLM.factor <- factor(df_mod$bif_trueLM, levels=c(0,1), labels=c("No", "Yes"))

#relabel Syntax score levels 
levels(df_mod$syntax_score.factor) <- c("Low", "Intermediate", "High")

#relevel variables
df_mod$diabetes.factor = relevel(df_mod$diabetes.factor, ref="No")
df_mod$acuity_of_case.factor = relevel(df_mod$acuity_of_case.factor, ref="Elective")
df_mod$mechanical_support.factor = relevel(df_mod$mechanical_support.factor, ref="No")
df_mod$atherectomy_lithotripsy.factor = relevel(df_mod$atherectomy_lithotripsy.factor, ref="No")
df_mod$intervention_for_isr.factor = relevel(df_mod$intervention_for_isr.factor, ref="No")

#label variable names
label(df_mod$age)="Age"
label(df_mod$sex.factor)="Sex"
label(df_mod$diabetes.factor)="Diabetes"
label(df_mod$syntax_score.factor)="Syntax Score"
label(df_mod$acuity_of_case.factor)="Case Acuity"
label(df_mod$bif_trueLM.factor)="LM Bifurcation Disease"
label(df_mod$mechanical_support.factor)="Mechanical Support"
label(df_mod$atherectomy_lithotripsy.factor)="Atherectomy/Lithotripsy"
label(df_mod$intervention_for_isr.factor)="ISR Intervention"
label(df_mod$main_branch_stent_size)="Main branch stent diameter"
label(df_mod$side_branch_stent_diameter)="Side branch stent diameter"

library(labelled)
labelled::var_label(df_mod) <- list(
  age = "Age (years)",
  sex.factor = "Sex",
  diabetes.factor = "Diabetes",
  syntax_score.factor = "Syntax Score",
  acuity_of_case.factor = "Case Acuity",
  bif_trueLM = "LM Bifurcation Disease",
  mechanical_support.factor = "Mechanical Support",
  atherectomy_lithotripsy.factor = "Atherectomy/Lithotripsy",
  intervention_for_isr.factor = "ISR Intervention",
  main_branch_stent_size = "Main Branch Stent Diameter (mm)",
  side_branch_stent_diameter = "Side Branch Stent Diameter (mm)"
)
table(df_mod$bif_trueLM.factor)
#estimate model
cox <- coxph(Surv(censor_time2, outcome_1yr_bi) ~ 
               age + sex.factor + diabetes.factor +  
               syntax_score.factor + acuity_of_case.factor + 
               bif_trueLM + mechanical_support.factor + 
               atherectomy_lithotripsy.factor + intervention_for_isr.factor + 
               main_branch_stent_size + side_branch_stent_diameter
               , data = df_mod)
summary(cox)

#testing assumptions
#PH test 
ph_test <- cox.zph(cox) 
ph_test #no red flags 
plot(ph_test) #plots look ok 

#create forest plot 
ggforest(cox, 
         main="",
         data=df_mod)

cox$xlevels <- unname(cox$xlevels)

cox_summary <- broom::tidy(cox, conf.int = TRUE)
cox_summary$term <- recode(cox_summary$term,
                           age = "Age (years)",
                           sex.factor = "Sex",
                           diabetes.factor = "Diabetes",
                           syntax_score.factor = "Syntax Score",
                           acuity_of_case.factor = "Case Acuity",
                           bif_trueLM = "LM Bifurcation Disease",
                           mechanical_support.factor = "Mechanical Support",
                           atherectomy_lithotripsy.factor = "Atherectomy/Lithotripsy",
                           intervention_for_isr.factor = "ISR Intervention",
                           main_branch_stent_size = "Main Branch Stent Diameter (mm)",
                           side_branch_stent_diameter = "Side Branch Stent Diameter (mm)")

aa_fit <-aareg(Surv(time, status) ~ trt + celltype +
                 karno + diagtime + age + prior , 
               data = vet)

aa_fit