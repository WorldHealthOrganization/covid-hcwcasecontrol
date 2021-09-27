####################################################################################################################################################
## Go.Data Cleaning Scripts 
# Includes messaging for overall missing and errorneous data - main purpose is to ensure that there are no errors in cleaning the data
####################################################################################################################################################

rm(list = ls())

setwd('~/Desktop/covid-hcwcasecontrol-main/')

# load raw data
## NOTE: check pathway is correct
dat = read.csv('~/Desktop/covid-hcwcasecontrol-main/rawdata.csv')

# load libraries and cleaning functions
path_to_functions <- here::here("~/Desktop/covid-hcwcasecontrol-main/scripts")
scripts_files <- dir(path_to_functions, pattern = ".R$", full.names=TRUE)
for (file in scripts_files) source(file, local = TRUE)

#########################################################################
### CLEAN QUESTIONNAIRE
##########################################################################

##############
## change dates 
##############
datecol = colnames(dat)[grep('date', colnames(dat))]
datecol = datecol[-which(datecol %in% c('hospitalization_date_known', 'outcome_date_known'))]
dat = clean_col(dat = dat, colnam = datecol, convertto = 'date', check = F)

dat = dat[!is.na(dat$init_inter_id),]

##############
## number of enrolment, main/ sensitivity/exclude
##############
dat$analysis_type = NA

## GET TOTAL 
nocasecontrol = table(dat$case_control, useNA = 'ifany')
noinitinterview = sum(is.na(dat$init_inter_id))

## GET CASE CONTROL by country and site
any(is.na(dat$country.godata))
table(dat$country.godata)
any(is.na(dat$site.godata))
table(dat$site.godata) # check strange spellings

################
#### CASES
################

######### SET RULES 
A = c('<2wk of initial interview','>2wk of initial interview','No','missing')
B = c('<2wk of initial interview','>2wk of initial interview', 'missing')
C = c('done','not done')

combs = expand.grid(initial_vxax = A, initial_viro = B, initial_interview = C, follow_up_interview=paste(C, collapse = '/'), analysis_type = NA)
combs = combs[order(combs$initial_vxax),]

combs$analysis_type[which(combs$initial_interview == 'not done')] = 'EXCLUDE'
combs$analysis_type[which(combs$initial_vxax == '>2wk of initial interview')] = 'EXCLUDE'
combs$analysis_type[which(combs$initial_vxax == '<2wk of initial interview' & combs$initial_viro == '<2wk of initial interview' & is.na(combs$analysis_type))] = 'MAIN'
combs$analysis_type[which(combs$initial_vxax == 'No' & combs$initial_viro == '<2wk of initial interview' & is.na(combs$analysis_type))] = 'MAIN'
combs$analysis_type[is.na(combs$analysis_type)] = 'SENSITIVITY'
#combs

initial_vxax = rep(NA, nrow(dat))
initial_vxax[dat$init_form_date - dat$init_vaccine_date  < 15] = '<2wk of initial interview'
initial_vxax[dat$init_form_date - dat$init_vaccine_date  > 14] = '>2wk of initial interview'
initial_vxax[dat$init_vaccine == 'NO'] = 'No'
initial_vxax[is.na(dat$init_vaccine)] = 'missing'
any(is.na(initial_vxax))

initial_viro = rep(NA, nrow(dat))
initial_viro[dat$init_form_date - dat$init_viro_collection_date  < 15] = '<2wk of initial interview'
initial_viro[dat$init_form_date - dat$init_viro_collection_date  > 14] = '>2wk of initial interview'
initial_viro[is.na(dat$init_viro_collection_date)] = 'missing'
any(is.na(initial_viro))
# confirmed with Jordan prior that all cases had pcr done within a week prior to 1st interview 
initial_viro[dat$country.godata == 'JORDAN' & dat$case_control == 'CASE'] = '<2wk of initial interview'

initial_interview = rep(NA, nrow(dat))
initial_interview[is.na(dat$init_inter_id)] = 'not done'
initial_interview[!is.na(dat$init_inter_id)] = 'done'
any(is.na(initial_interview))

for (i in dat$row.no[which(dat$case_control == 'CASE')]){
  
  pick = which(combs$initial_vxax %in% initial_vxax[which(dat$row.no == i)] & 
                 combs$initial_viro %in% initial_viro[which(dat$row.no == i)] & 
                 combs$initial_interview %in% initial_interview[which(dat$row.no == i)])
  
  dat$analysis_type[which(dat$row.no == i)] = combs$analysis_type[pick]
  
}

# check = dat[which(dat$case_control == 'CASE' & is.na(dat$analysis_type)),]
# check = dat[which(dat$case_control == 'CASE' & !is.na(dat$analysis_type)),]
# head(check[,c('init_vaccine', 'init_vaccine_date', 'init_viro_collection_date', 'init_form_date', 'fu_form_date', 'analysis_type', 'row.no')], 20)

################
#### CONTROLS
################

######### SET RULES 
A = c('<2wk of initial interview','>2wk of initial interview','No','missing')
B = c('negative','positive', 'inconclusive', 'missing')
C = c('done','not done')

combs = expand.grid(initial_vxax = A, initial_sero = B, initial_interview = C, follow_up_sero = B, follow_up_interview = paste(C, collapse = '/'), analysis_type = NA)
combs = combs[order(combs$initial_vxax),]

combs$analysis_type[which(combs$initial_interview == 'not done')] = 'EXCLUDE'
combs$analysis_type[which(combs$initial_vxax == '>2wk of initial interview')] = 'EXCLUDE'
combs$analysis_type[which(combs$initial_sero == 'positive')] = 'EXCLUDE'
combs$analysis_type[which(combs$follow_up_sero == 'positive')] = 'EXCLUDE'
combs$analysis_type[which(combs$initial_vxax == '<2wk of initial interview' & combs$initial_sero == 'negative' & combs$follow_up_sero == 'negative')] = 'MAIN'
combs$analysis_type[which(combs$initial_vxax == 'No' & combs$initial_sero == 'negative' & combs$follow_up_sero == 'negative' & is.na(combs$analysis_type))] = 'MAIN'
combs$analysis_type[is.na(combs$analysis_type)] = 'SENSITIVITY'

initial_sero = rep(NA, nrow(dat))
initial_sero[dat$init_sero_result == 'POSITIVE'] = 'positive'
initial_sero[dat$init_sero_result == 'NEGATIVE'] = 'negative'
initial_sero[dat$init_sero_result == 'INCONCLUSIVE'] = 'inconclusive'
initial_sero[is.na(dat$init_sero_result)] = 'missing'
dat$init_sero_result[is.na(initial_sero)]

follow_up_sero = rep(NA, nrow(dat))
follow_up_sero[dat$fu_sero_result == 'POSITIVE'] = 'positive'
follow_up_sero[dat$fu_sero_result == 'NEGATIVE'] = 'negative'
follow_up_sero[dat$fu_sero_result == 'INCONCLUSIVE'] = 'inconclusive'
follow_up_sero[is.na(dat$fu_sero_result)] = 'missing'
dat$fu_sero_result[is.na(follow_up_sero)]

for (i in dat$row.no[which(dat$case_control == 'CONTROL')]){
  
  pick = which(combs$initial_vxax %in% initial_vxax[which(dat$row.no == i)] & 
                 combs$initial_sero %in% initial_sero[which(dat$row.no == i)] & 
                 combs$follow_up_sero %in% follow_up_sero[which(dat$row.no == i)] & 
                 combs$initial_interview %in% initial_interview[which(dat$row.no == i)])
  
  dat$analysis_type[which(dat$row.no == i)] = combs$analysis_type[pick]
  
}

# check = dat[which(dat$case_control == 'CONTROL' & is.na(dat$analysis_type)),]
# check = dat[which(dat$case_control == 'CONTROL' & !is.na(dat$analysis_type)),]
# head(check[,c('init_inter_id', 'init_vaccine', 'init_vaccine_date', 
#               'init_sero_collection_date', 'fu_sero_collection_date', 
#               'init_sero_result',  'fu_sero_result', 
#               'init_form_date', 'fu_form_date', 'analysis_type', 'row.no')], 20)

# View(dat[is.na(dat$analysis_type),
#          c('init_inter_id', 'case_control', 'init_vaccine', 'init_vaccine_date', 
#               'init_sero_collection_date', 'fu_sero_collection_date', 
#               'init_sero_result',  'fu_sero_result', 
#               'init_form_date', 'fu_form_date', 'analysis_type', 'row.no')])

table(dat$analysis_type, useNA = 'ifany')

dat$pp = 0
dat$pp[dat$analysis_type == 'MAIN'] = 1

message(paste('There are a total of ', sum(dat$pp == 1, na.rm = T), ' participants in the main analysis group.'))

##############
## demographics 
##############

dat$age = as.numeric(dat$age)
hist(dat$age)

##############
## give levels to categorical questions 
##############

dat = clean_col(dat = dat, lvl = c('YES', 'NO', 'UNKNOWN'))
dat = clean_col(dat = dat, lvl = c('FEMALE', 'MALE', 'NOT KNOWN'))

dat = clean_col(dat = dat,lvl = c('MEDICAL DOCTOR', 'REGISTERED NURSE (OR EQUIVALENT)', 
                                  'ASSISTANT NURSE, NURSE TECHNICIAN (OR EQUIVALENT)',
                                  'NUTRITIONIST/DIETITIAN', 'PHYSICAL THERAPIST',
                                  'PHLEBOTOMIST', 'RADIOLOGY / X-RAY TECHNICIAN', "PATIENT TRANSPORTER",
                                  'LABORATORY PERSONNEL', "ADMISSION/RECEPTION CLERK",
                                  'ADMINISTRATION/CLERK', 'CLEANER', 'CATERING STAFF',
                                  'OTHER'))
dat$hcw_role_cat = NA
dat$hcw_role_cat[which(dat$hcw_role == 'MEDICAL DOCTOR')] = 'MEDICAL DOCTOR'
dat$hcw_role_cat[which(dat$hcw_role %in% c('REGISTERED NURSE (OR EQUIVALENT)', 
                                           'ASSISTANT NURSE, NURSE TECHNICIAN (OR EQUIVALENT)'))] = 'NURSE'
dat$hcw_role_cat[is.na(dat$hcw_role_cat)] = 'OTHER'

dat = clean_col(dat = dat, lvl = c('TERTIARY/UNIVERSITY', 'SECONDARY', 'PRIMARY', 'NONE', 'PREFER NOT TO ANSWER'))
dat$education[is.na(dat$education)] = 'PREFER NOT TO ANSWER'
dat$education_cat = NA
dat$education_cat[which(dat$education == 'TERTIARY/UNIVERSITY')] = 'TERTIARY/UNIVERSITY'
dat$education_cat[is.na(dat$education_cat)] = 'SECONDARY AND BELOW'
dat = clean_col(dat = dat, lvl = c('TERTIARY/UNIVERSITY', 'SECONDARY AND BELOW'))

dat = clean_col(dat = dat, lvl = c('YES', 'NO', 'THERE ARE NO COVID-19 DEDICATED STAFF IN MY FACILITY'))
dat = clean_col(dat = dat, lvl = c('YES', 'NO', 'NOT SURE'))

dat = clean_col(dat = dat, lvl = c('POSITIVE', 'NEGATIVE', 'INCONCLUSIVE'))
dat = clean_col(dat = dat, lvl = c('Most days (≥8 days)', 'Some days (4-7 days)', 
                                   'Few days (≤3 days)', 'Not had any other social interaction'))
dat = clean_col(dat = dat, lvl = c('Most days (≥8 days)', 'Some days (4-7 days)', 
                                   'Few days (≤3 days)', 'Not used public transport'))
dat = clean_col(dat = dat, lvl = c("ALWAYS, AS RECOMMENDED", "MOST OF THE TIME", "OCCASIONALLY", "RARELY", "NEVER"))
dat = clean_col(dat = dat, lvl = c("ALWAYS, AS RECOMMENDED", "MOST OF THE TIME", "OCCASIONALLY", "RARELY", "NEVER", "I DON’T KNOW WHAT IPC STANDARD PRECAUTIONS ARE"))
dat = clean_col(dat = dat, lvl = c("ALWAYS, ACCORDING TO THE RISK ASSESSMENT", "MOST OF THE TIME, ACCORDING TO THE RISK ASSESSMENT", "OCCASIONALLY", "RARELY", "NEVER"))
dat = clean_col(dat = dat, lvl = c("YES, ALL 5", "YES, ALL 4", "YES, ALL 3", "YES, ALL 6", "I DON'T KNOW THEM"))
dat = clean_col(dat = dat, lvl = c('MORE THAN 2 HOURS', 'LESS THAN 2 HOURS', 'I DON’T KNOW WHAT IPC STANDARD PRECAUTIONS ARE'))
dat = clean_col(dat = dat, lvl = c('I REMEMBER THE DATE', 'FORGOTTEN/NOT SURE', 'I DON’T KNOW WHAT IPC STANDARD PRECAUTIONS ARE'))
dat = clean_col(dat = dat, lvl = c('BOTH', 'ONLY PRACTICAL', 'ONLY REMOTELY/THEORETICAL', 'I DON’T KNOW WHAT IPC STANDARD PRECAUTIONS ARE'))

dat = clean_col(dat = dat, lvl = c('HIGH INCOME', 'UPPER MIDDLE INCOME', 'LOWER MIDDLE INCOME'))

dat$covid_specific_care_days[which(dat$covid_specific_care == 'THERE ARE NO COVID-19 DEDICATED STAFF IN MY FACILITY' | 
                                     dat$covid_specific_care == 'NO')] = 0
dat$covid_specific_care_days = as.numeric(dat$covid_specific_care_days)

#colnames(dat)[grep('type', colnames(dat))]

turn10toyesno = c('contact_mat_typePersonal items', 
                  'contact_mat_typeMedical devices used on the patient',
                  'contact_mat_typeClothes',
                  'contact_mat_typeLinen',
                  'contact_mat_typeMedical equipment connected to the patient (e.g. ventilator, infusion pump etc.)',
                  'contact_surface_typeWard corridor',
                  'contact_surface_typeBed',
                  'contact_surface_typeBathroom',
                  'contact_surface_typePatient table',
                  'contact_surface_typeBedside table',
                  'contact_surface_typeDining table',
                  'contact_surface_typeMedical gas panel',
                  "contact_1m_prolong15min_ppe_typeFace shield",                                                   
                  "contact_1m_prolong15min_ppe_typeGloves",                                                        
                  "contact_1m_prolong15min_ppe_typeCoverall",                                                        
                  "contact_1m_prolong15min_ppe_typeHead cover",                                                      
                  "contact_1m_prolong15min_ppe_typeRespirator (e.g. N95, FFP2 or equivalent)",                       
                  "contact_1m_prolong15min_ppe_typeShoe covers",                                                     
                  "contact_1m_prolong15min_ppe_typeMedical/surgical mask",                                           
                  "contact_1m_prolong15min_ppe_typeGoggles/glasses",                                                 
                  "contact_1m_prolong15min_ppe_typeGown",                                                            
                  "contact_1m_aerosol_ppe_typeFace shield",                                                          
                  "contact_1m_aerosol_ppe_typeGloves",                                                               
                  "contact_1m_aerosol_ppe_typeCoverall",                                                            
                  "contact_1m_aerosol_ppe_typeHead cover",                                                        
                  "contact_1m_aerosol_ppe_typeRespirator (e.g. N95, FFP2 or equivalent)",                        
                  "contact_1m_aerosol_ppe_typeShoe covers",                                                       
                  "contact_1m_aerosol_ppe_typeMedical/surgical mask",                                              
                  "contact_1m_aerosol_ppe_typeGoggles/glasses",                                                      
                  "contact_1m_aerosol_ppe_typeGown",                                                                 
                  "contact_1m_bodyfld_ppe_typeGloves",                                                               
                  "contact_1m_bodyfld_ppe_typeCoverall",                                                             
                  "contact_1m_bodyfld_ppe_typeHead cover",                                                           
                  "contact_1m_bodyfld_ppe_typeRespirator (e.g. N95, FFP2 or equivalent)",                            
                  "contact_1m_bodyfld_ppe_typeShoe covers",                                                          
                  "contact_1m_bodyfld_ppe_typeGoggles/glasses",                                                      
                  "contact_1m_bodyfld_ppe_typeMedical/surgical mask",                                                
                  "contact_1m_bodyfld_ppe_typeFace shield",                                                          
                  "contact_1m_bodyfld_ppe_typeGown",
                  'contact_mat_bodyfld_ppeFace shield',
                  'contact_mat_bodyfld_ppeGloves',
                  'contact_mat_bodyfld_ppeCoverall',
                  'contact_mat_bodyfld_ppeHead cover',
                  'contact_mat_bodyfld_ppeShoe covers',
                  'contact_mat_bodyfld_ppeRespirator (e.g. N95, FFP2 or equivalent)',
                  'contact_mat_bodyfld_ppeMedical/surgical mask',
                  'contact_mat_bodyfld_ppeGoggles/glasses',
                  'contact_mat_bodyfld_ppeGown', 
                  "contact_surface_bodyfld_ppe_typeGloves",                                                               
                  "contact_surface_bodyfld_ppe_typeCoverall",                                                             
                  "contact_surface_bodyfld_ppe_typeHead cover",                                                           
                  "contact_surface_bodyfld_ppe_typeRespirator (e.g. N95, FFP2 or equivalent)",                            
                  "contact_surface_bodyfld_ppe_typeShoe covers",                                                          
                  "contact_surface_bodyfld_ppe_typeGoggles/glasses",                                                      
                  "contact_surface_bodyfld_ppe_typeMedical/medical mask",                                                
                  "contact_surface_bodyfld_ppe_typeFace shield",                                                          
                  "contact_surface_bodyfld_ppe_typeGown",
                  "hcf_ipcprogramIPC programme",
                  "hcf_ipcprogramIPC team/service",
                  "hcf_ipcprogramIPC focal point",
                  "hcf_ipcprogramIPC training",
                  "hcf_ipcprogramI don’t know what an IPC programme is",
                  "hcf_alcavail_whereIn every room",
                  "hcf_alcavail_whereIn every ward (corridors)",
                  "hcf_alcavail_whereNext to each bed"
)

for (x in 1: length(turn10toyesno)) {
  dat = classify_cat(dat, turn10toyesno[x], 1, 'YES')
  dat = classify_cat(dat, turn10toyesno[x], 0, 'NO')
}

dat = clean_col(dat = dat, lvl = c('YES', 'NO'))

##############
## clean free text 
##############
dat$covid_pt_noexposed_num = NA
dat$covid_pt_noexposed = str_replace_all(dat$covid_pt_noexposed, fixed(" "), "")
#### those with words 
dat$covid_pt_noexposed_num[dat$covid_pt_noexposed %in% c('NOTGIVEN', 'NOTPROVIDED', 'NONE', 'NOTMENTIONED', 'NO',
                                                         'NOTAWAREOF', 'NESSUNO', 'NIL')] = '0'
dat$covid_pt_noexposed_num[dat$covid_pt_noexposed %in% c('400(APPROX)')] = '400'
dat$covid_pt_noexposed_num[grep('SAMPLE', dat$covid_pt_noexposed)] = '0'
dat$covid_pt_noexposed_num[grep('LAB', dat$covid_pt_noexposed)] = '0'
dat$covid_pt_noexposed_num[grep('/DAY', dat$covid_pt_noexposed)] = NA
dat$covid_pt_noexposed_num[dat$covid_pt_noexposed %in% c('LESS THAN 10', '03', '3/4')] = '10'
dat$covid_pt_noexposed_num[dat$covid_pt_noexposed %in% c('15/20')] = '20'
dat$covid_pt_noexposed_num[dat$covid_pt_noexposed %in% c('ABOVE200PATIENTS')] = '200'

#### those with > < - TO
dat$covid_pt_noexposed_num[grep('>', dat$covid_pt_noexposed)] = substr(dat$covid_pt_noexposed[grep('>', dat$covid_pt_noexposed)], 2 , nchar(dat$covid_pt_noexposed[grep('>', dat$covid_pt_noexposed)]))
dat$covid_pt_noexposed_num[grep('<', dat$covid_pt_noexposed)] = substr(dat$covid_pt_noexposed[grep('<', dat$covid_pt_noexposed)], 2 , nchar(dat$covid_pt_noexposed[grep('<', dat$covid_pt_noexposed)]))
dat$covid_pt_noexposed_num[grep('-', dat$covid_pt_noexposed)] = sub('^[^-]*-(\\d+).*', '\\1', dat$covid_pt_noexposed[grep('-', dat$covid_pt_noexposed)])
dat$covid_pt_noexposed_num[grep('TO', dat$covid_pt_noexposed)] = sub('^[^TO]*TO(\\d+).*', '\\1', dat$covid_pt_noexposed[grep('TO', dat$covid_pt_noexposed)])
dat$covid_pt_noexposed_num[grep('AROUND', dat$covid_pt_noexposed)] = sub('^[^AROUND]*AROUND(\\d+).*', '\\1', dat$covid_pt_noexposed[grep('AROUND', dat$covid_pt_noexposed)])
dat$covid_pt_noexposed_num[grep('ABOUT', dat$covid_pt_noexposed)] = sub('^[^ABOUT]*ABOUT(\\d+).*', '\\1', dat$covid_pt_noexposed[grep('ABOUT', dat$covid_pt_noexposed)])
dat$covid_pt_noexposed_num[grep('LESSTHAN', dat$covid_pt_noexposed)] = sub('^[^LESSTHAN]*LESSTHAN(\\d+).*', '\\1', dat$covid_pt_noexposed[grep('LESSTHAN', dat$covid_pt_noexposed)])
dat$covid_pt_noexposed_num[grep('MORETHAN', dat$covid_pt_noexposed)] = sub('^[^MORETHAN]*MORETHAN(\\d+).*', '\\1', dat$covid_pt_noexposed[grep('MORETHAN', dat$covid_pt_noexposed)])
dat$covid_pt_noexposed_num[grep('MORETHEN', dat$covid_pt_noexposed)] = sub('^[^MORETHEN]*MORETHEN(\\d+).*', '\\1', dat$covid_pt_noexposed[grep('MORETHEN', dat$covid_pt_noexposed)])
dat$covid_pt_noexposed_num[dat$covid_pt_noexposed %in% c('UNKNOWN', '-', 'NOT KNOWN', 'NOTKNOWN', 'NOT SURE', 'UNKOWN', 'UNKNWON', 'MULTIPLEPATIENTS',
                                                         'ALARGERNUMBEROFPATIENTS', 'PLENTY', 'TOMANY', 'PLENTY', 'NOTSURE', 'UNCOUNTABLE', 'MANY',
                                                         'ALOT,IMPSSIBLETOSAYHOWMUCH')] = NA
dat$covid_pt_noexposed_num[dat$covid_pt_noexposed %in% c('>50TIMES')] = '50'
dat$covid_pt_noexposed_num = as.numeric(dat$covid_pt_noexposed_num)

#table(dat$covid_pt_noexposed_num)

dat$hcf_form_hcf_name[which(dat$hcf_form_hcf_name == '\u062c\u0631\u0634 \u0627\u0644\u062d\u0643\u0648\u0645\u064a')] = "JERASH GOVERNMENTAL HOSPITAL"
dat$hcf_form_hcf_name[which(dat$hcf_form_hcf_name == '\u062c\u0631\u0634')] = "JERASH GOVERNMENTAL HOSPITAL"
dat$hcf_form_hcf_name[which(dat$hcf_form_hcf_name == '\u0645\u0633\u062a\u0634\u0641\u0649 \u0627\u0644\u062c\u0627\u0631\u062f\u0646\u0632')] = "AL-GARDENS"
dat$hcf_form_hcf_name[which(dat$hcf_form_hcf_name == '\u0627\u0644\u0645\u0644\u0643 \u0627\u0644\u0645\u0624\u0633\u0633')] = "KING ABDULLAH UNIVERSITY HOSPITAL"

########################
### categorise data 
########################
dat$contact_1m_prolong15min[which(dat$contact_1m == 'NO')] = 'NO'
dat$contact_1m_aerosol[which(dat$contact_1m == 'NO')] = 'NO'
dat$contact_1m_bodyfld[which(dat$contact_1m == 'NO')] = 'NO'
dat$contact_1m_no[which(dat$contact_1m == 'NO')] = '<10 TIMES'
dat$contact_1m_dur[which(dat$contact_1m == 'NO')] = '<5 MINUTES'

dat$contact_mat_bodyfld[which(dat$contact_mat == 'NO')] = 'NO'
dat$contact_mat_no[which(dat$contact_mat == 'NO')] = '<10 TIMES'

dat$contact_surface_bodyfld[which(dat$contact_surface == 'NO')] = 'NO'
dat$contact_surface_no[which(dat$contact_surface == 'NO')] = '<10 TIMES'

###########
dat = clean_col(dat = dat, lvl = c('<10 TIMES', '10-50 TIMES', '>50 TIMES'))
dat = clean_col(dat = dat, lvl = c('<5 MINUTES', '5-15 MINUTES', '>15 MINUTES'))
############

#  Personal risk factors - unknown and no considered NO
dat$covid_specific_care_cat = 'NO'
dat$covid_specific_care_cat[which(dat$covid_specific_care == 'YES')] = 'YES'

# dat$covid_specific_care_days not included due to large amount of missing data 

# dat$contact_1m_cat = 'YES'
# dat$contact_1m_cat[which(dat$contact_1m != 'YES')] = 'NO'
# 
# dat$contact_1m_aerosol_cat = 'YES'
# dat$contact_1m_aerosol_cat[which(dat$contact_1m_aerosol != 'YES')] = 'NO'
# 
# dat$contact_1m_prolong15min_cat = 'YES'
# dat$contact_1m_prolong15min_cat[which(dat$contact_1m_prolong15min != 'YES')] = 'NO'
# 
# dat$contact_1m_bodyfld_cat = 'YES'
# dat$contact_1m_bodyfld_cat[which(dat$contact_1m_bodyfld != 'YES')] = 'NO'
# 
# dat$contact_mat_cat = 'YES'
# dat$contact_mat_cat[which(dat$contact_mat != 'YES')] = 'NO'
# 
# dat$contact_mat_bodyfld_cat = 'YES'
# dat$contact_mat_bodyfld_cat[which(dat$contact_mat_bodyfld != 'YES')] = 'NO'
# 
# dat$contact_surface_cat = 'YES'
# dat$contact_surface_cat[which(dat$contact_surface != 'YES')] = 'NO'
# 
# dat$contact_surface_bodyfld_cat = 'YES'
# dat$contact_surface_bodyfld_cat[which(dat$contact_surface_bodyfld != 'YES')] = 'NO'
# 
# dat$contact_covid_outside_cat = 'YES'
# dat$contact_covid_outside_cat[which(dat$contact_covid_outside == 'NO')] = 'NO'
# 
# dat$public_transport_cat = 'YES'
# dat$public_transport_cat[which(dat$public_transport == 'NOT USED PUBLIC TRANSPORT'))] = 'NO'
# 
# dat$ppe_avail_cat = 'YES'
# dat$ppe_avail_cat[which(dat$ppe_avail != 'YES')] = 'NO'
# 
# dat$contact_social_outside_cat = 'YES'
# dat$contact_social_outside_cat[which(dat$contact_social_outside == 'NOT HAD ANY OTHER SOCIAL INTERACTION'))] = 'NO'

## practices
# dat$covid_specific_training_cat = 'YES'
# dat$covid_specific_training_cat[which(dat$covid_specific_training != 'YES')] = 'NO'
# 
# dat$contact_1m_aerosol_cat = 'YES'
# dat$contact_1m_aerosol_cat[which(dat$contact_1m_aerosol != 'YES')] = 'NO'
# 
# dat$contact_1m_bodyfld_cat = 'YES'
# dat$contact_1m_bodyfld_cat[which(dat$contact_1m_bodyfld != 'YES')] = 'NO'
# 
# dat$contact_mat_bodyfld_cat = 'YES'
# dat$contact_mat_bodyfld_cat[which(dat$contact_mat_bodyfld != 'YES')] = 'NO'
# 
# dat$contact_surface_bodyfld_cat = 'YES'
# dat$contact_surface_bodyfld_cat[which(dat$contact_surface_bodyfld != 'YES')] = 'NO'

##############
dat = clean_col(dat = dat, lvl = c('YES', 'NO'))
##############

cols = c('ipc_pt_practice_cat', 'hh_moments_practice_cat', 'contact_1m_hhbf_cat', 'contact_1m_hhaft_cat',
         'contact_mat_bodyfld_hhbf_cat', 'contact_mat_bodyfld_hhaft_cat', 'contact_surface_bodyfld_hhaft_cat')
for (col in cols){
  dat[[col]] = 'ALWAYS'
  dat[[col]][which(dat[[gsub("_cat*.","", col)]] != 'ALWAYS, AS RECOMMENDED')] = 'NOT ALWAYS'
}

###########
dat = clean_col(dat = dat, lvl = c('ALWAYS', 'NOT ALWAYS'))
###########

dat$ipc_training_latest_cat = 'I REMEMBER THE DATE' 
dat$ipc_training_latest_cat[which(dat$ipc_training_latest != 'I REMEMBER THE DATE')] = 'UNSURE'
dat = clean_col(dat = dat, lvl = c('I REMEMBER THE DATE', 'UNSURE'))

dat$ipc_training_hours_cat = 'MORE THAN 2 HOURS'
dat$ipc_training_hours_cat[which(dat$ipc_training_hours != 'MORE THAN 2 HOURS')] = 'LESS THAN 2 HOURS'
dat = clean_col(dat = dat, lvl = c('MORE THAN 2 HOURS', 'LESS THAN 2 HOURS'))

dat$hh_moments_aware_cat = 'NOT AWARE'
dat$hh_moments_aware_cat[which(dat$hh_moments_aware != 'YES, ALL 5')] = 'AWARE OF 5 MOMENTS'
dat = clean_col(dat = dat, lvl = c('AWARE OF 5 MOMENTS', 'NOT AWARE'))

dat$ppe_cat = 'ALWAYS/ MOST OF THE TIME'
dat$ppe_cat[dat$ppe != 'ALWAYS, ACCORDING TO THE RISK ASSESSMENT'] = 'NOT ALWAYS OR MOST OF THE TIME'
dat = clean_col(dat = dat, lvl = c('ALWAYS/ MOST OF THE TIME', 'NOT ALWAYS OR MOST OF THE TIME'))

dat$hh_practice_avg = round(rowMeans(data.frame(as.integer(dat$hh_pt_bf), as.integer(dat$hh_pt_aft), as.integer(dat$hh_bodyfld_aft), as.integer(dat$hh_procedure_bf), as.integer(dat$hh_env_aft))))

dat$public_transport_cat = 'NOT USED PUBLIC TRANSPORT'
dat$public_transport_cat[dat$public_transport != 'NOT USED PUBLIC TRANSPORT'] = 'USED PUBLIC TRANSPORT'
dat = clean_col(dat = dat, lvl = c('USED PUBLIC TRANSPORT', 'NOT USED PUBLIC TRANSPORT'))

dat$contact_social_outside_cat = 'NOT HAD ANY OTHER SOCIAL INTERACTION'
dat$contact_social_outside_cat[dat$contact_social_outside != 'NOT HAD ANY OTHER SOCIAL INTERACTION'] = 'HAD OTHER SOCIAL INTERACTION'
dat = clean_col(dat = dat, lvl = c('HAD OTHER SOCIAL INTERACTION', 'NOT HAD ANY OTHER SOCIAL INTERACTION'))

save(dat, file = 'cleandata.Rdata')


#########################################################################
### PLOT FLOW DIAGRAM
##########################################################################

library(DiagrammeRsvg)
library(magrittr)
library(rsvg)

## n enrolled 
n_total = dim(dat)[1]
n_case_total = count_n(d = dat, outcome = 'total', participant.type = 'CASE')
n_control_total = count_n(d = dat, outcome = 'total', participant.type = 'CONTROL')

## excluded 
n_case_vacc = count_n(d = dat, outcome = 'vaccine2wks', participant.type = 'CASE')
n_case_nointer = count_n(d = dat, outcome = 'interviewmiss', participant.type = 'CASE')
n_control_vacc = count_n(d = dat, outcome = 'vaccine2wks', participant.type = 'CONTROL')
n_control_nointer = count_n(d = dat, outcome = 'interviewmiss', participant.type = 'CONTROL')
n_control_seropos = count_n(d = dat, outcome = 'serologypositive', participant.type = 'CONTROL')

## sensitivity analysis 
n_case_vaccmiss = count_n(d = dat, outcome = 'vaccinemiss', participant.type = 'CASE')
n_case_viro2wks = count_n(d = dat, outcome = 'virology2wks', participant.type = 'CASE')
n_case_viromiss = count_n(d = dat, outcome = 'virologymiss', participant.type = 'CASE')
n_control_vaccmiss = count_n(d = dat, outcome = 'vaccinemiss', participant.type = 'CONTROL')
n_control_seromiss = count_n(d = dat, outcome = 'serologymiss', participant.type = 'CONTROL')
n_control_seroincon = count_n(d = dat, outcome = 'serologyinconclusive', participant.type = 'CONTROL')

n_case_pp = count_n(d = dat, outcome = 'pp', participant.type = 'CASE')
n_control_pp = count_n(d = dat, outcome = 'pp', participant.type = 'CONTROL')

g = "
digraph nicegraph {

graph[fontsize = 9]


  # node attributes
  node [shape = rectangle,
        fontname = Helvetica,
        color = grey80,
        style = filled, 
        width = 3,
        height = 2
        fixedsize = true]
  
  # node definitions with substituted label text
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      
      1 [label = '', width = 0, height = 0]
      2 [label = '', width = 0, height = 0]
      3 [label = '', width = 0, height = 0]
      4 [label = '', width = 0, height = 0]
      
{rank = same; tab1; tab2}
{rank = same; 1; 2; tab3; tab4}
{rank = same; 3; 4; tab5; tab6}
{rank = same; tab7; tab8}

  # edge attributes
  edge [color = '#C9C9EE']

  # node attributes
  
  node [shape = rectangle]
    tab1 -> 1 [arrowsize = 0]
    1 -> tab3 
    1 -> 3 [arrowsize = 0]
    3 -> tab5
    3 -> tab7
    
    tab2 -> 2 [arrowsize = 0]
    2 -> tab4
    2 -> 4 [arrowsize = 0]
    4 -> tab6
    4 -> tab8 
}

      [1]: paste0('Cases enrolled', '\\n ', '(n = ', n_case_total, ')')
      [2]: paste0('Controls enrolled', '\\n ', '(n = ', n_control_total, ')')
      [3]: paste0('Excluded', '\\n ', 'Vaccined >2 weeks before', '\\n ', 'the first interview', '\\n ', '(n = ', n_case_vacc, ')', '\\n ', 'Missing interview', '\\n ', '(n = ', n_case_nointer, ')')
      [4]: paste0('Excluded', '\\n ', 'Vaccined >2 weeks before', '\\n ', 'the first interview', '\\n ', '(n = ', n_control_vacc, ')', '\\n ', 'Missing interview', '\\n ', '(n = ', n_control_nointer, ')', '\\n ', 'Positive serology', '\\n ', '(n = ',n_control_seropos, ')')
      [5]: paste0('Sensitivity analysis', '\\n ', 'Missing vaccination', '\\n ', '(n = ', n_case_vaccmiss, ')', '\\n ', 'PCR >2 weeks before', '\\n ', 'the first interview', '\\n ', '(n = ', n_case_viro2wks, ')', '\\n ', 'Missing PCR', '\\n ', '(n = ', n_case_viromiss, ')')
      [6]: paste0('Sensitivity analysis', '\\n ', 'Missing vaccination', '\\n ', '(n = ', n_control_vaccmiss, ')', '\\n ', 'Missing serology', '\\n ', '(n = ', n_control_seromiss, ')', '\\n ', 'Inconclusive serology', '\\n ', '(n = ', n_control_seroincon, ')')
      [7]: paste0('Cases main analysis', '\\n ', '(n = ', n_case_pp, ')')
      [8]: paste0('Controls main analysis', '\\n ', '(n = ', n_control_pp, ')')
      
"
grViz(g) %>%
  export_svg %>% charToRaw %>% rsvg_png("flow.png")
