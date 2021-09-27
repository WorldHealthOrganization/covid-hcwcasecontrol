# covid-hcwcasecontrol
This is the code repository for 'Assessment of risk factors for coronavirus disease 2019 (COVID-19) in health workers - a case-control study'

# Introduction 
The spread of SARS-CoV-2 is accompanied by uncertainty concerning its key epidemiological, clinical and virological characteristics, particularly its ability to spread in the human population and its virulence. Understanding SARS-CoV-2 infection among health workers and identifying the risk factors for adverse outcomes are important not only for characterizing virus transmission patterns and risk factors for infection, but also for preventing the future infection of health workers and patients, for informing and updating infection prevention and control (IPC) measures at health care facility and national levels, and for reducing secondary SARS-CoV-2 transmission within health care settings. </br>

The primary aim of the study is to characterise and assess the IPC risk factors for SARS-CoV-2 infection in health workers exposed to COVID-19 patients. The specific objectives are to: </br>

* describe the socio-demographic characteristics of health workers enrolled in the study; 
* quantify the relative importance of exposure risks for health workers to contract COVID-19;  
* identify individual-level risk factors and effective protective measures against SARS-CoV-2 transmission; and, 
* relative importance of individual-level risk factors across the healthcare facilities with varying IPC policies and strengths. 

# Methodology

## Study design and participant enrolment 
Health workers with confirmed COVID-19 were recruited as cases. Health workers exposed to COVID-19 patients in the same setting but without infection were recruited as controls with a target of at least 2–4 controls for every case. A health worker was defined as any member of staff in the health care facility involved in the provision of care for a COVID-19 patient, including those who have been present in the same area as the patient as well as those who may not have provided direct care to the patient but who have had contact with the patient’s body fluids, potentially contaminated items or environmental surfaces. This included health care professionals, allied health workers and auxiliary health workers such as cleaning and laundry personnel, x-ray physicians and technicians, clerks, phlebotomists, respiratory therapists, nutritionists, social workers, physical therapists, laboratory personnel, cleaners, admission/reception clerks, patient transporters, and catering staff. </br>

Exposure to COVID-19 patients was defined as close contact (within 1 metre and for more than 15 minutes) with a suspected/probable/confirmed COVID-19 patient(s), or indirect contact with fomites (for example, clothes, linen, utensils, furniture and so on) or with materials, devices or equipment linked to a suspected/probable/confirmed COVID-19 patient(s). A case was defined as a health worker exposed in a health care setting to a COVID-19 patient in the 14 days prior to the health worker’s confirmation test, and who is a confirmed COVID-19 case fulfilling either of the three criteria below^1^: </br>

 * A. A person with a positive Nucleic Acid Amplification Test (NAAT) </br>
 * B. A person with a positive SARS-CoV-2 Ag-RDT AND meeting either the probable case definition or suspected criteria A OR B </br>
 * C. An asymptomatic person with a positive SARS-CoV-2 Ag-RDT AND who is a contact of a probable or confirmed case. </br>

The exclusion criteria for a case was having a confirmed COVID-19 case among their close contacts, including in their household, within the previous 14 days (with the exception of the COVID-19 patient(s) to which they were exposed), and vaccination 2 weeks prior to the first interview. </br>

A control was defined as a health worker exposed in a health care setting to a COVID-19 patient in the 14 days prior to recruitment, and who is not being classified as a suspected or probable or confirmed COVID-19 case.^1^ The exclusion criteria for a control was having a positive serology test to SARS-CoV-2, and vaccination 2 weeks prior to the first interview. 

## Data 
Data collection and entry was performed on Go.Data. To ensure data quality across the various sites, data entered was checked for accuracy, consistency and completeness prior to merging and analysis. Data variables obtained from the questionnaires include the following broad categories:  

* Demographic factors, e.g., age, sex, country of residence, educational level;  
* Personal risk factors, e.g., occupation, hygiene practices, various types of exposures to SARS-CoV-2; 
* Institutional risk factors, e.g., IPC policies, available PPE resources; and,
* Outcomes, e.g., infection with SARS-CoV-2, mortality, hospitalisation, serological response. 

## Statistical analysis
Descriptive statistics can be automatically generated from the code shared in this repository. They included frequency tables for categorical data, means (with standard deviations), or medians (with interquartile ranges) depending on the distribution of the data. Categorical variables are compared with χ2 and Fisher exact tests as appropriate and continuous variables with unpaired, 2-tailed t tests or nonparametric Wilcoxon rank sum tests as appropriate.  

Further to the above, codes for regression models can also be found. These are used to identify risk factors for SARS-CoV-2 infection in health workers, i.e., associations between exposure variables and outcome variables. We used conditional logistic regressions to estimate odds ratios (ORs) adjusted for the confounding variables^2^. Because the cases and controls were matched based on the health care facilities when enrolled into the study, health care facility was used as grouping strata in the conditional regression model. Collinearity of the infection risk factors were evaluated in separate multivariate models adjusted for age, sex, occupation, education level, and country of residence. 

All tests of significance are performed at 5%. Confidence intervals will be obtained and reported where appropriate. 

All codes are written in R, a freely downloadable software (see: https://www.r-project.org/). 

# How to use this Github repository 

### Preparing 

### Preparing the data 
1. Please request for a cleaned dataset for the data you have previously shared with the WHO on the Go.Data platform (contact Mo Yin at yinm@who.int). The dataset will be made available in `.csv` format. For data security purposes, this cannot be directly retrieved from this Github repository. 
2. Download the following:
* R software, a free software environment for statistical computing and graphics (https://www.r-project.org/),
* Rstudio, a free and user-friendly integrated development environment(https://www.rstudio.com/products/rstudio/download/).
3.  

