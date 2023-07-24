
#Summary stats
source("Table1_and_Supp_Fig_1.R") #Table 1 and supplementary figure 1

#Test for confounding factors
source("Accounting_for_smoking.R") #No association so can be ignored # (Changed to NS for this one... But not next two)
source("Accounting_for_age.R")  #No association so can be ignored except possibly for total kolesterol #-
source("Accounting_for_BMI.R") #Consistent association so we must account for this! BMI_and_biomarkers.pdf #-

#Test for association with each biomarker
source("Figure_Unadjusted_biomarker_associations.R") #Figure 2, T2_metabolic_values_UNADJUSTED
source("Figure_all_biomarkers_adjusted.R") #All subgroups included in scrips 

source("Supplementary_table_2.R") #Biomarker associations, adjusted and unadjusted.


#Odds ratio — 11/12-2022
#Kontrol sammenlignet med hele casegruppen
source("Odds_ratios.R") #Odds_ratio_biomarkers_above_treshhold.pdf #-
#Kontrol sammenlignet med kvinder med primær RPL (patienttype 1)
source("Odds_ratios_primary_RPL.R") #-
#Kontrol sammenlignet med kvinder med sekundær RPL (patienttype 2)
source("Odds_ratios_secondary_RPL.R") #-

#Adjusted odds ratios 27/04-2023 #Fix color and widthds!
source("Odds_ratios_Adjusted.R")
#Kontrol sammenlignet med kvinder med primær RPL (patienttype 1)
source("Odds_ratios_Adjusted_primary_RPL.R")
#Kontrol sammenlignet med kvinder med sekundær RPL (patienttype 2)
source("Odds_ratios_Adjusted_secondary_RPL.R")
#Kontrol sammenlignet med kvinder med recurent late pregnancy loss (patienttype 3)
#For some biomarkers too little data for the GLM to converge
#source("Odds_ratios_Adjusted_third_RPL.R")

#Table 3 with odds ratios
source("Supp_table_3.R")

#Evaluate the rate of achieved pregnancies - 08-01-2023 
source("Rate_of_achieved_pregnancies.R") #Rate_of_pregnancy_and_live_birth.pdf ##
source("Rate_of_achieved_pregnancies_adjusted.R") ##


# source("Downsample_by_age.R")
# source("Supplementary_table_2.R")