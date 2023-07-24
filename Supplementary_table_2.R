#Summary statistics
library(tidyverse)
library(data.table)
source("Plot_themes.R")

#Load data
d = readxl::read_excel("../Data/05-06-2023 - case og control til statistik (949).xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
prior_loss = filter(d, group == "Control" & n_losses_before_ref > 0) 
d = filter(d, !record_id %in% prior_loss$record_id)

cases = filter(d, group == "Cases")
control = filter(d, group == "Control")

#Make empty dataframe
vars = c(
"HbA1c, mean (SD)
(mmol/mol)",

"Estimated average glucose, mean (SD)
(mmol/l)",
"Total cholesterol, mean (SD)
(mmol/l)",
"LDL, mean (SD)
(mmol/l)",

"HDL, mean (SD)
(mmol/l)",

"Triglycerides, mean (SD) (mmol/l)"
)

t2 = data.frame(
  Biomarker = vars,
  Cases = NA,
  Controls = NA,
  'p value' = NA
)

t2$Cases[1] = paste(round(mean(cases$hba1c, na.rm =T),1), " (", round(sd(cases$hba1c, na.rm=T), 1), ")", sep = "")
t2$Controls[1] = paste(round(mean(control$hba1c, na.rm =T),1), " (", round(sd(control$hba1c, na.rm=T), 1), ")", sep = "")
t2$p.value[1] = scales::scientific(t.test(cases$hba1c, control$hba1c, na.rm =T)$p.value, 3)

t2$Cases[2] = paste(round(mean(cases$glucose, na.rm =T),1), " (", round(sd(cases$glucose, na.rm=T), 1), ")", sep = "")
t2$Controls[2] = paste(round(mean(control$glucose, na.rm =T),1), " (", round(sd(control$glucose, na.rm=T), 1), ")", sep = "")
t2$p.value[2] = scales::scientific(t.test(cases$glucose, control$glucose, na.rm =T)$p.value, 3)


t2$Cases[3] = paste(round(mean(cases$total_kolesterol, na.rm =T),1), " (", round(sd(cases$total_kolesterol, na.rm=T), 1), ")", sep = "")
t2$Controls[3] = paste(round(mean(control$total_kolesterol, na.rm =T),1), " (", round(sd(control$total_kolesterol, na.rm=T), 1), ")", sep = "")
t2$p.value[3] = scales::scientific(t.test(cases$total_kolesterol, control$total_kolesterol, na.rm =T)$p.value, 3)


t2$Cases[4] = paste(round(mean(cases$ldl, na.rm =T),1), " (", round(sd(cases$ldl, na.rm=T), 1), ")", sep = "")
t2$Controls[4] = paste(round(mean(control$ldl, na.rm =T),1), " (", round(sd(control$ldl, na.rm=T), 1), ")", sep = "")
t2$p.value[4] = scales::scientific(t.test(cases$ldl, control$ldl, na.rm =T)$p.value, 3)

t2$Cases[5] = paste(round(mean(cases$hdl, na.rm =T),1), " (", round(sd(cases$hdl, na.rm=T), 1), ")", sep = "")
t2$Controls[5] = paste(round(mean(control$hdl, na.rm =T),1), " (", round(sd(control$hdl, na.rm=T), 1), ")", sep = "")
t2$p.value[5] = scales::scientific(t.test(cases$hdl, control$hdl, na.rm =T)$p.value, 3)

t2$Cases[6] = paste(round(mean(cases$triglycerider, na.rm =T),1), " (", round(sd(cases$triglycerider, na.rm=T), 1), ")", sep = "")
t2$Controls[6] = paste(round(mean(control$triglycerider, na.rm =T),1), " (", round(sd(control$triglycerider, na.rm=T), 1), ")", sep = "")
t2$p.value[6] = scales::scientific(t.test(cases$triglycerider, control$triglycerider, na.rm =T)$p.value, 3)

# Add adjusted values
d2 = fread( "../Results/Tables/tmp_table_adjusted_biomarkers.tsv")

#HBA1C
t2$"Adjusted p-value (all cases)"[1] = filter(d2, group == "ALL", feat == "HbA1c")$pval
t2$"Adjusted p-value (primary RPL)"[1] = filter(d2, group == "Primary RPL", feat == "HbA1c")$pval
t2$"Adjusted p-value (secondary RPL)"[1] = filter(d2, group == "Secondary RPL", feat == "HbA1c")$pval
t2$"Adjusted p-value (recurrent late pregnancy loss)"[1] = filter(d2, group == "Recurrent late pregnancy loss", feat == "HbA1c")$pval


#glucose
t2$"Adjusted p-value (all cases)"[2] = filter(d2, group == "ALL", feat == "Glucose")$pval
t2$"Adjusted p-value (primary RPL)"[2] = filter(d2, group == "Primary RPL", feat == "Glucose")$pval
t2$"Adjusted p-value (secondary RPL)"[2] = filter(d2, group == "Secondary RPL", feat == "Glucose")$pval
t2$"Adjusted p-value (recurrent late pregnancy loss)"[2] = filter(d2, group == "Recurrent late pregnancy loss", feat == "Glucose")$pval

#total_kolesterol
t2$"Adjusted p-value (all cases)"[3] = filter(d2, group == "ALL", feat == "Total cholesterol")$pval
t2$"Adjusted p-value (primary RPL)"[3] = filter(d2, group == "Primary RPL", feat == "Total cholesterol")$pval
t2$"Adjusted p-value (secondary RPL)"[3] = filter(d2, group == "Secondary RPL", feat == "Total cholesterol")$pval
t2$"Adjusted p-value (recurrent late pregnancy loss)"[3] = filter(d2, group == "Recurrent late pregnancy loss", feat == "Total cholesterol")$pval


#ldl
t2$"Adjusted p-value (all cases)"[4] = filter(d2, group == "ALL", feat == "LDL")$pval
t2$"Adjusted p-value (primary RPL)"[4] = filter(d2, group == "Primary RPL", feat == "LDL")$pval
t2$"Adjusted p-value (secondary RPL)"[4] = filter(d2, group == "Secondary RPL", feat == "LDL")$pval
t2$"Adjusted p-value (recurrent late pregnancy loss)"[4] = filter(d2, group == "Recurrent late pregnancy loss", feat == "LDL")$pval

#hdl
t2$"Adjusted p-value (all cases)"[5] = filter(d2, group == "ALL", feat == "HDL")$pval
t2$"Adjusted p-value (primary RPL)"[5] = filter(d2, group == "Primary RPL", feat == "HDL")$pval
t2$"Adjusted p-value (secondary RPL)"[5] = filter(d2, group == "Secondary RPL", feat == "HDL")$pval
t2$"Adjusted p-value (recurrent late pregnancy loss)"[5] = filter(d2, group == "Recurrent late pregnancy loss", feat == "HDL")$pval

#triglycerider
t2$"Adjusted p-value (all cases)"[6] = filter(d2, group == "ALL", feat == "Triglycerides")$pval
t2$"Adjusted p-value (primary RPL)"[6] = filter(d2, group == "Primary RPL", feat == "Triglycerides")$pval
t2$"Adjusted p-value (secondary RPL)"[6] = filter(d2, group == "Secondary RPL", feat == "Triglycerides")$pval
t2$"Adjusted p-value (recurrent late pregnancy loss)"[6] = filter(d2, group == "Recurrent late pregnancy loss", feat == "Triglycerides")$pval

t2 = rename(t2, "p-value, without adjusting for baseline characteristics" = p.value)

openxlsx::write.xlsx(t2, "../Results/Tables/Table2.xlsx")


