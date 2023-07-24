#Summary statistics
library(tidyverse)
library(data.table)
source("Plot_themes.R")

#Load data
d = fread("../Results/Data/downsampled_by_age.tsv")
#d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
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
  'p value' = NA,
  'Adjusted p' = NA
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

#Adjust p
t2$"Adjusted p-value" = p.adjust(t2$p.value, method = "fdr")
t2$Adjusted.p = NA

###Make plot of it
d3 = d%>%
  pivot_longer(cols = c(hba1c, ldl, hdl, triglycerider, total_kolesterol, glucose))%>%
  dplyr::select(group, smoking, name, value)%>%
  group_by(group, name)%>%
  mutate(n = n(),
         title = paste(group, "\n","n=",n,sep=""))

ggplot(d3, aes(x = title, y = value))+
  geom_boxplot(aes(fill = group))+
  tt+
  facet_wrap(~name, scales = "free")+
  ggpubr::stat_compare_means(comparisons = list(c(1,2)), size = 3.2, method = "t.test")+
  scale_fill_manual(values = c("brown", "cornflowerblue"))+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))  

ggsave(filename = "../Results/Figures/T2_metabolic_values_AGE_ADJUSTED.pdf", device = cairo_pdf,
       width = 5, height = 3.5)



