#Summary statistics
library(tidyverse)
library(data.table)
library(patchwork)
library(openxlsx)
source("Plot_themes.R") #load theme for plots

#Load and format data
d = readxl::read_excel("../Data/05-06-2023 - case og control til statistik (949).xlsxcontrol til statistik (949).xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
prior_loss = filter(d, group == "Control" & n_losses_before_ref > 0) 
d = filter(d, !record_id %in% prior_loss$record_id)


d$smoking[is.na(d$smoking)] = 5
d$smoking = factor(d$smoking, levels = c(1,3,2,5), labels =   c("Current",
                                                                "Former",
                                                                "Never",
                                                                "Unknown"
))

d$patient_type[is.na(d$patient_type)] = 5
d$patient_type = factor(d$patient_type, levels = c(0,1,2,3,4, 5), 
                        labels = c("No prior loss\n(Control)", 
                                   "Primary RPL",
                                   "Secondary RPL",
                                   "Recurrent late\npregnancy loss",
                                   "Other",
                                   "Not available"))

cases = dplyr::filter(d, group == "Cases") #Make separate df with cases
control = dplyr::filter(d, group == "Control") #Make separate df with control

##### A figure of the data size
table(d$group)

##### To fill out table 1, we first we make the empty table#####
baseline = c(
  "Age at referral, median [IQR] (years)",
  "BMI, median [IQR] (range)",
  "Obesity, n (%)",
  "Number of smokers. (Current / Former / Never / Unknown)",
  "Number of prior losses, median [IQR], (range)",
  "Types of prior losses (No loss / Primary RPL / Secondary RPL / Recurrent late pregnancy loss)"
  )


baseline = 
  data.frame(
    Variable = baseline,
    Cases = rep("", 6),
    Control = rep("", 6),
    'p value' = rep("", 6),
    'Test' = rep("", 6)
  )

#Now, fill it out

####### "Age at referral, median [IQR] (years)"] #####
baseline$Cases[1] = paste(
  round(median(cases$age), 1), " [", round(quantile(cases$age, 0.25),1), ":",round(quantile(cases$age, 0.75),1), "] (n=", nrow(cases), ")"
, sep = "") 

baseline$Control[1] = paste(
  round(median(control$age), 1), " [", round(quantile(control$age, 0.25),1), ":",round(quantile(control$age, 0.75),1), "] (n=", nrow(control), ")"
  , sep = "") 

baseline$p.value[1] = scales::scientific(t.test(cases$age, control$age)$p.value, 3)

baseline$Test[1] = "T-test"

ageplot = ggplot(d, aes(x = age))+
  geom_histogram(fill = "dodgerblue3", col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  facet_grid(rows = vars(group))+
  tt+
  geom_vline(xintercept = 27, lty = 2, col = "grey10", size = 0.5)+
  ylab("No. of patients")+
  xlab("Age")+
  annotate(geom = "text", label = "27 years", x = 27.5, y = 60, hjust = 0, size = 3.2)
ageplot

######  "BMI, median [IQR] (range)" ##### 
baseline$Cases[2] = paste(
  round(median(cases$BMI, na.rm = T), 1), " [", round(quantile(cases$BMI, 0.25, na.rm = T),1), ":",round(quantile(cases$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(cases$BMI)), ")"
  , sep = "") 

baseline$Control[2] = paste(
  round(median(control$BMI, na.rm = T), 1), " [", round(quantile(control$BMI, 0.25, na.rm = T),1), ":",round(quantile(control$BMI, 0.75, na.rm = T),1), "] (n=", sum(!is.na(control$BMI)), ")"
  , sep = "") 

baseline$p.value[2] = scales::scientific(t.test(cases$BMI, control$BMI, na.rm = T)$p.value, 3)

baseline$Test[2] = "T-test"

#dev.off()
bmiplot = ggplot(d, aes(x = BMI))+
  geom_histogram(fill = "dodgerblue3", col = "black", breaks = seq(16, 46, 1), size = 0.2)+
  facet_grid(rows = vars(group))+
  tt+
  ylab("No. of patients")
 # geom_vline(xintercept = 27, lty = 2, col = "grey10", size = 2)+
#  annotate(geom = "text", label = "27 years", x = 27.5, y = 60, hjust = 0)
bmiplot

######  ""Obesity, n (%)"" ##### 
baseline$Cases[3] = paste(
  sum(cases$obesity == 1, na.rm = T), " (",round(sum(cases$obesity == 1, na.rm = T) / sum(!is.na(cases$obesity)), 1)*100, "%), n=",  sum(!is.na(cases$obesity))
    , sep = "") 

baseline$Control[3] = paste(
  sum(control$obesity == 1, na.rm = T), " (",round(sum(control$obesity == 1, na.rm = T) / sum(!is.na(control$obesity)), 1)*100, "%), n=",  sum(!is.na(control$obesity))
  , sep = "") 


baseline$p.value[3] = scales::scientific(
  fisher.test(table(d[,c("group", "obesity")]))$p.value, 3)

baseline$Test[3] = "T-test"

d2 = d%>%
  dplyr::filter(!is.na(obesity))%>%
  count(group, obesity) %>%       
  group_by(group) %>%
  mutate(pct= prop.table(n)) 
d2$obesity = factor(d2$obesity, levels = c(1,0), labels = c("Obese", "Non-obese"))

#dev.off()
obesityplot = 
ggplot(d2, aes(x = group, y = pct, fill = obesity))+
  geom_histogram(col = "black", stat = "identity", size = 0.2, width= 0.6)+
  tt+
  xlab("")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("brown", "dodgerblue3"))
obesityplot

#### Smoking habits
#Summarize per group and smoking habit to get the percentage
d2 = d%>%
  count(group, smoking) %>%       
  group_by(group) %>%
  mutate(pct= prop.table(n)) 

#dev.off() #Closes other graph windows
smokeplot = ggplot(d2, aes(x = smoking, fill = group, y = pct))+
  geom_histogram(stat = "identity", col = "black", position = "dodge", size = 0.2, width = 0.6)+
  tt+
  xlab("")+
  ylab("Pct. of patients")+
  scale_fill_manual(values = c("blue2", "grey70"))+
  scale_y_continuous(labels = scales::percent)
smokeplot

#Number of smokers (Current / prior / never / unknown)

baseline$Cases[4] = paste(
  sum(cases$smoking == "Current", na.rm = T), " / ", 
  sum(cases$smoking == "Former", na.rm = T), " / ", 
  sum(cases$smoking == "Never", na.rm = T), " / ", 
  sum(cases$smoking == "Unknown", na.rm = T)
  , sep = "") 

baseline$Control[4] = paste(
  sum(control$smoking == "Current", na.rm = T), " / ", 
  sum(control$smoking == "Former", na.rm = T), " / ", 
  sum(control$smoking == "Never", na.rm = T), " / ", 
  sum(control$smoking == "Unknown", na.rm = T)
  , sep = "") 

cc = chisq.test(table(d$group, d$smoking))

baseline$p.value[4] = cc$p.value

baseline$Test[4] = "Fisher's exact chi-square test"

#### #"Number of prior losses, median [IQR], (range)"# #####
baseline$Cases[5] = paste(
  median(cases$n_losses_before_ref, na.rm = T), " [",round(quantile(cases$n_losses_before_ref, 0.25, na.rm = T), 1), ":", round(quantile(cases$n_losses_before_ref, 0.75, na.rm = T), 1), "]",
  " n=", sum(!is.na(cases$n_losses_before_ref))
  , sep = "") 

baseline$Control[5] = "—"
baseline$p.value[5] = "—"
baseline$Test[5] = "—"

#dev.off()
priorlossplot = 
ggplot(d, aes(x = n_losses_before_ref))+
  geom_histogram(size = 0.2, col = "black", fill = "dodgerblue3", 
                  binwidth = 1)+
  tt+
  xlab("No. of prior losses")+
  ylab("No. of patients")+
  facet_grid(rows = vars(group), scales = "free_x")+
  scale_x_continuous(breaks = 0:15)
priorlossplot

baseline$Cases[5] = paste(
  median(cases$n_losses_before_ref, na.rm = T), " [",round(quantile(cases$n_losses_before_ref, 0.25, na.rm = T), 1), ":", round(quantile(cases$n_losses_before_ref, 0.75, na.rm = T), 1), "]",
  " n=", sum(!is.na(cases$n_losses_before_ref))
  , sep = "") 

baseline$Control[5] = paste(
  median(control$n_losses_before_ref, na.rm = T), " [",round(quantile(control$n_losses_before_ref, 0.25, na.rm = T), 1), ":", round(quantile(control$n_losses_before_ref, 0.75, na.rm = T), 1), "]",
  " n=", sum(!is.na(control$n_losses_before_ref))
  , sep = "") 

cc = chisq.test(table(d$group, d$n_losses_before_ref))

baseline$p.value[5] = cc$p.value
baseline$Test[5] = "Fisher's exact chi-square test"


#Types of prior losses
TypesOfLosses = ggplot(d, aes(x = patient_type, fill = group))+
  geom_histogram(stat = "count", col = "black", size = 0.2, width = 0.6)+
  tt+
  xlab("")+
  ylab("No. of patients")+
  scale_fill_manual(values = c("blue2", "grey70"))
  
TypesOfLosses

#  "Types of prior losses (No loss / Primary RPL / Secondary RPL / Recurrent late pregnancy loss)"
baseline$Cases[6] = paste(
  sum(cases$patient_type == "No prior loss\n(Control)", na.rm = T), " / ", 
  sum(cases$patient_type == "Primary RPL", na.rm = T), " / ", 
  sum(cases$patient_type == "Secondary RPL", na.rm = T), " / ", 
  sum(cases$patient_type == "Recurrent late\npregnancy loss", na.rm = T)
  , sep = "") 

baseline$Control[6] = paste(
  sum(control$patient_type == "No prior loss\n(Control)", na.rm = T), " / ", 
  sum(control$patient_type == "Primary RPL", na.rm = T), " / ", 
  sum(control$patient_type == "Secondary RPL", na.rm = T), " / ", 
  sum(control$patient_type == "Recurrent late\npregnancy loss", na.rm = T)
  , sep = "") 

baseline$p.value[6] = "—"
baseline$Test[6] = "—"



#Export the table
#write.table(x = baseline, file = "../Artikel/Tabeller og figurer/Output_from_simons_code/Table1.tsv", sep ="\n", col.names = T, row.names = F)
openxlsx::write.xlsx(baseline, "../Results/Tables/Table1.xlsx")


ggsave(plot = ageplot, filename = "../Results/Supp Figures/Supp figure 1//ageplot.pdf", device = cairo_pdf,
       width = 4, height = 3)

ggsave(plot = bmiplot, filename = "../Results/Supp Figures/Supp figure 1//bmiplot.pdf", device = cairo_pdf,
       width = 4, height = 3)

ggsave(plot = obesityplot, filename = "../Results/Supp Figures/Supp figure 1//obesityplot.pdf", device = cairo_pdf,
       width = 3, height = 3)

ggsave(plot = smokeplot, filename = "../Results/Supp Figures/Supp figure 1/smoking.pdf", device = cairo_pdf,
       width = 4, height = 3)

ggsave(plot = priorlossplot, filename = "../Results/Supp Figures/Supp figure 1//priorlossplot.pdf", device = cairo_pdf,
       width = 4, height = 3)

ggsave(plot = TypesOfLosses, filename = "../Results/Supp Figures//Supp figure 1//Type_of_loss.pdf", device = cairo_pdf,
       width = 5, height = 3)


#####

