#Summary statistics
library(tidyverse)
library(data.table)
library(patchwork)
library(openxlsx)
source("Plot_themes.R")

#Load data
d = readxl::read_excel("../Data/Updated_data_nov22/26-10-2022 - case og control til statistik (949).xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
cases = dplyr::filter(d, group == "Cases") #Make separate df with cases
control = dplyr::filter(d, group == "Control") #Make separate df with control

cases$patient_type[is.na(cases$patient_type)] = 5
cases$patient_type = factor(cases$patient_type, levels = c(1,2,3,4, 5), labels = c("Primary RPL",
                                                                                   "Secondary RPL",
                                                                                   "Recurrent late\npregnancy loss",
                                                                                   "Other",
                                                                                   "Not available"))

#dev.off()
ggplot(cases, aes(x = patient_type))+
  geom_histogram(stat = "count", col = "black", fill = "dodgerblue3", size = 0.2, width = 0.5)+
  tt+
  xlab("")+
  ylab("No. of patients")
ggsave(filename = "../Results/Figures/Figure 1//Type_of_loss.pdf", device = cairo_pdf,
       width = 4, height = 3)
