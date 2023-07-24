# Odds ratio med 95 % konfidensintervaller: sammenligning mellem kontroller og cases med forhøjede/abnorme metaboliske biomarkører (ja/nej) for hver biomarkører
# HbA1c > 39.0
# Glucose > 6.4
# Total cholesterol >5.0 mmol/l
# LDL > 3.0 mmol/l
# HDL < 1.2
# Triglycerider > 2.0

#Summary statistics
library(tidyverse)
library(data.table)
library(patchwork)
library(openxlsx)
source("Plot_themes.R") #load theme for plots

#Load data
d = readxl::read_excel("../Data/05-06-2023 - case og control til statistik (949).xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
prior_loss = filter(d, group == "Control" & n_losses_before_ref > 0) 
d = filter(d, !record_id %in% prior_loss$record_id)


d = filter(d, patient_type %in% c(0,2))

d = dplyr::select(d,
                  record_id, patient_type, group, 'hba1c', 'glucose', 'total_kolesterol',
                  'ldl', 'hdl', 'triglycerider')%>%
  mutate(
    'HbA1c>39' = ifelse(hba1c >= 39,"Elevated", "Normal"),
    'Clucose>6,4'= ifelse(glucose >= 6.4, "Elevated", "Normal"), 
    'Total cholesterol>5' = ifelse(total_kolesterol >= 5, "Elevated", "Normal"),
    'LDL>3' = ifelse(ldl >= 3, "Elevated", "Normal"), 
    'HDL<1,2' = ifelse(hdl >= 1.2, "Elevated", "Normal"), 
    'Triglycerides>2' = ifelse(triglycerider >= 2, "Elevated", "Normal")
  )%>%
  pivot_longer(c('HbA1c>39', 'Clucose>6,4', 'Total cholesterol>5',
                 'LDL>3', 'HDL<1,2', 'Triglycerides>2'))

d$value = factor(d$value, levels = rev(c("Elevated", "Normal")))

# Odds ratio med 95 % konfidensintervaller: sammenligning mellem kontroller og cases med 
# forhøjede/abnorme metaboliske biomarkører (ja/nej) for hver biomarkører

features = unique(d$name)
#f = features[3]
for(f in features){
  f
  tmp = filter(d, name == f)
  summary = tmp%>%
    group_by(group)%>%
    na.omit(value)%>%
    mutate(n = n(),
           n_elevated = sum(value == "Elevated"),
           rate = n_elevated / n)%>%
    distinct(group, n, n_elevated, rate)
  summary
  summary$rate[1] / summary$rate[2]
  
  ggplot(summary, aes(x = group, y = rate))+
    geom_histogram(stat = "identity")
  
  gg = glm(group ~ value, family = binomial, data = tmp)
  
  ss = summary(gg)
  odds_ratio = exp(coef(gg))[2] #Exponential of the coefficient of being elevated 
  summary$rate[1] / summary$rate[2] #Manual calculation of odds ratio, i think. 
  odds_ratio
  
 # confint(gg)[[2]]
  sum2 = pivot_wider(summary, names_from = c(group), values_from = c(n_elevated, n, rate))
  out_tmp = data.frame(feature = f, 
                       odds_ratio_glm = odds_ratio,
                       odds_ratio_manual = summary$rate[1] / summary$rate[2],
                       'confint_2,5%' = exp(confint(gg)[[2]]),
                       'confint_97,5%' = exp(confint(gg)[[4]]),
                       p_val = ss$coefficients[8])
  out_tmp = bind_cols(out_tmp, sum2)
  
  if(f == features[1]){
    out = out_tmp
  }else{
    out = bind_rows(out, out_tmp)
  }
}

out2 = out%>%
  pivot_longer(cols = c(rate_Control, rate_Cases))

out2$feature = factor(out2$feature, levels = c('HbA1c>39', 'Clucose>6,4', 'Total cholesterol>5',
                                               'LDL>3', 'HDL<1,2', 'Triglycerides>2'))


out2$name = factor(out2$name, levels =  c("rate_Control", "rate_Cases"), labels =  c("Control", "Cases"))
out2 = out2%>%
  mutate(
    id = paste(feature, "\n", "Odds ratio: ", round(odds_ratio_glm, 2), "\n",
               "95%-CI: [", round(confint_2.5., 2), ":", round(confint_97.5., 2), "]",
               "\n", "p \u2264 ", scales::scientific(p_val, digits = 2),
               sep = ""),
    id = ifelse(odds_ratio_glm == 0, paste(feature, "\n", "Odds ratio: NA", "\n",
                                           "95%-CI: NA",
                                           "\n", "p \u2264 ", 
                                           sep = ""), 
                id),
  )

out2 = arrange(out2, feature)
out2$id = factor(out2$id, levels = unique(out2$id))

ggplot(out2, aes(x = name, y = value))+
  geom_histogram(stat = "identity", width = 0.3, fill = "dodgerblue4", color = "black", linewidth = .4)+
  facet_wrap(~id)+
  tt+
  ylab("Rate")+
  xlab("")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  ggtitle("Secondary RPL, unadjusted for baseline characteristics")

ggsave("../Results/Supp Figures/Supp Figure 3//Odds_ratio_biomarkers_above_treshhold_secondary_RPL.pdf",
       width = 5, height = 4)
 

out3 = out2%>%
  mutate(
    id = paste(round(odds_ratio_glm, 2),
               ", 95%-CI: [", round(confint_2.5., 2), ":", round(confint_97.5., 2), "]",
               sep = ""),
    id = ifelse(odds_ratio_glm == 0, paste("NA",
                                           ", 95%-CI: NA", 
                                           sep = ""), 
                id),
  )%>%
  distinct(feature, "odds ratio" = id, "p-value" = paste(scales::scientific(p_val, digits = 2)) )
out3 = out3%>%
  mutate(
    "Patient group" = "Secondary RPL",
    "Adjusted for baseline characteristica" = "No"
  )

write.table(out3, "../Results/Tables/TMP/or2_un.tsv", sep = "\t", col.names = T, row.names = F)
