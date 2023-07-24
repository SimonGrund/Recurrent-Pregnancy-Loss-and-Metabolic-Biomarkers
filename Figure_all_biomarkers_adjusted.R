#Summary statistics
library(tidyverse)
library(data.table)
source("Plot_themes.R")

run_all = function(case_group = 1){
  
  #Load data
  d = readxl::read_excel("../Data/05-06-2023 - case og control til statistik (949).xlsx")
  d$group = factor(d$group, levels = c(0,1), labels = rev(c(": Cases", ": Control")))
  prior_loss = filter(d, group == ": Control" & n_losses_before_ref > 0) 
  d = filter(d, !record_id %in% prior_loss$record_id)

  
  d = dplyr::filter(d, patient_type %in% c(0,case_group)) #Now, only subgroup 0 (control) and patients (1)
  
  #cases = filter(d, group == "Cases")
  #control = filter(d, group == "Control")
  # 
  # d = d%>%
  #   pivot_longer(cols = c(hba1c, ldl, hdl, triglycerider, total_kolesterol, glucose))%>%
  #   dplyr::select(group, smoking, age, BMI, name, value)
  
  #ldl - SIG! Being in the CASE group, you have, on average, 0.5 higher LDL after adjusting for BMI.
  ll = lm(data = d, formula = 'ldl ~ BMI + group' )
  summary(ll)
  # Since mean in control is 2.0, this is a 25% elevation in LDL among cases compared to control
  mean(cases$ldl, na.rm = T)
  mean(control$ldl, na.rm = T)
  
  #hdl — SIG! Being in the CASE group, you have, on average, 0.1 lower LDL after adjusting for BMI
  ll = lm(data = d, formula = 'hdl ~ BMI + group' )
  summary(ll)
  # Since mean in control is 1.7, this is a 6% decrease in LDL among cases compared to control
  mean(cases$hdl, na.rm = T)
  mean(control$hdl, na.rm = T)
  
  ### Rest were non-significant
  #hba1c- NS
  ll = lm(data = d, formula = 'hba1c ~ BMI + group' )
  summary(ll)
  
  #triglycerider — NS
  ll = lm(data = d, formula = 'triglycerider ~ BMI + group' )
  summary(ll)
  
  #total_kolesterol — NS
  ll = lm(data = d, formula = 'total_kolesterol ~ BMI + age + group')
  summary(ll)
  
  #glucose — NS
  ll = lm(data = d, formula = 'glucose ~ BMI + group' )
  summary(ll)
  
  
  ###Lets make a graph :D ###
  
  #ldl - SIG! Being in the CASE group, you have, on average, 0.5 higher LDL after adjusting for BMI.
  ll = lm(data = d, formula = 'ldl ~ BMI + group' )
  ss = summary(ll)
  coef = ss$coefficients[3]
  std = ss$coefficients[6]
  pval = ss$coefficients[12]
  ldl = data.frame(feat = "ldl", coef = coef, std = std, pval = pval)
  
  #hdl — SIG! Being in the CASE group, you have, on average, 0.1 lower LDL after adjusting for BMI
  ll = lm(data = d, formula = 'hdl ~ BMI + group' )
  ss = summary(ll)
  coef = ss$coefficients[3]
  std = ss$coefficients[6]
  pval = ss$coefficients[12]
  hdl = data.frame(feat = "hdl", coef = coef, std = std, pval = pval)
  
  #hba1c- NS
  ll = lm(data = d, formula = 'hba1c ~ BMI + group' )
  ss = summary(ll)
  coef = ss$coefficients[3]
  std = ss$coefficients[6]
  pval = ss$coefficients[12]
  hba1c = data.frame(feat = "hba1c", coef = coef, std = std, pval = pval)
  
  #triglycerider — NS
  ll = lm(data = d, formula = 'triglycerider ~ BMI + group' )
  ss = summary(ll)
  coef = ss$coefficients[3]
  std = ss$coefficients[6]
  pval = ss$coefficients[12]
  triglycerider = data.frame(feat = "triglycerider", coef = coef, std = std, pval = pval)
  
  #total_kolesterol — NS
  ll = lm(data = d, formula = 'total_kolesterol ~ BMI + age + group' )
  ss = summary(ll)
  coef = ss$coefficients[4]
  std = ss$coefficients[8]
  pval = ss$coefficients[16]
  total_kolesterol = data.frame(feat = "total_kolesterol", coef = coef, std = std, pval = pval)
  
  #glucose — NS
  ll = lm(data = d, formula = 'glucose ~ BMI + group' )
  ss = summary(ll)
  coef = ss$coefficients[3]
  std = ss$coefficients[6]
  pval = ss$coefficients[12]
  glucose = data.frame(feat = "glucose", coef = coef, std = std, pval = pval)
  
  ##Assemble df
  dp = bind_rows(ldl, hdl, hba1c, triglycerider, total_kolesterol, glucose)
  dp$q = p.adjust(dp$pval, method = "bonferroni")
  
  
  dp$feat = factor(dp$feat, levels = rev(c("hba1c", "glucose", "total_kolesterol", "ldl", "hdl", "triglycerider")), 
                    labels = rev(c("HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL", "Triglycerides")))
  
  
  dp = dp%>%
    mutate(
      pval2 = "NS",
      pval2 = ifelse(pval < 0.05, "p \u2264 0.05", pval2),
      pval2 = ifelse(pval < 0.01, "p \u2264 0.01", pval2),
      pval2 = ifelse(pval < 0.001, "p \u2264 0.001", pval2)
    )
  
  plot_out = ggplot(dp, aes(x = coef, y =feat))+
    geom_point()+
    geom_errorbarh(aes(xmin = coef-2*std, xmax = coef+2*std), height = 0.2, size = 0.2)+
    tt+
    theme(
      plot.margin = margin(6,12,6,6)
    )+
    geom_vline(xintercept = 0, lty = 2, size = 0.4, col = "grey60")+
    geom_text(aes(label = pval2), size = 3.2, vjust = 0, nudge_y = 0.2)+
    
    #geom_text(aes(label = paste("q \u2264 ", scales::scientific(q, 2))), size = 3.2, vjust = 0, nudge_y = 0.2)+ #Exchange for this line to get proper q values
    ylab("")+
    xlab("Coefficient")+
    ggtitle(label = paste("Case group: ", paste(case_group, collapse = ", "), sep = ""), 
            subtitle = paste("Control: ", sum(d$group == ': Control'),
                       paste("Cases: ", sum(d$group == ': Cases'))
                       )
    )
  
  return(list(plot_out, dp))
}

#dev.off()
quartz(type = 'pdf', file = "../Results/Figures/Figure 3//Result_cases_ALL.pdf", width = 4, height = 3.5)
run_all(case_group = c(1,2,3))[[1]] + ggtitle("Complete Case Group ")

dev.off()


quartz(type = 'pdf', file = "../Results/Figures/Figure 3//Result_cases_1.pdf", width = 4, height = 3.5)
run_all(case_group = 1)[[1]] +  ggtitle("Primary RPL")
dev.off()


quartz(type = 'pdf', file = "../Results/Figures/Figure 3//Result_cases_2.pdf", width = 4, height = 3.5)
run_all(case_group = 2)[[1]] +  ggtitle("Secondary RPL ")
dev.off()

quartz(type = 'pdf', file = "../Results/Figures/Figure 3//Result_cases_3.pdf", width = 4, height = 3.5)
run_all(case_group = 3)[[1]] +  ggtitle("Recurrent Late Pregnancy Loss")
dev.off()

#Make table
all = run_all(case_group = c(1,2,3))[[2]] %>% mutate(group = "ALL") #All
d1 = run_all(case_group = 1)[[2]] %>% mutate(group = "Primary RPL")
d2 = run_all(case_group = 2)[[2]] %>% mutate(group = "Secondary RPL")
d3 = run_all(case_group = 3)[[2]] %>% mutate(group = "Recurrent late pregnancy loss")

out = bind_rows(all, d1, d2, d3)
write.table(out, file = "../Results/Tables/tmp_table_adjusted_biomarkers.tsv", sep = "\t", 
            col.names = T, row.names = F)

# run_all(case_group = 4)
# ggsave("../Results/Figures/Result_cases_4.pdf", device = "pdf", width = 4, height = 3.5) Group gone in updated data 
