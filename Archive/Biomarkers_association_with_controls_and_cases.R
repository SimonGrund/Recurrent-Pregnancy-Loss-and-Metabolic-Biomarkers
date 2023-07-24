#Summary statistics
library(tidyverse)
library(data.table)
source("Plot_themes.R")

#Load data
d = readxl::read_excel("../Data/22-08-2022 - case og control til statistik (957).xlsx")
d$group = factor(d$group, levels = c(0,1), labels = rev(c(": Cases", ": Control")))
cases = filter(d, group == "Cases")
control = filter(d, group == "Control")
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

ggplot(dp, aes(x = coef, y = reorder(feat, coef)))+
  geom_point()+
  geom_errorbarh(aes(xmin = coef-2*std, xmax = coef+2*std), height = 0.2, size = 0.2)+
  tt+
  geom_vline(xintercept = 0, lty = 2, size = 0.4, col = "grey60")+
  geom_text(aes(label = paste("q = ", scales::scientific(q, 2))), size = 3.2, vjust = 0, nudge_y = 0.2)+
  ylab("")+
  xlab("Coefficient")

#ggsave()