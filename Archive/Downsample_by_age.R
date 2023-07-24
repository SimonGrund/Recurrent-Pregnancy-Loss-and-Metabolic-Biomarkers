###Accounting for age, BMI, and smoking
library(tidyverse)
library(data.table)
library(caret)

d = readxl::read_excel("../Data/22-08-2022 - case og control til statistik (957).xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
#d = filter(d, age < 28)
d$age = round(d$age, 0)
d$age = as.factor(d$age)

#Downsample to exactly match case and control on age.
cases = filter(d, group == "Cases")
cases = pivot_longer(cases, )
cases$age = as.integer(cases$age)
ll = lm(data = cases, formula = "ldl~age")
summary(ll)
cases = pivot_longer()
ggplot(cases, aes(x = age, y = glucose))+
  geom_point()+
  geom_smooth(method = "lm", col = "blue", size = 2)

control = filter(d, group == "Control")
for(i in unique(control$age)){
  tmp_cont = filter(control, age == i)
  tmp_cas = filter(cases, age == i)
  nn = min(nrow(tmp_cont), nrow(tmp_cas))
  set.seed(1)
  cont_out = tmp_cont[sample(x = 1:nrow(tmp_cont), size = nn),]
  set.seed(1)
  cas_out = tmp_cas[sample(x = 1:nrow(tmp_cas), size = nn),]
  tmp_out = bind_rows(cont_out, cas_out)
  if(i == unique(control$age)[1]){
    out = tmp_out
  }else{
    out = bind_rows(out, tmp_out)
  }
}
table(out$group)
hist(out$age)
out$age = as.numeric(out$age)

write.table(out, "../Results/Data/downsampled_by_age.tsv", sep = "\t",
            col.names = T, row.names = F)
# 
# gg = glm(
#   formula = "group ~ age + BMI + smoking + hba1c",
#   data = out,
#   family = "binomial"
# )
# summary(gg)
# plot(gg)
# 
# hist(d2$hba1c)
# 
# gg = glm(
#   formula = " hba1c ~  age + BMI + smoking+ Class",
#   data = d2,
#   family = "gaussian"
# )
# summary(gg)
# 
# 
