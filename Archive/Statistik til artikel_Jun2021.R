
#DATA 736 DELTAGERE
setwd("H:/HbA1c og gentagne graviditetstab/Data/Efter juni 2021/Merge SS og AbHab")
dir()
d <- read.csv2("10-06-2021 Datasæt d - SS og AbHab til statistik CSV.csv")
dir()
s <- read.csv2("01-07-2021 Datasæt s - SS til statistik CSV.csv")

dir()
a <- read.csv2("01-07-2021 Datasæt a - AbHab til statistik CSV.csv")

dir()
pRPL <- read.csv2("02-07-2021 Datasæt pRPL - Primær RPL til statistik CSV.csv")

sRPL <- read.csv2("02-07-2021 Datasæt sRPL - Sekundær RPL til statistik CSV.csv")

RLPL <- read.csv2("02-07-2021 Datasæt RLPL - Senabort til statistik CSV.csv")

#DATA 583 DELTAGERE
setwd("H:/HbA1c og gentagne graviditetstab/Data/Efter juni 2021/Merge SS og AbHab")
dir()

d2 <- read.csv2("09-08-2021 Datasæt d2 - SS  og AbHab til statistik (825) CSV.csv")

a2 <- read.csv2("30-07-2021 Datasæt a2 - AbHab til statistik (583) CSV.csv")

s <- read.csv2("01-07-2021 Datasæt s - SS til statistik CSV.csv")

pRPL2 <- read.csv2("09-08-2021 Datasæt pRPL2 - primær RPL og SS (578) CSV.csv")

sRPL2 <- read.csv2("09-08-2021 Datasæt sRPL2 - sekundær RPL og SS (418) CSV.csv")

RLPL2 <- read.csv2("09-08-2021 Datasæt RLPL2 - senaborter og SS (250) CSV.csv")



#Pakker
install.packages("Epi")
install.packages("MASS")
install.packages("tableone")
install.packages("epiR")
install.packages("Publish")
install.packages("doBy")

library(Epi)
library(MASS)
library(tableone)
library(epiR)
library(Publish)
library(doBy)

#Histogrammer
hist(a$Age)



#Normalfordelte
hist(d$HbA1c)
hist(d$total_kolesterol)
hist(d$hdl)

hist(a$HbA1c)
hist(a$glucose)
hist(a$total_kolesterol)
hist(a$hdl)


#Ikke-normalfordelte
hist(d$glucose)
hist(d$ldl)
hist(d$vldl)
hist(d$triglycerider)

hist(a$ldl)
hist(a$vldl)
hist(a$triglycerider)

hist(s$HbA1c)
hist(s$glucose)
hist(s$total_kolesterol)
hist(s$hdl)
hist(s$ldl)
hist(s$vldl)
hist(s$triglycerider)

# Baseline information


#Koder - variabler
d2$Group<-as.factor(d2$Group)
d2$Obesity<-as.factor(d2$Obesity)
d2$Smoking_status<-as.factor(d2$Smoking_status)
d2$Age<-as.numeric(d2$Age)
d2$HbA1c<-as.numeric(d2$HbA1c)
d2$glucose<-as.numeric(d2$glucose)
d2$total_kolesterol<-as.numeric(d2$total_kolesterol)
d2$ldl<-as.numeric(d2$ldl)
d2$hdl<-as.numeric(d2$hdl)
d2$vldl<-as.numeric(d2$vldl)
d2$triglycerider<-as.numeric(d2$triglycerider)

d2$Graviditetstab<-as.numeric(d2$Graviditetstab)


#Numeriske tabeller
tab<-CreateTableOne(
  vars=c("Age","BMI","HbA1c","Graviditetstab"),
  
  
#Factor
  factorVars=c("Obesity", "Smoking_status"),
  strata=c("Group"),
  data=d2,
  testApprox = chisq.test,
  testExact = fisher.test,
  testNormal = oneway.test,
  testNonNormal = wilcox.test)

tableone <- print(tab,
                  nonnormal = c("Age","BMI", "Graviditetstab", "HbA1c"),
                  Approx = c("Obesity","Smoking_status"),
                  ,quote=F)

tableone <- print(tab,
                  nonnormal = c("Age","BMI", "Graviditetstab", "HbA1c"),
                  Approx = c("Obesity","Smoking_status"),
                  ,quote=F)


#Obesity

table(d2$Obesity,d2$Group)
#Obese procent cases
174/528*100

#Obese procent control 
52/242*100

#P-værdi Obesity
chisq.test(d$Obesity,d$Group)

#Baseline: Patienttype
table(d2$Patient_type,d2$Group)
336/583*100
176/583*100
8/583*100
43/583*100
20/583*100


#Baseline: Smoking

table(d2$Smoking_status,d2$Group)

chisq.test(d2$Smoking_status,d2$Group)
90+113+34
49+252+138

46/410*100
236/410*100
128/410*100

90/237*100
113/237*100
34/237*100


#Table 2: mean metabolic values P-værdier
#HBA1C
#mean og SD
mean(a2$HbA1c, na.rm=TRUE)
sd(a2$HbA1c, na.rm=TRUE)

mean(s$HbA1c, na.rm=TRUE)
sd(s$HbA1c, na.rm=TRUE)

#Lineær regression
d2 <- within(d2,Group <- relevel(Group,ref="0"))
model1 = lm(HbA1c ~ Group, data = d2)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(HbA1c ~ Group+Age+BMI, data = d2)
model1 = lm(HbA1c ~ Group+Age+BMI, data = d[d$Age <= 27,])
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)


#GLUCOSE
#mean og SD
mean(a2$glucose, na.rm=TRUE)
sd(a2$glucose, na.rm=TRUE)

mean(s$glucose, na.rm=TRUE)
sd(s$glucose, na.rm=TRUE)

#Lineær regression
d2 <- within(d2,Group <- relevel(Group,ref="0"))
model1 = lm(glucose ~ Group, data = d2)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(glucose ~ Group+Age+BMI, data = d2)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)

#TOTALKOLESTEROL
#mean og SD
mean(a2$total_kolesterol, na.rm=TRUE)
sd(a2$total_kolesterol, na.rm=TRUE)

mean(s$total_kolesterol, na.rm=TRUE)
sd(s$total_kolesterol, na.rm=TRUE)

#Lineær regression
d2 <- within(d,Group <- relevel(Group,ref="0"))
model1 = lm(total_kolesterol ~ Group, data = d2)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(total_kolesterol ~ Group+Age+BMI, data = d2)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)

#LDL
#mean og SD
mean(a2$ldl, na.rm=TRUE)
sd(a2$ldl, na.rm=TRUE)

mean(s$ldl, na.rm=TRUE)
sd(s$ldl, na.rm=TRUE)

#Lineær regression
d2 <- within(d2,Group <- relevel(Group,ref="0"))
model1 = lm(ldl ~ Group, data = d2)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(ldl ~ Group+Age+BMI, data = d2)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)


#HDL
#mean og SD
mean(a2$hdl, na.rm=TRUE)
sd(a2$hdl, na.rm=TRUE)

mean(s$hdl, na.rm=TRUE)
sd(s$hdl, na.rm=TRUE)

#Lineær regression
d2 <- within(d2,Group <- relevel(Group,ref="0"))
model1 = lm(hdl ~ Group, data = d2)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(hdl ~ Group+Age+BMI, data = d2)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)

#TRIGLYCERIDER
#mean og SD
mean(a2$triglycerider, na.rm=TRUE)
sd(a2$triglycerider, na.rm=TRUE)

mean(s$triglycerider, na.rm=TRUE)
sd(s$triglycerider, na.rm=TRUE)

#Lineær regression
d2 <- within(d2,Group <- relevel(Group,ref="0"))
model1 = lm(log(triglycerider) ~ Group, data = d2)
model1
plot(model1)
#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#plot
plot(model1)

#Justeret for alder og BMI
model1 = lm(log(triglycerider) ~ Group+Age+BMI, data = d2)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)



#Table 3 - HbA1c
table(a2$HbA1c.38)
table(s$HbA1c.38)

#Table 3 - HbA1c - Unadjusted
logr = glm(HbA1c.38 ~ Group,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 3 - HbA1c - Adjusted
logr = glm(HbA1c.38 ~ Group+Age+BMI,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Plot
View(d2)
X0 <- with(d2[d2$Group == 0,], table(Age, HbA1c)
ggplot(d2, aes(x=Age, y=HbA1c, colour=factor(Group))) + geom_point() + stat_smooth()


#Table 3 - Glucose
table(a2$glucose.6.4)
table(s$glucose.6.4)

#Table 3 - Glucose - Unadjusted
logr = glm(glucose.6.4 ~ Group,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 3 - Glucose - Adjusted
logr = glm(glucose.6.4 ~ Group+Age+BMI,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 3 - Totalkolesterol
table(a2$total.kolesterol.5)
table(s$total.kolesterol.5)

#Table 3 - Totalkolesterol - Unadjusted
logr = glm(total.kolesterol.5 ~ Group,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 3 - Totalkolesterol - Adjusted
logr = glm(total.kolesterol.5 ~ Group+Age+BMI,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 3 - LDL
table(a2$ldl.3)
table(s$ldl.3)

#Table 3 - ldl - Unadjusted
logr = glm(ldl.3 ~ Group,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 3 - ldl - Adjusted
logr = glm(ldl.3 ~ Group+Age+BMI,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 3 - HDL
table(a2$hdl.1.2)
table(s$hdl.1.2)

#Table 3 - Hdl - Unadjusted
logr = glm(hdl.1.2 ~ Group,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 3 - Hdl - Adjusted
logr = glm(hdl.1.2 ~ Group+Age+BMI,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 3 - TRIGLLYCERIDER
table(a2$triglycerider.2)
table(s$triglycerider.2)

#Table 3 - TRIGLLYCERIDER - Unadjusted
logr = glm(triglycerider.2 ~ Group,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 3 - TRIGLLYCERIDER - Adjusted
logr = glm(triglycerider.2 ~ Group+Age+BMI,family = binomial(),data = d2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Nyt datasæt med primære/sekundære etc.
#CASES med forhøjede biomarkører
table(pRPL2$HbA1c.38,pRPL2$Group)
308+16
16/324*100
table(pRPL2$glucose.6.4,pRPL2$Group)
13+312
13/325*100
table(pRPL2$total.kolesterol.5,pRPL2$Group)
88+216
88/304*100
table(pRPL2$ldl.3,pRPL2$Group)
91+212
91/303*100
table(pRPL2$hdl.1.2,pRPL2$Group)
54+249
54/303*100
table(pRPL2$triglycerider.2,pRPL2$Group)
19+286
19/305*100


#Table 4a: Primær RPL 


#Table 4a - HbA1c - Unadjusted
logr = glm(HbA1c.38 ~ Group,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4a - HbA1c - Adjusted
logr = glm(HbA1c.38 ~ Group+Age+BMI,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4a - Glucose - Unadjusted
logr = glm(glucose.6.4 ~ Group,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4a - Glucose - Adjusted
logr = glm(glucose.6.4 ~ Group+Age+BMI,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 4a - Totalkolesterol - Unadjusted
logr = glm(total.kolesterol.5 ~ Group,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4a - Totalkolesterol - Adjusted
logr = glm(total.kolesterol.5 ~ Group+Age+BMI,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 4a - ldl - Unadjusted
logr = glm(ldl.3 ~ Group,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4a - ldl - Adjusted
logr = glm(ldl.3 ~ Group+Age+BMI,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4a - Hdl - Unadjusted
logr = glm(hdl.1.2 ~ Group,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4a - Hdl - Adjusted
logr = glm(hdl.1.2 ~ Group+Age+BMI,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 4a - TRIGLLYCERIDER - Unadjusted
logr = glm(triglycerider.2 ~ Group,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4a - TRIGLLYCERIDER - Adjusted
logr = glm(triglycerider.2 ~ Group+Age+BMI,family = binomial(),data = pRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4b: Sekundær RPL 
table(sRPL2$HbA1c.38,sRPL2$Group)
7+166
7/173*100
table(sRPL2$glucose.6.4,sRPL2$Group)
5+168
5/173*100
table(sRPL2$total.kolesterol.5,sRPL2$Group)
33+132
33/165*100
table(sRPL2$ldl.3,sRPL2$Group)
37+127
37/164*100
table(sRPL2$hdl.1.2,sRPL2$Group)
35+129
35/164*100
table(sRPL2$triglycerider.2,sRPL2$Group)
15+148
15/163*100



#Table 4b - HbA1c - Unadjusted
logr = glm(HbA1c.38 ~ Group,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4b - HbA1c - Adjusted
logr = glm(HbA1c.38 ~ Group+Age+BMI,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 4b - Glucose - Unadjusted
logr = glm(glucose.6.4 ~ Group,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4b - Glucose - Adjusted
logr = glm(glucose.6.4 ~ Group+Age+BMI,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 4b - Totalkolesterol - Unadjusted
logr = glm(total.kolesterol.5 ~ Group,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4b - Totalkolesterol - Adjusted
logr = glm(total.kolesterol.5 ~ Group+Age+BMI,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 4b - ldl - Unadjusted
logr = glm(ldl.3 ~ Group,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4b - ldl - Adjusted
logr = glm(ldl.3 ~ Group+Age+BMI,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4b - Hdl - Unadjusted
logr = glm(hdl.1.2 ~ Group,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4b - Hdl - Adjusted
logr = glm(hdl.1.2 ~ Group+Age+BMI,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 4b - TRIGLLYCERIDER - Unadjusted
logr = glm(triglycerider.2 ~ Group,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4b - TRIGLLYCERIDER - Adjusted
logr = glm(triglycerider.2 ~ Group+Age+BMI,family = binomial(),data = sRPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 4c: Senabort 
table(RLPL2$HbA1c.38,RLPL2$Group)
6+2
2/8*100
table(RLPL2$glucose.6.4,RLPL2$Group)
6+2
2/8*100
table(RLPL2$total.kolesterol.5,RLPL2$Group)
1+4
1/5*100
table(RLPL2$ldl.3,RLPL2$Group)
1+4
1/5*100
table(RLPL2$hdl.1.2,RLPL2$Group)
2+4
2/6*100
table(RLPL2$triglycerider.2,RLPL2$Group)
4+1
1/5*100

#Table 4c - HbA1c - Unadjusted
logr = glm(HbA1c.38 ~ Group,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4c - HbA1c - Adjusted
logr = glm(HbA1c.38 ~ Group+Age+BMI,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4c - Glucose - Unadjusted
logr = glm(glucose.6.4 ~ Group,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4c - Glucose - Adjusted
logr = glm(glucose.6.4 ~ Group+Age+BMI,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4c - Totalkolesterol - Unadjusted
logr = glm(total.kolesterol.5 ~ Group,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4c - Totalkolesterol - Adjusted
logr = glm(total.kolesterol.5 ~ Group+Age+BMI,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4c - ldl - Unadjusted
logr = glm(ldl.3 ~ Group,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4c - ldl - Adjusted
logr = glm(ldl.3 ~ Group+Age+BMI,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4c - Hdl - Unadjusted
logr = glm(hdl.1.2 ~ Group,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4c - Hdl - Adjusted
logr = glm(hdl.1.2 ~ Group+Age+BMI,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Table 4c - TRIGLLYCERIDER - Unadjusted
logr = glm(triglycerider.2 ~ Group,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 4c - TRIGLLYCERIDER - Adjusted
logr = glm(triglycerider.2 ~ Group+Age+BMI,family = binomial(),data = RLPL2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)








#Table 5a: 

#Sæt kun med cases
a <- read.csv2("01-07-2021 Datasæt a - AbHab til statistik CSV.csv")
table(a2$Pregnancy_rate)



#HbA1c - Unadjusted
logr = glm(Pregnancy_rate ~ HbA1c,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-15

#HbA1c - Justeret
logr = glm(Pregnancy_rate ~ HbA1c+Age+BMI,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-69

#Glucose - Unadjusted
logr = glm(Pregnancy_rate ~ glucose,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-14

#Glucose - Justeret
logr = glm(Pregnancy_rate ~ glucose+Age+BMI,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-68

#LDL - Unadjusted
logr = glm(Pregnancy_rate ~ ldl,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-53

#LDL - Justeret
logr = glm(Pregnancy_rate ~ ldl+Age+BMI,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-104


#Totalkolesterol - Unadjusted
logr = glm(Pregnancy_rate ~ total_kolesterol,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-51


#Totalkolesterol - Justeret
logr = glm(Pregnancy_rate ~ total_kolesterol+Age+BMI,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-102


#Triglycerider - Unadjusted
logr = glm(Pregnancy_rate ~ triglycerider,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-52


#Triglycerider - Justeret
logr = glm(Pregnancy_rate ~ triglycerider+Age+BMI,family = binomial(),data = a2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
250-103

#Table 5b: 

#Sæt kun med cases
dir()
b2 <- read.csv2("10-08-2021 Datasæt b2 - AbHab til statistik (250) CSV.csv")
table(b2$Live_birth)


#HbA1c - Unadjusted
logr = glm(Live_birth ~ HbA1c,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-8

#HbA1c - Justeret
logr = glm(Live_birth ~ HbA1c+Age+BMI,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-23


#Glucose - Unadjusted
logr = glm(Live_birth ~ glucose,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-7

#Glucose - Justeret
logr = glm(Live_birth ~ glucose+Age+BMI,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-22


#LDL - Unadjusted
logr = glm(Live_birth ~ ldl,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-33

#LDL - Justeret
logr = glm(Live_birth ~ ldl+Age+BMI,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-46

#Totalkolesterol - Unadjusted
logr = glm(Live_birth ~ total_kolesterol,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-32

#Totalkolesterol - Justeret
logr = glm(Live_birth ~ total_kolesterol+Age+BMI,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-45


#Triglycerider - Unadjusted
logr = glm(Live_birth ~ triglycerider,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-32


#Triglycerider - Justeret
logr = glm(Live_birth ~ triglycerider+Age+BMI,family = binomial(),data = b2)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
116-45



































#DATA UNGE DELTAGERE


setwd("H:/HbA1c og gentagne graviditetstab/Data/Efter juni 2021/Merge SS og AbHab")
dir()
d27 <- read.csv2("10-08-2021 Datasæt d27 - SS  og AbHab til statistik (298) CSV.csv")
dir()
s <- read.csv2("01-07-2021 Datasæt s - SS til statistik CSV.csv")

dir()
a27 <- read.csv2("10-08-2021 Datasæt a27 - AbHab til statistik (56) CSV.csv")

dir()
pRPL27 <- read.csv2("10-08-2021 Datasæt pRPL27 - primær RPL og SS (284) CSV.csv")



# Baseline information


#Koder - variabler
d27$Group<-as.factor(d27$Group)
d27$Obesity<-as.factor(d27$Obesity)
d27$Smoking_status<-as.factor(d27$Smoking_status)
d27$Age<-as.numeric(d27$Age)
d27$HbA1c<-as.numeric(d27$HbA1c)
d27$glucose<-as.numeric(d27$glucose)
d27$total_kolesterol<-as.numeric(d27$total_kolesterol)
d27$ldl<-as.numeric(d27$ldl)
d27$hdl<-as.numeric(d27$hdl)
d27$vldl<-as.numeric(d27$vldl)
d27$triglycerider<-as.numeric(d27$triglycerider)

d27$Graviditetstab<-as.numeric(d27$Graviditetstab)


#Numeriske tabeller
tab<-CreateTableOne(
  vars=c("Age","BMI","HbA1c","Graviditetstab"),
  
  
  #Factor
  factorVars=c("Obesity", "Smoking_status"),
  strata=c("Group"),
  data=d27,
  testApprox = chisq.test,
  testExact = fisher.test,
  testNormal = oneway.test,
  testNonNormal = wilcox.test)

tableone <- print(tab,
                  nonnormal = c("Age","BMI", "Graviditetstab", "HbA1c"),
                  Approx = c("Obesity","Smoking_status"),
                  ,quote=F)

tableone <- print(tab,
                  nonnormal = c("Age","BMI", "Graviditetstab", "HbA1c"),
                  Approx = c("Obesity","Smoking_status"),
                  ,quote=F)


#Obesity

table(d27$Obesity,d27$Group)
#Obese procent cases
19/53*100

#Obese procent control 
52/242*100

#P-værdi Obesity
chisq.test(d27$Obesity,d27$Group)

#Baseline: Patienttype
table(d27$Patient_type,d27$Group)
42+6+1+5+2

42/56*100
6/56*100
1/56*100
5/56*100
2/56*100


#Baseline: Smoking

table(d27$Smoking_status,d27$Group)
9+24+10
9/43*100
24/43*100
10/43*100

chisq.test(d27$Smoking_status,d27$Group)
90+113+34
49+252+138

46/410*100
236/410*100
128/410*100

90/237*100
113/237*100
34/237*100


#Table 2: mean metabolic values P-værdier
#HBA1C
#mean og SD
mean(a27$HbA1c, na.rm=TRUE)
sd(a27$HbA1c, na.rm=TRUE)

mean(s$HbA1c, na.rm=TRUE)
sd(s$HbA1c, na.rm=TRUE)

#Lineær regression
d27 <- within(d27,Group <- relevel(Group,ref="0"))
model1 = lm(HbA1c ~ Group, data = d27)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(HbA1c ~ Group+Age+BMI, data = d27)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)


#GLUCOSE
#mean og SD
mean(a27$glucose, na.rm=TRUE)
sd(a27$glucose, na.rm=TRUE)

mean(s$glucose, na.rm=TRUE)
sd(s$glucose, na.rm=TRUE)

#Lineær regression
d27 <- within(d27,Group <- relevel(Group,ref="0"))
model1 = lm(glucose ~ Group, data = d27)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(glucose ~ Group+Age+BMI, data = d27)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)

#TOTALKOLESTEROL
#mean og SD
mean(a27$total_kolesterol, na.rm=TRUE)
sd(a27$total_kolesterol, na.rm=TRUE)

mean(s$total_kolesterol, na.rm=TRUE)
sd(s$total_kolesterol, na.rm=TRUE)

#Lineær regression
d27 <- within(d,Group <- relevel(Group,ref="0"))
model1 = lm(total_kolesterol ~ Group, data = d27)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(total_kolesterol ~ Group+Age+BMI, data = d27)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)

#LDL
#mean og SD
mean(a27$ldl, na.rm=TRUE)
sd(a27$ldl, na.rm=TRUE)

mean(s$ldl, na.rm=TRUE)
sd(s$ldl, na.rm=TRUE)

#Lineær regression
d27 <- within(d27,Group <- relevel(Group,ref="0"))
model1 = lm(ldl ~ Group, data = d27)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(ldl ~ Group+Age+BMI, data = d27)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)


#HDL
#mean og SD
mean(a27$hdl, na.rm=TRUE)
sd(a27$hdl, na.rm=TRUE)

mean(s$hdl, na.rm=TRUE)
sd(s$hdl, na.rm=TRUE)

#Lineær regression
d27 <- within(d27,Group <- relevel(Group,ref="0"))
model1 = lm(hdl ~ Group, data = d27)
model1

#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#Justeret for alder og BMI
model1 = lm(hdl ~ Group+Age+BMI, data = d27)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)

#TRIGLYCERIDER
#mean og SD
mean(a27$triglycerider, na.rm=TRUE)
sd(a27$triglycerider, na.rm=TRUE)

mean(s$triglycerider, na.rm=TRUE)
sd(s$triglycerider, na.rm=TRUE)

#Lineær regression
d27 <- within(d27,Group <- relevel(Group,ref="0"))
model1 = lm(log(triglycerider) ~ Group, data = d27)
model1
plot(model1)
#Summary af model inkl. parameterestimater
summary(model1)

#Konfidensintervaller for parameterestimater
confint(model1)

#plot
plot(model1)

#Justeret for alder og BMI
model1 = lm(log(triglycerider) ~ Group+Age+BMI, data = d27)
model1

# Summary af model inkl. parameterestimater
summary(model1)

# Konfidens intervaller for parameterestimater
confint(model1)



#Table 3 - HbA1c
table(a27$HbA1c.38)
4/56*100
table(s$HbA1c.38)

#Table 3 - HbA1c - Unadjusted
logr = glm(HbA1c.38 ~ Group,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Table 3 - HbA1c - Adjusted
logr = glm(HbA1c.38 ~ Group+Age+BMI,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Plot
View(d2)
X0 <- with(d2[d2$Group == 0,], table(Age, HbA1c)
           ggplot(d2, aes(x=Age, y=HbA1c, colour=factor(Group))) + geom_point() + stat_smooth()
           
           
#Table 3 - Glucose
table(a27$glucose.6.4)
table(s$glucose.6.4)
           
#Table 3 - Glucose - Unadjusted
logr = glm(glucose.6.4 ~ Group,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
xp(confint(logr))
result<-publish(logr)
           
#Table 3 - Glucose - Adjusted
logr = glm(glucose.6.4 ~ Group+Age+BMI,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
           
#Table 3 - Totalkolesterol
table(a27$total.kolesterol.5)
table(s$total.kolesterol.5)
11+39
11/50*100
           
#Table 3 - Totalkolesterol - Unadjusted
logr = glm(total.kolesterol.5 ~ Group,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 3 - Totalkolesterol - Adjusted
logr = glm(total.kolesterol.5 ~ Group+Age+BMI,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
           
#Table 3 - LDL
table(a27$ldl.3)
table(s$ldl.3)
12+38
12/50*100

           
#Table 3 - ldl - Unadjusted
logr = glm(ldl.3 ~ Group,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 3 - ldl - Adjusted
logr = glm(ldl.3 ~ Group+Age+BMI,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
           
#Table 3 - HDL
table(a27$hdl.1.2)
table(s$hdl.1.2)
17+33
17/50*100

           
#Table 3 - Hdl - Unadjusted
logr = glm(hdl.1.2 ~ Group,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 3 - Hdl - Adjusted
logr = glm(hdl.1.2 ~ Group+Age+BMI,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
           
#Table 3 - TRIGLLYCERIDER
table(a27$triglycerider.2)
6+44
6/50*100
table(s$triglycerider.2)
           
#Table 3 - TRIGLLYCERIDER - Unadjusted
logr = glm(triglycerider.2 ~ Group,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 3 - TRIGLLYCERIDER - Adjusted
logr = glm(triglycerider.2 ~ Group+Age+BMI,family = binomial(),data = d27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
#Nyt datasæt med primære/sekundære etc.
#CASES med forhøjede biomarkører
table(pRPL27$HbA1c.38,pRPL27$Group)
3+39
3/42*100           
table(pRPL27$glucose.6.4,pRPL27$Group)
39+3
3/42*100           
table(pRPL27$total.kolesterol.5,pRPL27$Group)
10+27
10/37*100           
table(pRPL27$ldl.3,pRPL27$Group)
11+26
11/37*100
table(pRPL27$hdl.1.2,pRPL27$Group)
12+25
12/37*100
table(pRPL27$triglycerider.2,pRPL27$Group)
33+4
4/37*100
           
           
#Table 4a: Primær RPL 
           
           
#Table 4a - HbA1c - Unadjusted
logr = glm(HbA1c.38 ~ Group,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 4a - HbA1c - Adjusted
logr = glm(HbA1c.38 ~ Group+Age+BMI,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
#Table 4a - Glucose - Unadjusted
logr = glm(glucose.6.4 ~ Group,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 4a - Glucose - Adjusted
logr = glm(glucose.6.4 ~ Group+Age+BMI,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
           
#Table 4a - Totalkolesterol - Unadjusted
logr = glm(total.kolesterol.5 ~ Group,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 4a - Totalkolesterol - Adjusted
logr = glm(total.kolesterol.5 ~ Group+Age+BMI,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
           
#Table 4a - ldl - Unadjusted
logr = glm(ldl.3 ~ Group,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 4a - ldl - Adjusted
logr = glm(ldl.3 ~ Group+Age+BMI,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
#Table 4a - Hdl - Unadjusted
logr = glm(hdl.1.2 ~ Group,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 4a - Hdl - Adjusted
logr = glm(hdl.1.2 ~ Group+Age+BMI,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
           
           
#Table 4a - TRIGLLYCERIDER - Unadjusted
logr = glm(triglycerider.2 ~ Group,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
           
#Table 4a - TRIGLLYCERIDER - Adjusted
logr = glm(triglycerider.2 ~ Group+Age+BMI,family = binomial(),data = pRPL27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Table 5a: 

#Sæt kun med cases
dir()
a27 <- read.csv2("10-08-2021 Datasæt a27 - AbHab til statistik (56) CSV.csv")
table(a27$Pregnancy_rate)
22+34



#HbA1c - Unadjusted
logr = glm(Pregnancy_rate ~ HbA1c,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#HbA1c - Justeret
logr = glm(Pregnancy_rate ~ HbA1c+Age+BMI,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Glucose - Unadjusted
logr = glm(Pregnancy_rate ~ glucose,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Glucose - Justeret
logr = glm(Pregnancy_rate ~ glucose+Age+BMI,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#LDL - Unadjusted
logr = glm(Pregnancy_rate ~ ldl,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
22-6

#LDL - Justeret
logr = glm(Pregnancy_rate ~ ldl+Age+BMI,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
22-9

#Totalkolesterol - Unadjusted
logr = glm(Pregnancy_rate ~ total_kolesterol,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Totalkolesterol - Justeret
logr = glm(Pregnancy_rate ~ total_kolesterol+Age+BMI,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
22-9


#Triglycerider - Unadjusted
logr = glm(Pregnancy_rate ~ triglycerider,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
22-6


#Triglycerider - Justeret
logr = glm(Pregnancy_rate ~ triglycerider+Age+BMI,family = binomial(),data = a27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)
22-9

#Table 5b: 

#Sæt kun med cases
dir()
b27 <- read.csv2("10-08-2021 Datasæt b27 - AbHab til statistik (22) CSV.csv")
table(b27$Live_birth)


#HbA1c - Unadjusted
logr = glm(Live_birth ~ HbA1c,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#HbA1c - Justeret
logr = glm(Live_birth ~ HbA1c+Age+BMI,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#Glucose - Unadjusted
logr = glm(Live_birth ~ glucose,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Glucose - Justeret
logr = glm(Live_birth ~ glucose+Age+BMI,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)



#LDL - Unadjusted
logr = glm(Live_birth ~ ldl,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#LDL - Justeret
logr = glm(Live_birth ~ ldl+Age+BMI,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Totalkolesterol - Unadjusted
logr = glm(Live_birth ~ total_kolesterol,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Totalkolesterol - Justeret
logr = glm(Live_birth ~ total_kolesterol+Age+BMI,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

#Triglycerider - Unadjusted
logr = glm(Live_birth ~ triglycerider,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)


#Triglycerider - Justeret
logr = glm(Live_birth ~ triglycerider+Age+BMI,family = binomial(),data = b27)
summary(logr)
exp(coef(logr))
exp(confint(logr))
result<-publish(logr)

           
           