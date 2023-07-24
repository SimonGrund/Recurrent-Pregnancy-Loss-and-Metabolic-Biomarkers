#Summary statistics
library(tidyverse)
library(data.table)
library(patchwork)
library(openxlsx)
library(rstatix)
library(ggpubr)
source("Plot_themes.R")

#Load data
d = readxl::read_excel("../Data/05-06-2023 - case og control til statistik (949).xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
prior_loss = filter(d, group == "Control" & n_losses_before_ref > 0) 
d = filter(d, !record_id %in% prior_loss$record_id)

d$smoking[is.na(d$smoking)] = 5
d$smoking = factor(d$smoking, levels = c(1,2,3,5), labels =   c("Current",
  "Never",
  "Former",
  "Not available"
))

### Moved to supp figure 1 script
# #Summarize per group and smoking habit to get the percentage
# d2 = d%>%
#   count(group, smoking) %>%       
#   group_by(group) %>%
#   mutate(pct= prop.table(n)) 
# 
# #dev.off() #Closes other graph windows
# ggplot(d2, aes(x = smoking, fill = group, y = pct))+
#   geom_histogram(stat = "identity", col = "black", position = "dodge", size = 0.2)+
#   tt+
#   xlab("")+
#   ylab("Pct. of patients")+
#     scale_fill_manual(values = c("blue2", "grey70"))+
#   scale_y_continuous(labels = scales::percent)
# 
# 
# ggsave(filename = "../Results/Supp Figures/Supp figure 1/smoking.pdf", device = cairo_pdf,
#        width = 4, height = 3)


###
d = d%>%
  pivot_longer(cols = c(hba1c, ldl, hdl, triglycerider, total_kolesterol, glucose))%>%
  dplyr::select(group, smoking, name, value)


#dev.off()
d = filter(d, smoking != "Not available")
d$smoking = factor(d$smoking, levels = c("Current", "Former", "Never"))

d$name = factor(d$name, levels = c("hba1c", "glucose", "total_kolesterol", "ldl", "hdl", "triglycerider"), 
                labels = c("HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL", "Triglycerides"))

symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                    symbols = c("****", "***", "**", "*", "ns"))

ggplot(d, aes(x = smoking, y = value))+
  geom_boxplot(aes(fill = group), size = 0.2, width = 0.5, outlier.size =  0.2)+
  facet_grid(rows = vars(name), scales = "free_y", cols = vars(group))+
  tt+
  ggpubr::stat_compare_means(comparisons = list(c(1,2), c(2,3), c(1,3)), size = 3.2,
                             aes(label = ..p.signif..)
                             )+
  scale_fill_manual(values = c("blue2", "grey70"))+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

ggsave(filename = "../Results/Supp Figures/Supp figure 2//smoking_and_biomarkers.pdf", device = cairo_pdf,
       width = 4.5, height = 8)

#Try to adjust for multiple testing using rstatix
# backup = d
# d = filter(backup, group = "Control" )
# stat.test <- d %>%
#   group_by(name) %>%
#   t_test(value ~ smoking) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance("p.adj")
# stat.test <- stat.test %>% add_xy_position(x = "smoking")
# stat.test$p.scient <- format(stat.test$p.adj, scientific = TRUE)
# 
# bxp = ggboxplot(d, x = "smoking", y = "value", ggtheme = tt, size = 0.2)
# bxp = facet(p = bxp,
#       scales = c("free"), 
#       facet.by = c("name"))
# 
# bxp + stat_pvalue_manual(stat.test, label = "p.scient", size = 3.2,
#                          , bracket.shorten = 0.05)+
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))


