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


###Make plot of it
d3 = d%>%
  pivot_longer(cols = c(hba1c, ldl, hdl, triglycerider, total_kolesterol, glucose))%>%
  dplyr::select(group, smoking, name, value)%>%
  filter(!is.na(value))%>%
  group_by(group, name)%>%
  mutate(n = n(),
         title = paste(group, "\n","n=",n,sep=""))


d3$name = factor(d3$name, levels = c("hba1c", "glucose", "total_kolesterol", "ldl", "hdl", "triglycerider"), 
                 labels = c("HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL", "Triglycerides"))

d3$group = factor(d3$group, levels = rev(c("Cases", "Control")))

ggplot(d3, aes(x = group, y = value, aes(fill = group)))+
  geom_jitter(width = 0.12, size = 0.5, aes(color = group))+
  geom_boxplot(width = 0.65, outlier.shape = NA, alpha = 0, show.legend = F)+
  tt+
  facet_wrap(~name, scales = "free")+
  ggpubr::stat_compare_means(comparisons = list(c(1,2)), size = 3.2, method = "t.test")+
  scale_color_manual(values = rev(c("blue2", "grey70")))+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.5)))+
  xlab("")

ggsave(filename = "../Results/Figures/Figure 2/T2_metabolic_values_UNADJUSTED.pdf", device = cairo_pdf,
       width = 5.5, height = 4)
# # # 


