#Summary statistics
library(tidyverse)
library(data.table)
library(patchwork)
library(openxlsx)
source("Plot_themes.R")

#Load data
d = readxl::read_excel("../Data/05-06-2023 - case og control til statistik (949).xlsx")
d$group = factor(d$group, levels = c(1,0), labels = c("Cases", "Control"))
prior_loss = filter(d, group == "Control" & n_losses_before_ref > 0) 
d = filter(d, !record_id %in% prior_loss$record_id)


d = d%>%
  pivot_longer(cols = c(hba1c, ldl, hdl, triglycerider, total_kolesterol, glucose))%>%
  dplyr::select(group, BMI, name, value)
table(is.na(d$value))
d = filter(d, !is.na(value))
#LMs 
for(nam in unique(d$name)){
  for(g in c("Cases", "Control")){
    tmp = filter(d, name == nam, group == g)
    l_tmp = lm(data = tmp, formula = "value ~ BMI")
    #  ggplot(l_tmp, aes(x = age, y = value))+ geom_point() + geom_smooth(method = "lm")
    s_tmp = summary(l_tmp)
    int = s_tmp$coefficients[[1]]
    coef = s_tmp$coefficients[[2]]
    p_val = s_tmp$coefficients[[8]]
    ggplot(l_tmp, aes(x = BMI, y = value))+ geom_point() + geom_smooth(method = "lm")+
      geom_abline(slope = coef, intercept = int)
    
    tmp_out = data.frame(name = nam, group = g, intercept = int, coefficient = coef, 
                         p_value = p_val, yval = max(tmp$value),
                         xval = ifelse(g == "Cases", 25, 20))
    if(nam == unique(d$name)[[1]] && g == "Cases"){
      out = tmp_out
    }else{
      out = bind_rows(out, tmp_out)
    }
  }
}

out$adjusted_p = p.adjust(out$p_value, method = "bonferroni")
out$group = factor(out$group, levels = c("Cases", "Control"))

d$name = factor(d$name, levels = c("hba1c", "glucose", "total_kolesterol", "ldl", "hdl", "triglycerider"), 
                labels = c("HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL", "Triglycerides"))

out$name = factor(out$name, levels = c("hba1c", "glucose", "total_kolesterol", "ldl", "hdl", "triglycerider"), 
                  labels = c("HbA1c", "Glucose", "Total cholesterol", "LDL", "HDL", "Triglycerides"))




#d = left_join(d, out)
ggplot(d, aes(x = BMI, y = value))+
  geom_point(aes(col = group), size = 0.2)+
  geom_smooth(method ="lm", lty = 2, col = "black")+ 
  # geom_abline(data = out, aes(slope = coef, intercept = intercept), lty = 2)+ #Somehow these lines dont locate correctly, although the data in "out" is correct! It's an error in the facets i believe. I fixed it by using the geom_smooth instead.
  geom_text(data = out, aes(x = xval, y = yval, 
                            label = paste("q \u2264", scales::scientific(adjusted_p, 2), sep = "")),
            size = 3.2)+
  facet_grid(rows = vars(name), scales = "free", cols = vars(group))+
  tt+
  scale_color_manual(values = c("blue2", "grey70"))+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))
# geom_hline(yintercept = 4)

ggsave(filename = "../Results/Supp Figures/Supp figure 2//BMI_and_biomarkers.pdf", device = cairo_pdf,
       width = 5.5, height = 6)


