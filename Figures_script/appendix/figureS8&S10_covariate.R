################################################################################
library(plyr)
library(dplyr)
library(tidyverse)
library(ISOweek)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(patchwork)
library(data.table)
library(reshape2)
library(rugarch)
library(scales)
library(patchwork)

file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

epi_vec <<- c("scale_case","ct_sm_7")
stringency<<-c("StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage",
               "ContainmentHealthIndex_WeightedAverage")
meteorology<<- c("temperature","humidity","wind_speed")
pollutant <<-c("ozone","nitrogen_dioxide","nitrogen_oxides","carbon_monoxide",
               "fine_suspended_particulates",'respirable_suspended_particulates',"sulphur_dioxide") 


predictors_name  = data.frame(vars_raw = c("scaled_case","ct_sm_7",
                                           "StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage",
                                           "ContainmentHealthIndex_WeightedAverage",
                                           "temperature","humidity","wind_speed",
                                           "ozone","nitrogen_dioxide","nitrogen_oxides","carbon_monoxide",
                                           "fine_suspended_particulates",'respirable_suspended_particulates',"sulphur_dioxide"),
                              predictor = c("Historical case number", "7-day smoothed Ct value",
                                            "Stringency index", "Government response index","ContainmentHealth index",
                                            "Temperature","Absolute humidity","Wind speed",
                                            "Ozone","Nitrogen dioxide","Nitrogen oxides","Carbon monoxide",
                                            "Fine suspended particulates",'Respirable suspended particulates',"Sulphur dioxide"))



epi_vec <<- c("log_case","ct_sm_7")
stringency<<-c("StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage",
               "ContainmentHealthIndex_WeightedAverage")
meteorology<<- c("temperature","humidity","wind_speed")
pollutant <<-c("ozone","nitrogen_dioxide","nitrogen_oxides","carbon_monoxide",
               "fine_suspended_particulates",'respirable_suspended_particulates',"sulphur_dioxide") 





# true_case = readRDS("./data/true_reportcase.rds") %>% 
#   filter(date >= "2022-02-15" & date<= "2022-12-31") %>% 
#   filter()

true_rt = readRDS("./data/data_full_new_bp.rds") %>% 
   filter(date == date_analysis) %>% 
   filter(date >= "2022-01-01" & date<= "2022-12-31") %>% 
  dplyr::select(date,rt,rt_lwr,rt_upr) %>% 
  mutate(rt_upr = ifelse(rt_upr>=6,6,rt_upr))

other_cov = read_rds("./data/other_cov.rds") %>% 
  filter(date >= "2022-01-01" & date<= "2022-12-31")



meteorology_df = other_cov[,c("date",meteorology)]

vars_raw = predictors_name[-1,]$vars_raw
predictors = predictors_name[-1,]$predictor

p_list = list()

# Compute global date range to fix x-axis across all plots
date_limits <- range(c(other_cov$date, true_rt$date))

for (i in seq_along(vars_raw)) {
  # Capture variables in local environment to avoid loop issues
  p <- local({
    current_i <- i  # Freeze the value of i for this iteration
    var <- vars_raw[current_i]
    myvar <- predictors[current_i]
    
    scaling_factor <- max(other_cov[, var]) / max(true_rt$rt)
    
    ggplot(other_cov) +
      geom_line(aes_string(x = "date", y = var), col = "#A2D2FF", size = 0.8) +
      geom_line(aes(x = date, y = rt * scaling_factor), data = true_rt, col = "salmon") +
      geom_ribbon(
        aes(x = date, ymin = rt_lwr * scaling_factor, ymax = rt_upr * scaling_factor),
        data = true_rt, fill = "salmon", alpha = 0.3
      ) +
      scale_y_continuous(
        name = myvar,
        sec.axis = sec_axis(~ . / scaling_factor, name = "Rt", breaks = c(2, 4, 6))
      ) +
      geom_hline(yintercept = scaling_factor, col = "red", linetype = 2) +
      theme_bw() +
      # ... (keep your existing theme code) ...
      scale_x_date(
        date_labels = "%b", date_breaks = "3 months",
        limits = date_limits  # Enforce consistent x-axis
      ) +
      labs(x = "Date")
  })
  
  p_list[[i]] <- p
}

# ct = ggarrange(p_list[[1]],NA,NA,nrow = 1,labels = c("A"))
# policy = ggarrange(p_list[[2]],p_list[[3]],p_list[[4]],nrow = 1,labels = c("A"))
# meteo = ggarrange(p_list[[5]],p_list[[6]],p_list[[7]],nrow = 1,labels = c("B"))
# pollutant = ggarrange(p_list[[8]],p_list[[9]],p_list[[10]],
#                       p_list[[11]],p_list[[12]],p_list[[13]],p_list[[14]],
#                       ncol = 4,nrow = 2,labels = c("C"),align = "hv")
# 
# ggarrange(ct,p_list[[2]],p_list[[3]],p_list[[4]], 
#           layout_matrix = matrix(c(1,NA,NA, 2,3,4), nrow=2, byrow=TRUE))
# 
# 
# 
# ggarrange(ct,policy,meteo,pollutant,nrow = 4, ncol = 1,align = "hv")
# 

# grid.arrange(p_list[[1]],
#              p_list[[2]],p_list[[3]],p_list[[4]],
#              p_list[[5]],p_list[[6]],p_list[[7]],
#              p_list[[8]],p_list[[9]],p_list[[10]],
#              p_list[[11]],p_list[[12]],p_list[[13]],
#              p_list[[14]],
#              layout_matrix = matrix(c(1, NA, NA,
#                                       2, 3, 4,
#                                       5, 6, 7,
#                                       8, 9 , 10, 
#                                       11, 12, 13,
#                                       14), 
#                                     nrow = 6, byrow = TRUE))

p_allcov = ggarrange(plotlist = p_list,nrow = 5, ncol = 3,align = "hv")+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


ggsave(p_allcov, file = "./figures/figure_allcov.pdf",dpi = 300,width = 12, height = 12)
ggsave(p_allcov, file = "./figures_png/figure_allcov.jpg",dpi = 300,width = 12, height = 12)



true_case = data_full_new_bp %>% 
  filter(date == date_analysis) %>% 
  filter(date >= "2022-07-01" & date<= "2022-12-31") %>% 
  mutate(case = confirm) %>% 
  dplyr::select(date,case) 


cov = other_cov[other_cov$date >= "2022-07-01" & other_cov$date<= "2022-12-31",vars_raw]
all = cbind(case = true_case$case,cov)

cor(all)
melted_corr  <- melt(cor(all)) %>% 
  mutate(Var1 = factor(Var1,levels = c("case",predictors_name$vars_raw),labels = c("Case",predictors_name$predictor)),
         Var2 = factor(Var2,levels = c("case",predictors_name$vars_raw),labels = c("Case",predictors_name$predictor)))


p_cor = 
  ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = "", y = "")+
  theme(axis.text.x = element_text(size = 11,colour = "black"),
        axis.text.y = element_text(size = 11,colour = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 15, face = 'bold',colour = "black",vjust= -1),
        axis.title.y = element_text(size = 15, face = 'bold',colour = "black"),
        plot.title = element_text(size = 20, face = 'bold',colour = "black"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 13, colour = "black",face = "bold",vjust = 1),
        legend.box.margin = ggplot2::margin(r = 1,unit = "cm"),
        ##plot.title.position = 'plot',
        legend.position = "right")



ggsave(p_cor, file = "./figures/figure_var_cor.pdf",dpi = 300,width = 11, height = 7)
ggsave(p_cor, file = "./figures_png/figure_var_cor.jpg",dpi = 300,width = 11, height = 7)


