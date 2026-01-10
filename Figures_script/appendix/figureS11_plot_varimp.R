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


### train res
model_name_list = c("arima","lm","garch","svr","rf","xgb","gbm","gru")
coef_all = NULL
for (modelname in model_name_list){
   coef_df_all = read.csv(paste("./train_coef/",modelname,"_coef_all.csv",sep = ""))
   
   coef_mean_df = 
     coef_df_all %>% 
     filter(!var %in% c("(intercept)","mu","ma1","omega","beta1")) %>% 
     group_by(var) %>% 
     dplyr::summarise(coef_mean = mean(coef)) %>% 
     arrange(desc(coef_mean)) %>% 
     mutate(coef_mean = normalize(coef_mean)) %>% 
     mutate(lag =  as.numeric(sapply(var, function(x) {sub(".*_lag(\\d+)$", "\\1", x)})),
            vars_raw = sub("_lag\\d+$","",var))
   
   
   coef_df = left_join(predictors_name,coef_mean_df, by ="vars_raw") %>% 
     mutate(modelname = modelname)
   
   coef_all = rbind(coef_all,coef_df)
}



coef_all %>% filter(modelname == "rf")

p_coef =
  coef_all %>% 
  drop_na() %>% 
  mutate(predictor = factor(predictor, levels = c("Historical case number", "7-day smoothed Ct value",
                                                  "Stringency index", "Government response index","ContainmentHealth index",
                                                  "Temperature","Absolute humidity","Wind speed",
                                                  "Ozone","Nitrogen dioxide","Nitrogen oxides","Carbon monoxide",
                                                  "Fine suspended particulates",'Respirable suspended particulates',"Sulphur dioxide")),
         modelname = factor(modelname,levels = c("arima","garch","gbm","lm","svr","rf","xgb","gru"),
                            labels = c("ARIMA","GARCH","GBM","LM","SVR","RF","XGB","GRU"))) %>% 
  ggplot(aes(y= predictor, fill=coef_mean, x= lag)) + 
  geom_tile() + ylab(NULL) + xlab(NULL) +
  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  #scale_fill_gradient(low ="white",high = "#CD3700") +
  scale_x_continuous("Lag Order",breaks = c(1,3,5,7))+
  scale_y_discrete("Predictors")+
  labs(fill = "Feature Importance")+
  facet_wrap(~modelname, nrow = 2)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12,colour = "black"),
        axis.text.y = element_text(size = 11,colour = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14,face = "bold"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 15, face = 'bold',colour = "black",vjust= -1),
        axis.title.y = element_text(size = 15, face = 'bold',colour = "black"),
        plot.title = element_text(size = 20, face = 'bold',colour = "black"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 13, colour = "black",face = "bold",vjust = 1),
        legend.box.margin = ggplot2::margin(r = 1,unit = "cm"),
        legend.key.width  = unit(1.5, "cm"),
        ##plot.title.position = 'plot',
        legend.position = "bottom")


ggsave(p_coef, file = "./figures/figure_varimp.pdf",dpi = 300,width = 11, height = 7)
ggsave(p_coef, file = "./figures_png/figure_varimp.png",dpi = 300,width = 11, height = 7)

