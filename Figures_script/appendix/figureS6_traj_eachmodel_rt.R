

################################################################################
library(plyr)
library(dplyr)
library(tidyverse)
library(ISOweek)
library(locpol)
library(forecast)
library(scoringutils)
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


model_name_list = c("const1","const","arima","gam","garch","gbm","gpr","gru","lm","rf","svr","xgb",'ensemble')#,'ensemble2')
modelname_clean =  c("BASELINE","CONST","ARIMA","GAM","GARCH","GBM","GPR","GRU","LM","RF","SVR","XGB","ENSEMBLE")#,"ENSEMBLE2") 
index_list = c('rmse','mape','wis','mae','smape')

interval_files = list.files( c("./pred_rt_interval"),
                             pattern="*.csv$", 
                             full.names=TRUE, 
                             ignore.case=TRUE)



rt.interval <-tibble()
for(modelname in model_name_list){
  print(modelname)
  model.interval = read.csv(paste("./pred_rt_interval/","interval_",modelname,".csv",sep = "")) %>% 
    dplyr::select(modelname,date,date_analysis,proj,true_rt,pred_rt, 
                  starts_with("lower"),starts_with("upper")) %>% 
    mutate(date_analysis = as.Date(date_analysis)) %>% 
    filter(date_analysis %in% seq(as.Date("2022-07-01"),as.Date("2022-12-31"),by = "day"))
  
  rt.interval = rbind(rt.interval,model.interval)
}


rt.interval = rt.interval %>% filter(proj<=7)



true_rt = rt.interval %>% 
  mutate(date = as.Date(date),
         date_analysis = as.Date(date_analysis),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T)) %>% 
  filter(proj ==0) %>% 
  dplyr::select(date,date_analysis,true_rt) %>% 
  distinct()


### plot rt traj
p1 = rt.interval %>% 
  mutate(date = as.Date(date),
         date_analysis = as.Date(date_analysis),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T)) %>% 
  #filter(modelname == "RF") %>% 
  ggplot(aes(x = date))+
  #geom_col(aes(y = true_rt), fill = "grey", alpha = 0.5, data = true_rt) + # Use geom_col for pre-aggregated values
  geom_line(aes(y = pred_rt,group = date_analysis ,color = factor(proj)),lwd = 0.8)+
  geom_line(aes(y = true_rt), lwd = 0.8,color = "darkgrey")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme_bw()+
  facet_wrap(~modelname, nrow = 7)+
  labs(
    y = "Rt",
    x = "Date"
  )+
  scale_color_discrete(name = "Prediction horizon")+
  theme(axis.text.x = element_text(size = 10,colour = "black"),
        axis.text.y = element_text(size = 13,colour = "black"),
        #panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold", colour = "black"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 15, face = 'bold',colour = "black",vjust = -1),
        axis.title.y = element_text(size = 15, face = 'bold',colour = "black"),
        plot.title = element_text(size = 16, face = 'bold',colour = "black"),
        legend.text = element_text(size = 15, colour = "black"),
        legend.title = element_text(size = 15, colour = "black",face = "bold"),
        legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2,byrow = T))



p1
ggsave(p1, file = "./figures/figure_eachmodel_traj_rt.pdf",dpi = 300,width = 14, height = 14)
ggsave(p1, file = "./figures_png/figure_eachmodel_traj_rt.png",dpi = 300,width = 14, height = 14)

