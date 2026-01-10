

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

model_name_list = c("const","arima","gam","garch","gbm","gpr","gru","lm","rf","svr","xgb","ensemble")  ## lasso
modelname_clean =  c("BASELINE","ARIMA","GAM","GARCH","GBM","GPR","GRU", "LM","RF","SVR","XGB","ENSEMBLE")


interval_files = list.files( c("./pred_reportcase_interval"),
                             pattern="*.csv$", 
                             full.names=TRUE, 
                             ignore.case=TRUE)

case.interval <-tibble()
for(modelname in model_name_list){
  print(modelname)
  model.interval = read.csv(paste("./pred_reportcase_interval/","interval_",modelname,".csv",sep = "")) %>% 
    mutate(modelname = modelname) %>% 
    dplyr::select(modelname,date,mytestdate,proj,true_reportcase,pred_reportcase,starts_with("lower"),starts_with("upper")) %>% 
    mutate(mytestdate = as.Date(mytestdate)) %>% 
    filter(mytestdate %in% seq(as.Date("2022-07-01"),as.Date("2022-12-31"),by = "day"))
  
  case.interval = rbind(case.interval,model.interval)
}

true_case = case.interval %>% 
  mutate(date = as.Date(date),
         mytestdate = as.Date(mytestdate),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T)) %>% 
  filter(proj ==0) %>% distinct()

### plot case traj
p1 = case.interval %>% 
  mutate(date = as.Date(date),
         mytestdate = as.Date(mytestdate),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T)) %>% 
  #filter(modelname == "RF") %>% 
  ggplot(aes(x = date))+
  #geom_col(aes(y = true_reportcase), fill = "grey", alpha = 0.5, data = true_case) + # Use geom_col for pre-aggregated values
  geom_line(aes(y = pred_reportcase,group = mytestdate ,color = factor(proj)),lwd = 0.8)+
  geom_line(aes(y = true_reportcase), lwd = 0.8,color = "darkgrey")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme_bw()+
  facet_wrap(~modelname, nrow = 6)+
  labs(
    y = "Case",
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
ggsave(p1, file = "./figures/figure_eachmodel_traj_case.pdf",dpi = 300,width = 14, height = 14)
ggsave(p1, file = "./figures_png/figure_eachmodel_traj_case.png",dpi = 300,width = 14, height = 14)

