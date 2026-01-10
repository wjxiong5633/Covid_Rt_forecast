

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
library(ggpubr)
library(patchwork)
library(scoringutils)
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
  filter(proj ==0) %>% distinct() %>% 
  filter(mytestdate<="2022-12-31")

### plot case traj

val_p<-c("powderblue","navajowhite",brewer.pal(10,"Paired")[5]) ##"#FFBBFF",
val_p2<-c("turquoise4","burlywood4",brewer.pal(10,"Paired")[6])# "#68228B",
ensemble =   case.interval %>%
  mutate(date = as.Date(date),
         mytestdate = as.Date(mytestdate),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T)) %>% 
  filter(modelname == 'ENSEMBLE')


p1 = 
  case.interval %>%
  mutate(date = as.Date(date),
         mytestdate = as.Date(mytestdate),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T),
         proj = factor(proj,levels = rev(0:14),labels = rev(0:14))) %>% 
  filter(modelname == 'ENSEMBLE' & proj %in% c(0,7,14)) %>% 
  filter(date>="2022-07-01" & date<="2022-12-31") %>% 
  ggplot() +
  ggnewscale::new_scale_fill()+
  geom_ribbon(aes(x= date, ymin= lower_10, ymax= upper_10, fill=as.factor(proj)),alpha=0.5)+
  geom_line(aes(x = date, y = pred_reportcase, color = as.factor(proj)),size = 0.8) +
  geom_line(data = true_case, aes(x = date, y = true_reportcase),size = 0.8) +
  labs(
    x = "Date",
    y = "Case Number"
  )+
  theme_bw()+
  scale_y_continuous(limits = c(0, 8500)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.text.x = element_text(angle = 0, hjust=0.5),
        axis.title.x = element_text(size=10, vjust = 0.3))+
  scale_color_manual("Prediction horizon", values = val_p2) +
  scale_fill_manual("Prediction horizon", values = val_p) +
  scale_size_manual("",labels="Data",values=0.8) +
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
  guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE))

p1

p2 = 
  case.interval %>% 
  mutate(date = as.Date(date),
         mytestdate = as.Date(mytestdate),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T)) %>% 
  filter(modelname == "ENSEMBLE") %>% 
  filter(date<="2022-12-31") %>% 
  ggplot(aes(x = date))+
  #geom_col(aes(y = true_reportcase), fill = "grey", alpha = 0.5, data = true_case) + # Use geom_col for pre-aggregated values
  geom_line(aes(y = pred_reportcase,group = mytestdate ,color = factor(proj)),lwd = 0.8)+
  geom_line(aes(y = true_reportcase), lwd = 0.8,color = "black",data = true_case)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(0, 8000)) +
  theme_bw()+
  labs(
    y = "Case Number",
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


p_ensemble_case = ggarrange(p1,p2,nrow = 2, labels = c("A","C"))






model_name_list = c("const1","const","arima","gam","garch","gbm","gpr","gru","lm","rf","svr","xgb",'ensemble')#,'ensemble2')
modelname_clean =  c("BASELINE","CONST","ARIMA","GAM","GARCH","GBM","GPR","GRU","LM","RF","SVR","XGB","ENSEMBLE")#,"ENSEMBLE2") 
index_list = c('rmse','mape','wis','mae','smape')



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



### plot case traj

val_p<-c("powderblue","navajowhite",brewer.pal(10,"Paired")[5])#"#FFBBFF",
val_p2<-c("turquoise4","burlywood4",brewer.pal(10,"Paired")[6])#"#68228B",
ensemble =   rt.interval %>%
  mutate(date = as.Date(date),
         date_analysis = as.Date(date_analysis),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T)) %>% 
  filter(modelname == 'ENSEMBLE')


p1 = 
  rt.interval %>%
  mutate(date = as.Date(date),
         date_analysis = as.Date(date_analysis),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T),
         proj = factor(proj,levels = rev(0:7),labels = rev(0:7))) %>% 
  filter(modelname == 'ENSEMBLE' & proj %in% c(0,3,7)) %>% 
  filter(date>="2022-07-01" & date<="2022-12-31") %>% 
  ggplot() +
  ggnewscale::new_scale_fill()+
  geom_ribbon(aes(x= date, ymin= lower_10, ymax= upper_10, fill=as.factor(proj)),alpha=0.5)+
  geom_line(aes(x = date, y = pred_rt, color = as.factor(proj)),size = 0.8) +
  geom_line(data = true_rt, aes(x = date, y = true_rt),size = 0.8) +
  labs(
    x = "Date",
    y = "Rt"
  )+
  theme_bw()+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme(axis.text.x = element_text(angle = 0, hjust=0.5),
        axis.title.x = element_text(size=10, vjust = 0.3))+
  scale_color_manual("Prediction horizon", values = val_p2) +
  scale_fill_manual("Prediction horizon", values = val_p) +
  scale_size_manual("",labels="Data",values=0.8) +
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
  guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE))

p1

p2 = 
  rt.interval %>% 
  mutate(date = as.Date(date),
         date_analysis = as.Date(date_analysis),
         modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T)) %>% 
  filter(modelname == "ENSEMBLE") %>% 
  filter(date<="2022-12-31") %>% 
  ggplot(aes(x = date))+
  #geom_col(aes(y = true_rt), fill = "grey", alpha = 0.5, data = true_case) + # Use geom_col for pre-aggregated values
  geom_line(aes(y = pred_rt,group = date_analysis ,color = factor(proj)),lwd = 0.8)+
  geom_line(aes(y = true_rt), lwd = 0.8,color = "black",data = true_rt)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme_bw()+
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


p_ensemble_rt = ggarrange(p1,p2,nrow = 2, labels = c("B","D")) 


p = ggarrange(p_ensemble_case,p_ensemble_rt,ncol = 2) +
  theme(   plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(p, file = "./figures_clean/Figure5_ensemble.pdf",dpi = 300,width = 13, height = 10)


p
