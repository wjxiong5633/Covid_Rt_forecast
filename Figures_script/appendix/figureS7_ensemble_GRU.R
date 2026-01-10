
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
library(scoringutils)


file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)


modelname_clean =  c("BASELINE","ARIMA","GAM","GARCH","GBM","GPR","GRU", "LM","RF","SVR","XGB","ENSEMBLE")

pal<-c("black", brewer.pal(10,"Paired"),brewer.pal(6, "Dark2")[6],"turquoise")
val_size<-c(rep(0.5,(length(modelname_clean)-1)),c(1))
val_linetype<-c("dashed", rep("solid",length(modelname_clean)-1))
lab_mod<-modelname_clean
val_linewidth = c(rep(0.5, 12), c(1))
val_point_size = c(rep(1.3, 12), c(1.8))

val_col = c("BASELINE" = pal[1],"ARIMA" = pal[2],"GAM"= pal[3], "GARCH"  = pal[4],   "GBM" = pal[5],
            "GPR" = pal[6],   "GRU"  = pal[7],    "LM"  = pal[8],    "RF"  = pal[9] ,    "SVR"  = pal[10],
            "XGB" = pal[11],"CONST" = pal[12], "ENSEMBLE" = pal[13] )


model_name_list = c("const","arima","gam","garch","gbm","gpr","gru","lm","rf","svr","xgb","ensemble")  ## lasso
modelname_clean =  c("BASELINE","ARIMA","GAM","GARCH","GBM","GPR","GRU", "LM","RF","SVR","XGB","ENSEMBLE")

index_list = c('rmse','mape','wis','mae','smape')

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



case.interval = case.interval %>% mutate(true = true_reportcase, point = pred_reportcase) %>% filter(proj<=21)

res.interval<-my.compute(case.interval)

case.result.interval <-res.interval %>%
  mutate(cov_true = 1) %>%
  group_by(modelname,proj) %>%
  dplyr::summarize(rmse = sqrt(mean(abs_error^2,na.rm=T)) ,
                   smape = (mean(abs_error/(abs(point)+abs(true))*2,na.rm=T))
                   ,mape = (mean(abs_error/abs(true),na.rm=T))
                   ,mae = mean(abs_error, na.rm=T)
                   ,wis = (mean(wis,na.rm=T)))%>%
  ungroup() %>% 
  as.data.frame() %>%
  mutate(modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean, ordered = T))

case.res.index.abs = case.result.interval


case.long.result = 
  case.result.interval %>% 
  pivot_longer(rmse:wis,names_to = "variable",values_to = "value") 

case.relative = case.long.result %>% 
  group_by(proj) %>% 
  mutate(relative_index = value/value[modelname== "BASELINE"]) %>% 
  group_by(variable,proj) %>% 
  arrange(relative_index) %>% 
  mutate(rank = row_number())




ensemble_case_rank = case.relative %>% 
  filter(modelname %in% c("ENSEMBLE", "GRU"))


p1 = 
  ensemble_case_rank %>% 
  mutate(variable = factor(variable,levels = c("rmse","mape","wis","mae","smape"),
                           labels = c("RMSE","MAPE","WIS","MAE","SMAPE"))) %>% 
  ##filter(variable != "smape") %>% 
  ggplot(aes(x = proj, y = rank, col = modelname))+
  geom_point(size = 2)+
  facet_wrap(~variable)+
  labs(
    x= "Prediction horizon",
    y = "Rank",
    title = "Case Number"
  )+
  scale_color_manual("Model",values=val_col) +
  scale_y_continuous(breaks = c(1,4,7,10))+
  scale_x_continuous(breaks = c(0:14))+
  theme_bw()+
  theme( legend.text=element_text(size=10),
         legend.key.size = unit(5, "mm"),
         legend.title = element_text(size=10, face = 'bold'))+
  theme(axis.text.x = element_text(size=10, hjust = 0.5, vjust = .5,colour = "black"),
        axis.text.y = element_text(size=10, face = 'bold',color = "black"),
        axis.title.x = element_text(size=11, face = 'bold'),
        axis.title.y = element_text(size=11, face = 'bold'),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold", colour = "black"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        plot.title = element_text(face = "bold",size = 13),
        legend.position = "bottom"
  )






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



rt.interval = rt.interval %>% mutate(true = true_rt, point = pred_rt)
res.interval<-my.compute(rt.interval)

rt.result.interval <-res.interval %>%
  group_by(modelname,proj) %>%
  dplyr::summarize(rmse = sqrt(mean(abs_error^2,na.rm=T)) ,
                   smape = (mean(abs_error/(abs(point)+abs(true))*2,na.rm=T))
                   ,mape = (mean(abs_error/abs(true),na.rm=T))
                   ,mae = mean(abs_error, na.rm=T)
                   ,wis = (mean(wis,na.rm=T)))%>%
  ungroup() %>% 
  as.data.frame() %>%
  mutate(modelname = factor(modelname,levels=model_name_list,
                            labels = modelname_clean  ,
                            ordered=T))
rt.res.index.abs = rt.result.interval


rt.long.result = 
  rt.result.interval %>% 
  pivot_longer(rmse:wis,names_to = "variable",values_to = "value") 

rt.relative = rt.long.result %>% 
  group_by(proj) %>% 
  mutate(relative_index = value/value[modelname== "BASELINE"]) %>% 
  group_by(variable,proj) %>% 
  arrange(relative_index) %>% 
  mutate(rank = row_number())



ensemble_rt_rank = rt.relative %>% filter(modelname %in% c("ENSEMBLE","GRU"))


p2 = 
  ensemble_rt_rank %>% 
  mutate(variable = factor(variable,levels = c("rmse","mape","wis","mae","smape"),
                           labels = c("RMSE","MAPE","WIS","MAE","SMAPE"))) %>% 
  ##filter(variable != "smape") %>% 
  ggplot(aes(x = proj, y = rank, col = modelname))+
  geom_point(size = 2)+
  facet_wrap(~variable)+
  labs(
    x= "Prediction horizon",
    y = "Rank",
    title = "Effective reproductive number (Rt)"
  )+
  scale_color_manual("Model",values=val_col) +
  scale_y_continuous(breaks = c(1,4,7,10))+
  scale_x_continuous(breaks = c(0:7))+
  theme_bw()+
  theme( legend.text=element_text(size=10),
         legend.key.size = unit(5, "mm"),
         legend.title = element_text(size=10, face = 'bold'))+
  theme(axis.text.x = element_text(size=10, hjust = 0.5, vjust = .5,color = "black"),
        axis.text.y = element_text(size=10, face = 'bold',color = "black"),
        axis.title.x = element_text(size=11, face = 'bold'),
        axis.title.y = element_text(size=11, face = 'bold'),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold", colour = "black"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        plot.title = element_text(face = "bold",size = 13),
        legend.position = "bottom"
  )



p_all = ggarrange(p1,p2, nrow = 2,labels = c("A","B"),align = "hv",common.legend =TRUE,legend = "bottom")+
  theme( plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p_all

ggsave(p_all, file = "./figures_clean/figure_S6_ENSEMBLE_GRU.pdf",dpi = 300, width = 10, height = 10)
#ggsave(p_all, file = "./figures_png/figure_ENSEMBLE_GRU.jpg",dpi = 300,width = 10, height = 10)




