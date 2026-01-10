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


case.interval = case.interval %>% mutate(true = true_reportcase, point = pred_reportcase) %>% filter(proj<=14)



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

res.index.relative = case.long.result

case.rank = case.long.result %>%
  group_by(modelname, variable) %>%
  dplyr::summarize(avg_value = mean(value, na.rm = TRUE)) %>%
  group_by(variable) %>% 
  mutate(relative_index = avg_value/avg_value[modelname== "BASELINE"]) %>% 
  modify_if(~is.numeric(.), ~round(., 3)) %>%
  mutate(rank = 0) %>%
  arrange(variable,relative_index)


for(v in unique(case.rank$variable)){
  case.rank$rank[which(case.rank$variable == v)] = rank(case.rank$relative_index[which(case.rank$variable == v)], ties.method = 'min')
}
# case.rank$rank <- ave(1:nrow(case.rank), case.rank$variable,FUN=rank)
case.rank$model2<-factor(case.rank$modelname,levels=rev(modelname_clean),ordered=T) 
case.rank$variable = factor(case.rank$variable,levels=index_list,ordered=T)

text.size = c(rep(10, (length(modelname_clean)-2)), c(10,10))
test.color = c(rep('black', (length(modelname_clean)-2)), rep('black',2))

val.rank<-case.rank %>% 
  mutate(variable = factor(variable,levels = c("rmse","mape","wis","mae","smape"),
                           labels = c("RMSE","MAPE","WIS","MAE","SMAPE"))) %>% 
  ggplot(aes(y= model2, fill=rank, x= variable)) + 
  geom_tile() + ylab(NULL) + xlab(NULL) +
  geom_text(aes(label=round(relative_index, 2)), size=3) +
  theme_bw()+
  labs(
    title = "Model performance relative to baseline for case number"
  )+
  scale_fill_gradient(name = "Rank of\nModel Performance",low ="darkgreen",high = "white", 
                      breaks = c(1,5,10),
                      limits=c(1,13)) +
  theme(axis.text.y = element_text(size=text.size, 
                                   face = 'bold', color = test.color, angle = 0),
        axis.text.x = element_text(size=11, face = 'bold',angle = 0),
        axis.title.x = element_text(size=11, face = 'bold'),
        axis.title.y = element_text(size=11, face = 'bold'),
        strip.background = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        plot.title = element_text(face = "bold",size = 13)
  )



val.rank

# ggsave(val.rank, file = "./figures/figure_error_avg_case.pdf",dpi = 300, width = 8, height = 7)
# ggsave(val.rank, file = "./figures_png/figure_error_avg_case.png",dpi = 300,width = 8, height = 7)

################################################################################
# plot
################################################################################
# #### -- week
# week_list = c(0,2,4,6,8,pred_horizon-1+0.05)
# res.index.abs = res.index.abs %>%
#   filter(week_ahead %in% week_list)
# res.index.relative = res.index.relative %>%
#   filter(week_ahead %in% week_list)
### --- plot

pal<-c("black",brewer.pal(10,"Paired"),"turquoise")
val_size<-c(rep(0.5,(length(modelname_clean)-1)),c(0.8))
val_linetype<-c("dashed", rep("solid",length(modelname_clean)-1))
lab_mod<-modelname_clean
val_linewidth = c(rep(0.5, (length(modelname_clean)-1)), c(1))
val_point_size = c(rep(1.3, (length(modelname_clean)-1)), c(1.8))

val_col = c("BASELINE" = pal[1],"ARIMA" = pal[2],"GAM"= pal[3], "GARCH"  = pal[4],   "GBM" = pal[5],
            "GPR" = pal[6],   "GRU"  = pal[7],    "LM"  = pal[8],    "RF"  = pal[9] ,    "SVR"  = pal[10],
            "XGB" = pal[11],"ENSEMBLE" = pal[12])


need_legend = 
  case.relative %>% 
  mutate(variable = factor(variable,levels = c("rmse","mape","wis","mae","smape"),
                           labels = c("RMSE","MAPE","WIS","MAE","SMAPE"))) %>% 
  ##filter(variable != "smape") %>% 
  ggplot() +
  geom_line(aes(x = proj, y = value, color = modelname,
                linetype=modelname, size = modelname)) +
  geom_point(aes(x = proj, y = value, color = modelname),size = 2) +
  facet_wrap(~variable,scales = "free")+
  scale_color_manual("Model",values=val_col) +
  scale_size_manual("Model",labels=lab_mod,values=val_linewidth) +
  scale_linetype_manual("Model",labels=lab_mod,values=val_linetype)+
  theme_bw()+
  theme( legend.title =element_text(size=11, face = 'bold'),
         legend.text=element_text(size=9, face = 'bold'),
         legend.key.size = unit(5, "mm"),
         legend.position = "bottom")+
  guides(color=guide_legend(title = "Model",nrow = 1)) 


leg = get_legend(need_legend)
metrics_value = 
  case.relative %>% 
  mutate(variable = factor(variable,levels = c("rmse","mape","wis","mae","smape"),
                           labels = c("RMSE","MAPE","WIS","MAE","SMAPE"))) %>% 
  ##filter(variable != "smape") %>% 
  ggplot() +
  geom_line(aes(x = proj, y = value, color = modelname,
                linetype=modelname, size = modelname)) +
  facet_wrap(~variable,scales = "free")+
  scale_color_manual("Model",values=val_col) +
  scale_size_manual("Model",labels=lab_mod,values=val_linewidth) +
  scale_linetype_manual("Model",labels=lab_mod,values=val_linetype)+
  theme_bw()+
  theme( legend.text=element_text(size=10, face = 'bold'),
         legend.key.size = unit(5, "mm"))+
  theme(axis.text.x = element_text(size=10, hjust = 0.5, vjust = .5, face = 'bold'),
        axis.text.y = element_text(size=10, face = 'bold'),
        axis.title.x = element_text(size=11, face = 'bold'),
        axis.title.y = element_text(size=11, face = 'bold'),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        plot.title = element_text(face = "bold",size = 13),
        legend.position = "None"
  )+
  labs(
    y = "Value",
    x = "Prediction horizon",
    title = "Model performance by prediction horizons for case number"
  )



proj.relative <-
  case.relative %>%
  mutate(variable = factor(variable,levels = c("rmse","mape","wis","mae","smape"),
                           labels = c("RMSE","MAPE","WIS","MAE","SMAPE"))) %>% 
  filter(rank == 1) %>% 
  ggplot() +
  geom_point(aes(x = proj, y = relative_index, color = modelname, group=variable), size = 2) +
  facet_wrap( ~ variable, nrow = 2, scale = "free")+
  scale_color_manual("Model",values=val_col) +
  scale_y_continuous(limits = c(0.7,1))+
  scale_x_continuous(breaks = c(0:14))+
  labs(
    y = "Relative value",
    x = "Prediction horizon",
    title = "Best-performing model at each prediction horizon for case number"
  )+
  theme_bw()+
  theme( legend.text=element_text(size=10, face = 'bold'),
         legend.key.size = unit(5, "mm"))+
  theme(axis.text.x = element_text(size=10, hjust = 0.5, vjust = .5, face = 'bold'),
        axis.text.y = element_text(size=10, face = 'bold'),
        axis.title.x = element_text(size=11, face = 'bold'),
        axis.title.y = element_text(size=11, face = 'bold'),
        plot.title = element_text(face = "bold",size = 13),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = "None")+
  guides(color=guide_legend(title = "Model")) 

proj.relative


library(ggpubr)

p_all = ggarrange(val.rank,metrics_value,proj.relative,as_ggplot(leg), nrow = 4,labels = c("A","B","C",""), heights = c(1,1,1,0.2))+
  theme( plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))
p_all

ggsave(p_all, file = "./figures_clean/Figure3_case_error.pdf",dpi = 300, width = 10, height = 12)

