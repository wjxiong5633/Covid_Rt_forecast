library(tidyverse)
library(randomForest)
library(gbm)
library(xgboost) 
library(glmnet)
library(kernlab)
library(e1071)
library(mgcv)
library(pROC)
library(Rcpp)
library(RcppParallel)
library(chron)
library(mvtnorm)
library(incidental)
library(surveillance)
file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)
set.seed(123)
x = c(seq(1,1000,length = 100))
y = stats::rnorm(length(x),x,1) %>% round()
mydata = data.frame(date = 1:100, case = y)

## suppose after 14 days we know the true data 
undereport= c(0,1,1,1,5,5,5,5,10,10,10,15,15,15)
undereport_p <<- undereport/sum(undereport)

select_dat = mydata
data_lst = list()
rt_list = list()

## Cut off 60 days
sourceCpp("rt.cpp")
for(i in 0:14) {
  dat = select_dat %>% filter(date<= 60 +i & date> 60 +i - 14)
  under_case = dat$case*undereport_p
  dat$newcase = round(dat$case - under_case)
  
  df = left_join(select_dat,dat, by = c("date","case")) %>% filter(date<= 60 +i)
  df$newcase = ifelse(is.na(df$newcase),df$case,df$newcase)
  data_lst[[i+1]] = df
}
saveRDS(data_lst,"./sim/data_lst.rds")

for(i in 0:14) {
  df = data_lst[[i+1]]
  res = run_rt_true(df)
  result = data.frame(date =res$date, local.rt.mean = res$local.rt.mean) %>% 
    rename(setNames("local.rt.mean", paste("later_",i,"_day",sep = "")))
  rt_list[[i+1]] = result
}

saveRDS(rt_list,"./sim/rt_list.rds")
rt_list = read_rds("./sim/rt_list.rds")
rawdata = mydata
rawdata$newcase = rawdata$case
res_all = run_rt_true(rawdata)
all_data_rt  = res_all %>% 
  dplyr::select(data.onset,local.rt.mean) %>% 
  rename(setNames("local.rt.mean", "true_rt"),
         "date" = "data.onset")
write.csv(all_data_rt,"./sim/true_rt.csv",row.names = F)

all_data_rt = read.csv("./sim/true_rt.csv")
all = full_join(all_data_rt,rt_list %>% reduce(full_join, by='date'),by = "date")
write.csv(all,"./sim/all_rt.csv",row.names = F)
all_rt = read.csv("./sim/all_rt.csv") 

all_long = 
  all_rt %>% 
  pivot_longer(
    later_0_day:later_14_day,
    names_to = "delay",
    values_to = "rt") %>% 
  mutate(
    delay = ifelse(delay == "true_rt","true_rt",gsub(".*[_]([^.]+)[_].*", "\\1", delay))
  ) 

data_all = read_rds("./sim/data_lst.rds") %>% 
  reduce(full_join, by=c('date','case')) 
colnames(data_all)  = c("date","case",paste("later_",0:14,"_day",sep = ""))

data_long = 
  data_all %>% 
  pivot_longer(
    later_0_day:later_14_day,
    names_to = "delay",
    values_to = "delay_case") %>% 
  mutate(
    delay = ifelse(delay == "true_rt","true_rt",gsub(".*[later_]([^.]+)[_].*", "\\1", delay))
  ) 

a = 
  data_long %>% 
  group_by(date) %>% 
  mutate(
    increase_case = c(dplyr::first(delay_case), diff(delay_case))
  )

df_final = left_join(all_long,a,by= c("date","delay")) %>% 
  filter(date>46 & date<= 60) %>% 
  mutate(
    real_date = as.Date("2020-01-01") + date -47,
    date_analysis = real_date + as.numeric(delay),
    delay_num = delay,
    delay = factor(delay,levels = c(14:0),labels = format(as.Date("2020-01-01")+ 27:13,format="%m/%d"))
  )

df_case = df_final %>% filter(date<=60) %>% 
  dplyr::select(date,case) %>% distinct()



###----------------------------------------plot ----------------------------------------
library(latex2exp)
fold = 400
mycolors = c("#006ACC", "#1272C0", "#2479B3", "#3681A7", "#47899B", "#59908E", "#6B9882",  
             "#7DA076", "#8FA769", "#A1AF5D", "#B3B650", "#C4BE44","#D6C638", "#E8CD2B",  "#FAD51F")
pp = ggplot() +
  geom_bar(aes(x = real_date, y = increase_case/fold, group = delay, fill = delay), alpha = 0.5, stat = "identity",position = "stack",
           data =df_final)+
  geom_line(aes(x = real_date, y = true_rt), lwd = 2, col = "red", data = df_final)+
  geom_line(aes(x = real_date, y = rt, group = delay, color = delay),data = df_final)+
  #scale_color_gradient(low = "yellow", high = "darkblue") + 
  scale_fill_manual(name = "Date of analysis", values=setNames(mycolors,  format(as.Date("2020-01-01")+ 13:27,format="%m/%d"))) +
  scale_color_manual(name = "Date of analysis", values=setNames(mycolors, format(as.Date("2020-01-01")+ 13:27,format="%m/%d"))) +
  labs(x = "Date",
       y =  TeX("$\\R_t$")) +
  scale_y_continuous(
    breaks = c(1,1.5),  limits = c(1,1.5), oob =  scales::oob_keep,
    sec.axis = sec_axis(~ .*fold, 
                        name = 'Case')
  )+
  theme_bw()+
  theme(
    strip.background =element_rect(fill=ggplot2::alpha("darkblue",0.1)),
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(vjust = -2, size = 8),
    axis.title.y = element_text(vjust = 2, size = 9),
    legend.position = "bottom",
    legend.title=element_text(size = 8),
    legend.text =element_text(size = 8),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.title = element_text(size = 10),
    plot.margin =  ggplot2::margin(15,15,10,15,unit = "pt"),
  ) +
  guides(alpha ='none') +
  scale_x_date(date_breaks = "1 day",date_labels = "%m/%d")

pp

ggpubr::ggexport(pp,filename = "./figures_clean/Figure_S2_sim_delay.pdf",width=10, height=6)
