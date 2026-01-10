
### train res
model_name_list = c("arima","lm","garch")
coef_all = NULL
for (modelname in model_name_list){
  coef_df_all = read.csv(paste("./train_coef/",modelname,"_coef_all.csv",sep = ""))
  
  coef_mean_df = 
    coef_df_all %>% 
    filter(!var %in% c("(intercept)","mu","ma1","omega","beta1")) %>% 
    group_by(var) %>% 
    dplyr::summarise(n = n(),
                     coef_mean = mean(coef),
                     sd = sd(coef)/sqrt(n())) %>% 
    mutate(CI_lwr = coef_mean - qt(0.975, df = n-1) * sd,
           CI_upr = coef_mean + qt(0.975, df = n-1) * sd) %>% 
    mutate(lag =  as.numeric(sapply(var, function(x) {sub(".*_lag(\\d+)$", "\\1", x)})),
           vars_raw = sub("_lag\\d+$","",var))
  
  
  coef_df = left_join(predictors_name,coef_mean_df, by ="vars_raw") %>% 
    mutate(modelname = modelname)
  
  coef_all = rbind(coef_all,coef_df)
}
  


p_CI = coef_all %>% 
  drop_na() %>% 
  mutate(predictor = factor(predictor, levels = c("Historical case number", "7-day smoothed Ct value",
                                                  "Stringency index", "Government response index","ContainmentHealth index",
                                                  "Temperature","Absolute humidity","Wind speed",
                                                  "Ozone","Nitrogen dioxide","Nitrogen oxides","Carbon monoxide",
                                                  "Fine suspended particulates",'Respirable suspended particulates',"Sulphur dioxide")),
         modelname = factor(modelname,levels = c("arima","garch","lm"),
                            labels = c("ARIMA","GARCH","LM"))) %>% 
  ggplot(aes(x = lag, y = coef_mean)) +
  geom_point(size = 1.5) +
  geom_pointrange(aes(ymin=CI_lwr,ymax=CI_upr,color = modelname),size = 0.3)+
  #geom_errorbar(aes(ymin = CI_lwr, ymax = CI_upr), width = 0.1) +
  facet_wrap(~ predictor, scales = "free_y", nrow = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +  # Reference line
  scale_x_continuous(breaks = c(1,3,5,7))+
  scale_color_discrete(name = "Modelname")+
  labs(
    x = "Lag Order",
    y = "Coefficient Estimate",
    title = "Coefficients with 95% Confidence Intervals"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12,colour = "black"),
        axis.text.y = element_text(size = 11,colour = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
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



ggsave(p_CI, file = "./figures/figure_var_CI.pdf",dpi = 300,width = 12, height =11)
ggsave(p_CI, file = "./figures_png/figure_var_CI.jpg",dpi = 300,width = 12, height = 11)

