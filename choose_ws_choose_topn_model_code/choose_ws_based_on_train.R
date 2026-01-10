


###-------------------------------------- Evaluate window sizes

library(zoo)
library(tidyverse)
# Function to evaluate and select the best window size
evaluate_best_window_size <- function(data, window_sizes, target_coverage = 0.95, tolerance = 0.02) {
  results <- data.frame(window_size = integer(), coverage = numeric(), avg_width = numeric())
  
  for (w in window_sizes) {
    # Calculate rolling standard deviation
    data = data %>% 
      group_by(proj) %>% 
      mutate(pred_sd = zoo::rollapply(pred, w, sd, fill = NA, align = "right"),
             lower = pred - 1.96 * pred_sd,
             upper = pred + 1.96 * pred_sd)
    
    # data$pred_sd <- zoo::rollapply(data$pred, w, sd, fill = NA, align = "right")
    # # Calculate prediction interval
    # data$lower <- data$pred - 1.96 * data$pred_sd
    # data$upper <- data$pred + 1.96 * data$pred_sd
    
    # Coverage calculation
    in_interval <- data$true >= data$lower & data$true <= data$upper
    coverage <- mean(in_interval, na.rm = TRUE)
    avg_width <- mean(data$upper - data$lower, na.rm = TRUE)
    
    # Store results
    results <- rbind(results, data.frame(window_size = w, coverage = coverage, avg_width = avg_width))
  }
  return(results)
}



window_sizes <- seq(5, 30, 5)
model_name_list =  c("const","arima","garch","gam","lm","svr","lasso","gpr","gru","rf","gbm","xgb") ##c("arima","garch","gam","lm","rf","xgb","svr","lasso","gpr","gru","const")
ws_res = tibble()
ws_res_list = lapply(model_name_list,function(modelname){
  print(modelname)
  train_res = read.csv(paste("./train_choose_ws/",modelname,"_train.csv",sep = "")) 
  
  # 
  # valid_res = read.csv(paste("./pred_reportcase_point/",modelname,"_reportcase_point.csv",sep = "")) %>%
  #   filter(mytestdate <= "2022-06-30") %>%
  #   mutate(modelname = modelname,
  #          pred = pred_reportcase,
  #          true = true_reportcase) %>% 
  #   dplyr::select(mytestdate, date, pred, true )
  
  # ws_res_df = 
  #   train_res %>% 
  #   group_by(proj) %>%
  #   group_modify(~ evaluate_best_window_size(.x,window_sizes))
  
  ws_res_df = evaluate_best_window_size(train_res, window_sizes) %>% 
    mutate(modelname = modelname)
  return(ws_res_df)
})
  

ws_res_all = ws_res_list %>% list.rbind()
p_ws = 
  ws_res_all %>% 
  ggplot(aes(x = window_size, y = coverage))+
  geom_line(size =1, color = "black")+
  facet_wrap(~modelname)+
  labs(
    y = "Coverage",
    x = "Window size"
  )+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12,colour = "black"),
        axis.text.y = element_text(size = 12,colour = "black"),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 13,face = "bold"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(-0.5,0.7,0,0.2), "cm"),
        axis.title.x = element_text(size = 16, face = 'bold',colour = "black",vjust= -1),
        axis.title.y = element_text(size = 16, face = 'bold',colour = "black"),
        plot.title = element_text(size = 20, face = 'bold',colour = "black"),
        legend.text = element_text(size = 15, colour = "black"),
        legend.title = element_text(size = 15, colour = "black",face = "bold"),
        ##plot.title.position = 'plot',
        legend.position = "bottom")


p_ws
ggsave(p_ws, file = "./figures/figure_windowsize.pdf",dpi = 300,width = 8, height = 8)



best_ws = lapply(ws_res_list,function(results){
  target_coverage  = 0.95
  # Filter for coverage close to target
  best_result <- results %>% filter(coverage >= target_coverage)
  
  if (nrow(best_result) == 0) {
    return("No window size found within tolerance.")
  } else {
    # Return the smallest window size that meets the criteria
    return(best_result[which.min(best_result$avg_width), "window_size"])
  }
}
) %>% list.rbind()


best_ws_df = data.frame(modelname = model_name_list,best_ws = best_ws)
write.csv(best_ws_df,"./window_size/best_ws_df.csv",row.names = F)

