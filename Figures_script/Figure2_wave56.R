
library(tidyverse)
library(gridExtra)
library(cowplot)
library(ggpubr)

file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)


data_case = readRDS("./data/data_full_new_bp.rds") %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date,confirm,rt,rt_lwr, rt_upr) %>% 
  distinct()

wave5 = data_case %>% 
  filter(date>= "2022-01-01" & date<= "2022-05-31") %>% 
  mutate(rt_upr = ifelse(rt_upr>=6,6,rt_upr))

scaling_factor5 = 15000
p3 = wave5 %>% 
  ggplot( aes(x = date, y = confirm)) +
  geom_col(fill = "skyblue", color = "black") + # Use geom_col for pre-aggregated values
  scale_x_date(date_labels = "%Y-%m", date_breaks = "month") + # Format dates
  geom_line(aes(y = rt * scaling_factor5),col = "firebrick")+
  geom_ribbon(aes(ymin = rt_lwr * scaling_factor5, ymax = rt_upr * scaling_factor5),fill = "salmon", alpha = 0.3)+
  geom_line(y = scaling_factor5, col = "Salmon", linetype = 2,size = 0.8)+
  scale_y_continuous(
    name = "Confirmed Cases",  # Left axis
    sec.axis = sec_axis(~ . / scaling_factor5, name = "Rt")  # Right axis
  )+
  labs(
    x = "Date",
    title = "Wave 5"
  )+
  theme_bw() +
  theme(axis.text.x = element_text(size = 10,colour = "black"),
        axis.text.y = element_text(size = 13,colour = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        strip.text = element_text(size = 13, face = "bold", colour = "black"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 15, face = 'bold',colour = "black",vjust = -1),
        axis.title.y = element_text(size = 15, face = 'bold',colour = "black"),
        plot.title = element_text(size = 16, face = 'bold',colour = "black"),
        legend.text = element_text(size = 15, colour = "black"),
        legend.title = element_text(size = 15, colour = "black"),
        legend.position = "bottom")

p3


wave6 = data_case %>% 
  filter(date<= "2022-12-31" & date>= "2022-06-01")

scaling_factor6 = max(wave6$confirm)/max(wave6$rt)
p4 = wave6 %>% 
  ggplot( aes(x = date, y = confirm)) +
  geom_col(fill = "skyblue", color = "black") + # Use geom_col for pre-aggregated values
  scale_x_date(date_labels = "%Y-%m", date_breaks = "month") + # Format dates
  geom_line(aes(y = rt * scaling_factor6),col = "firebrick")+
  geom_ribbon(aes(ymin = rt_lwr * scaling_factor6, ymax = rt_upr * scaling_factor6),fill = "salmon", alpha = 0.3)+
  geom_line(y = scaling_factor5, col = "Salmon", linetype = 2,size = 0.8)+
  scale_y_continuous(
    name = "Confirmed Cases",  # Left axis
    sec.axis = sec_axis(~ . / scaling_factor6, name = "Rt")  # Right axis
  )+
  labs(
    x = "Date",
    title = "Wave 6"
  )+
  theme_bw() +
  theme(axis.text.x = element_text(size = 10,colour = "black"),
        axis.text.y = element_text(size = 13,colour = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        strip.text = element_text(size = 13, face = "bold", colour = "black"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 15, face = 'bold',colour = "black",vjust = -1),
        axis.title.y = element_text(size = 15, face = 'bold',colour = "black"),
        plot.title = element_text(size = 16, face = 'bold',colour = "black"),
        legend.text = element_text(size = 15, colour = "black"),
        legend.title = element_text(size = 15, colour = "black"),
        legend.position = "bottom")

p4

p_all = ggarrange(p3,p4, nrow =2, ncol = 1, labels = c("A","B","C","D"))

p_all
ggsave(p_all, file = "./figures_clean/Figure2_wave5to6.pdf",dpi = 300,width = 10, height = 10)
