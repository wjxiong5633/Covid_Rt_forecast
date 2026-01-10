library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)
library(latex2exp)
library(ggpubr)
library(scales)


##-------------------------------------------------Panel A
sim_data = read_rds("./data/sim_data1.rds")

colors = c("Infections" = "#6c757d", 
           "Report" =  "#84C3b7",  
           "Deconv-BP-time150" = "#3a86ff",
           "Report-shift" =  "#ef476f",
           "Deconv-BP-time200" = "#ffb703")

types = c("Infections" = "dashed", 
          "Report" =  "solid", 
          "Deconv-BP-time150" =  "solid",  
          "Deconv-BP-time200" = "solid",
          "Report-shift" =   "solid")


dt_longer = 
  sim_data %>% 
  pivot_longer(-c("time"), names_to = "type", values_to = "case") %>% 
  mutate(type = factor(type, 
                       levels = c("true_case", "report_case", "BP_case", "BP_case_time200","report_shift_case"),
                       labels = c("Infections", "Report", "Deconv-BP-time150", "Deconv-BP-time200","Report-shift")))

bp_df = dt_longer %>% filter(type == "Deconv-BP-time150") 
reportshift_df = dt_longer %>% filter(type  == "Report-shift") %>% filter(!is.na(case))


p = 
  dt_longer %>% ggplot(aes(x = time, y = case,col = type,linetype = type))+
  geom_line(size = 0.8)+ scale_color_manual(name = "Type", values = colors)+
  scale_linetype_manual(name = "Type", values = types)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 13,face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.width =   unit(0.8,"cm"),
        legend.key.spacing = unit(0.4,"cm"))


p_legend = get_legend(p)

df1 = 
  sim_data %>% 
  mutate(
    x = max(bp_df$time)-10,
    x_end = 150,
    y = bp_df$case[bp_df$time == max(bp_df$time)-10],
    y_end = bp_df$case[bp_df$time == max(bp_df$time)-10])


p1 =
  df1 %>% 
  ggplot() +
  geom_line(aes(x = time, y = BP_case), color = "#3a86ff", linetype = "solid",size = 1) +
  geom_line(aes(x = time, y = report_case), color = "#84C3b7", linetype = "solid",size = 1) +
  geom_line(aes(x = time, y = true_case), color = "#6c757d", linetype = "dashed",size = 0.8) +
  geom_line(aes(x = time, y = BP_case_time200), color = "#ffb703", linetype = "solid",size = 1) +
  scale_x_continuous(
    breaks = c(0,50,100,150, 200), 
    limits = c(50, 150)
  ) +
  theme_bw() +
  labs(x = "Time", y = "Case", title = "Deconv-BP") +
  theme(legend.position = "bottom") +
  geom_vline(aes(xintercept = x), linetype = 2, col = "#E76F51", size = 1) +
  geom_vline(aes(xintercept = x_end), linetype = 2, col = "#E76F51", size = 1)+
  # geom_segment(
  #   aes(
  #     x = x,
  #     y = y,
  #     xend = x_end,
  #     yend = y_end
  #   ),     # Set color outside aes()
  #   col = "#E76F51",
  #   linewidth = 0.8,
  #   arrow = arrow(length = unit(0.02, "npc"), ends = "both"))+
  theme(axis.text.x = element_text(size = 14,colour = "black"),
        axis.text.y = element_text(size = 14,colour = "black"),
        panel.spacing = unit(1, "lines"),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9,face = "bold"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 18, face = 'bold',colour = "black"),
        axis.title.y = element_text(size = 18, face = 'bold',colour = "black"),
        plot.title = element_text(size = 15, face = 'bold',colour = "black",hjust = 0.5),
        ##plot.title.position = 'plot',
        legend.position = "None")

p1



df2 = 
  sim_data %>% 
  mutate(
    x = tail(reportshift_df,1)$time,
    x_end = 150,
    y = tail(reportshift_df,1)$case,
    y_end = tail(reportshift_df,1)$case)


p2 =
  df2 %>% 
  ggplot() +
  geom_line(aes(x = time, y = report_shift_case), color = "#ef476f", linetype = "solid",size = 1) +
  geom_line(aes(x = time, y = report_case), color = "#84C3b7", linetype = "solid",size = 1) +
  geom_line(aes(x = time, y = true_case), color = "#6c757d", linetype = "dashed",size = 1) +
  scale_x_continuous(
    breaks = c(0,50,100,150, 200), 
    limits = c(50, 150)
  ) +
  theme_bw() +
  labs(x = "Time", y = "Case", title =  "Report-shift") +
  theme(legend.position = "bottom") +
  geom_vline(aes(xintercept = x), linetype = 2, col = "#E76F51", size = 1) +
  geom_vline(aes(xintercept = x_end), linetype = 2, col = "#E76F51", size = 1)+
  # geom_segment(
  #   aes(
  #     x = x,
  #     y = y,
  #     xend = x_end,
  #     yend = y_end
  #   ),     # Set color outside aes()
  #   col = "#E76F51",
  #   linewidth = 0.8,
  #   arrow = arrow(length = unit(0.02, "npc"), ends = "both"))+
  theme(axis.text.x = element_text(size = 14,colour = "black"),
        axis.text.y = element_text(size = 14,colour = "black"),
        panel.spacing = unit(1, "lines"),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9,face = "bold"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 18, face = 'bold',colour = "black"),
        axis.title.y = element_text(size = 18, face = 'bold',colour = "black"),
        plot.title = element_text(size = 15, face = 'bold',colour = "black",hjust = 0.5),
        ##plot.title.position = 'plot',
        legend.position = "None")

p2





###################--------------------------Rt -----------------------------------------
sim_rt = read_rds("./data/sim_rt1.rds") 
  
p3 = 
  sim_rt %>% 
  filter(type != "Report-shift") %>% 
  ggplot() +
  geom_line(aes(x = t_end , y = mean_r, color = type, linetype = type),size = 0.8)+
  geom_ribbon(aes(x = t_end , ymin = quantile_0_025_r, ymax = quantile_0_975_r, fill = type),alpha = 0.2)+
  scale_color_manual(name = "Type", values = colors)+
  scale_fill_manual(name = "Type", values = colors)+
  scale_linetype_manual(name = "Type",values = types)+
  scale_x_continuous(
    breaks = c(0,50,100,150, 200), 
    limits = c(50, 150)
  ) +
  scale_y_continuous(breaks = c(1,2),limits = c(0.5,2))+
  theme_bw() +
  labs(x = "Time", y = "Rt", title = "Deconv-BP") +
  geom_vline(aes(xintercept = 141), linetype = 2, col = "#E76F51", size = 1) +
  geom_vline(aes(xintercept = 150), linetype = 2, col = "#E76F51", size = 1)+
  theme(axis.text.x = element_text(size = 14,colour = "black"),
        axis.text.y = element_text(size = 14,colour = "black"),
        panel.spacing = unit(1, "lines"),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9,face = "bold"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 18, face = 'bold',colour = "black"),
        axis.title.y = element_text(size = 18, face = 'bold',colour = "black"),
        plot.title = element_text(size = 15, face = 'bold',colour = "black",hjust = 0.5),
        ##plot.title.position = 'plot',
        legend.position = "None")

p3

p4 = 
  sim_rt %>% 
  filter(type  %in% c("Infections","Report","Report-shift"))%>% 
  ggplot() +
  geom_line(aes(x = t_end , y = mean_r, color = type, linetype = type),size = 0.8)+
  geom_ribbon(aes(x = t_end , ymin = quantile_0_025_r, ymax = quantile_0_975_r, fill = type),alpha = 0.2)+
  scale_color_manual(name = "Type", values = colors)+
  scale_fill_manual(name = "Type", values = colors)+
  scale_linetype_manual(name = "Type",values = types)+
  scale_x_continuous(
    breaks = c(0,50,100,150, 200), 
    limits = c(50, 150)
  ) +
  scale_y_continuous(breaks = c(1,2),limits = c(0.5,2))+
  theme_bw() +
  labs(x = "Time", y = "Rt", title = "Report-shift") +
  geom_vline(aes(xintercept = 141), linetype = 2, col = "#E76F51", size = 1) +
  geom_vline(aes(xintercept = 150), linetype = 2, col = "#E76F51", size = 1)+
  theme(axis.text.x = element_text(size = 14,colour = "black"),
        axis.text.y = element_text(size = 14,colour = "black"),
        panel.spacing = unit(1, "lines"),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9,face = "bold"),
        plot.background = element_rect(fill = 'white', colour = 'white'),   ###IMPORTANT!!!
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.line = element_line(colour = "black", linewidth = 0),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size = 18, face = 'bold',colour = "black"),
        axis.title.y = element_text(size = 18, face = 'bold',colour = "black"),
        plot.title = element_text(size = 15, face = 'bold',colour = "black",hjust = 0.5),
        ##plot.title.position = 'plot',
        legend.position = "None")

p4


p12 = ggarrange(p1,p2,p3,p4,
                labels = c("A","B","C","D"), nrow = 2,ncol = 2,
          font.label = list(size = 15),align = "v")

p_all = ggarrange(p12, as_ggplot(p_legend), nrow = 2,heights = c(0.9,0.1),
                  font.label = list(size = 15),legend = "bottom",align = "v")+
  theme(plot.margin = ggplot2::margin(10,15,5,15,unit = "pt"))

p_all

ggsave(p_all,file = "./figures_clean/Figure1_sim.pdf",dpi = 300,width = 13, height = 10)# 
#ggsave(p_all,file = "./figures_png/Figure1_sim.png",dpi = 300,width = 13, height = 10)# 

