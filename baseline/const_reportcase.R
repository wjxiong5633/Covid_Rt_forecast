
### ------------------case 
data_case_cov <- read_csv("data/data2023_case_covariates.csv") %>% 
  as.data.frame() %>% 
  dplyr::select(-region,-date_analysis) %>% 
  dplyr::rename("case" = "confirm")

sub1 = data_case_cov
sub1$date = sub1$date+1

const_report = data.frame(mytestdate  = rep(unique(sub1$date),each = 22),
                          pred_reportcase = rep(sub1$case, each = 22)) %>% 
  mutate(proj = rep(0:21,length(unique(sub1$date))),
         #const_reportcase_log = log(pred_reportcase+0.0001),
         date = mytestdate  + proj,
         modelname = "const") %>% 
  filter(mytestdate %in% seq(as.Date("2022-05-25"),as.Date("2022-12-31"),by = "day")) %>% 
  left_join(true_dat,by = "date") %>% 
  mutate(type = "test",
         true_reportcase = report_case)


write.csv(const_report,"./pred_reportcase_point/const_reportcase_point.csv",row.names = F)
