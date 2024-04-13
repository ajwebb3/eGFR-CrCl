#----Library----
library(tidyverse)
library(gtsummary)

#----Data----
set.seed(1234)

df <- tibble(gender_m = rbinom(n = 1000000, size = 1, prob = 0.5),
             age = floor(rnorm(n = 1000000, mean = 57.0, sd = 10.8)),
             cr = rgamma(n = 1000000, shape = 1.66, rate = 1.16)) %>% 
  mutate(weight_kg = if_else(gender_m == 1, rnorm(1, mean = 85, sd = 20), 
                             rnorm(1, mean = 75, sd = 20)),
         height_m = if_else(gender_m == 1, rnorm(1, mean = 1.6, sd = 0.2), 
                            rnorm(1, mean = 1.5, sd = 0.2))) %>% 
  mutate(bsa = sqrt((height_m*100*weight_kg)/3600)) %>% 
  mutate(egfr = 142 * ifelse(cr < 1, cr, 1)^(ifelse(gender_m == 1, -0.302, -0.241)) * ifelse(cr > 1, cr, 1)^-1.2 * 0.9938^age * 
           ifelse(gender_m == 1, 1, 1.012)) %>% 
  mutate(egfr_di = egfr * (bsa/1.73),
         crcl = ((140 - age) * weight_kg)/(72*cr)) %>% 
  mutate(egfr_bucket = if_else(egfr_di >=90, ">=90 mL/min",
                               if_else(egfr_di >=60 & egfr_di <90, "60-89 mL/min",
                                       if_else(egfr_di >=30 & egfr_di <60, "30-59 mL/min",
                                               if_else(egfr_di >=10 & egfr_di <30, "10-30 mL/min",
                                                       "<10 mL/min")))),
         crcl_bucket = if_else(crcl >=90, ">=90 mL/min",
                               if_else(crcl >=60 & crcl <90, "60-89 mL/min",
                                       if_else(crcl >=30 & crcl <60, "30-59 mL/min",
                                               if_else(crcl >=10 & crcl <30, "10-30 mL/min",
                                                       "<10 mL/min"))))) %>% 
  mutate(concord = egfr_bucket == crcl_bucket) %>% 
  mutate(egfr_bucket = factor(egfr_bucket, levels = c("<10 mL/min", "10-30 mL/min", "30-59 mL/min",
                                         "60-89 mL/min", ">=90 mL/min")),
         crcl_bucket = factor(crcl_bucket, levels = c("<10 mL/min", "10-30 mL/min", "30-59 mL/min",
                                                      "60-89 mL/min", ">=90 mL/min")))


df %>% tbl_summary(include = c(egfr_bucket, concord), by = crcl_bucket)
