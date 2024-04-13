#----Library----
library(tidyverse)
library(gtsummary)

#----Data----
set.seed(1234)

df <- tibble(gender_m = rbinom(n = 100000, size = 1, prob = 0.5),
             age = floor(rnorm(n = 100000, mean = 57.0, sd = 10.8)),
             cr = rgamma(n = 100000, shape = 1.7, rate = 1.16)) %>% 
  mutate(weight_kg = rnorm(100000, mean = if_else(gender_m == 1, 86, 74), sd = 15),
         height_m = rnorm(100000, mean = if_else(gender_m == 1, 1.58, 1.4), sd = 0.2),
         cr = ifelse(gender_m == 0, cr * 0.8, cr)) %>% 
  mutate(bsa = round(sqrt((height_m*100*weight_kg)/3600), 2),
         cr = round(if_else(cr < 0.17, cr + 0.17, cr), 2),
         weight_kg = round(weight_kg, 2),
         height_m = round(height_m, 2),
         age = if_else(age < 18, 18, age),
         weight_kg = if_else(weight_kg < 30, 30, weight_kg)) %>% 
  mutate(egfr = 142 * 
           ifelse(cr/ifelse(gender_m == 1, 0.9, 0.7) < 1, cr/ifelse(gender_m == 1, 0.9, 0.7), 1)^(ifelse(gender_m == 1, -0.302, -0.241)) * 
           ifelse(cr/ifelse(gender_m == 1, 0.9, 0.7) > 1, cr/ifelse(gender_m == 1, 0.9, 0.7), 1)^-1.2 * 
           0.9938^age * 
           ifelse(gender_m == 1, 1, 1.012)) %>% 
  mutate(egfr_di = egfr * (bsa/1.73),
         crcl = if_else(gender_m == 1, ((140 - age) * weight_kg)/(72*cr), (((140 - age) * weight_kg)/(72*cr)) * 0.85)) %>% 
  mutate(egfr_bucket = if_else(egfr_di >=90, ">=90 mL/min",
                               if_else(egfr_di >=60 & egfr_di <90, "60-89 mL/min",
                                       if_else(egfr_di >=30 & egfr_di <60, "30-59 mL/min",
                                               if_else(egfr_di >=15 & egfr_di <30, "15-30 mL/min",
                                                       "<15 mL/min")))),
         crcl_bucket = if_else(crcl >=90, ">=90 mL/min",
                               if_else(crcl >=60 & crcl <90, "60-89 mL/min",
                                       if_else(crcl >=30 & crcl <60, "30-59 mL/min",
                                               if_else(crcl >=15 & crcl <30, "15-30 mL/min",
                                                       "<15 mL/min"))))) %>% 
  mutate(concord = egfr_bucket == crcl_bucket) %>% 
  mutate(egfr_bucket = factor(egfr_bucket, levels = c("<15 mL/min", "15-30 mL/min", "30-59 mL/min",
                                         "60-89 mL/min", ">=90 mL/min")),
         crcl_bucket = factor(crcl_bucket, levels = c("<15 mL/min", "15-30 mL/min", "30-59 mL/min",
                                                      "60-89 mL/min", ">=90 mL/min")))

# Visualizing the simulated data
df %>% ggplot(aes(x = age)) + geom_density()
df %>% ggplot(aes(x = cr)) + geom_density() #Artificial spike at 0.17 because I got rid of values less than that
df %>% ggplot(aes(x = weight_kg, fill = as.factor(gender_m))) + geom_density(alpha = 0.5)
df %>% ggplot(aes(x = height_m, fill = as.factor(gender_m))) + geom_density(alpha = 0.5)
df %>% ggplot(aes(x = cr, fill = as.factor(gender_m))) + geom_density(alpha = 0.5)
df %>% ggplot(aes(x = egfr_di, fill = as.factor(gender_m))) + geom_density(alpha = 0.5)
df %>% ggplot(aes(x = crcl, fill = as.factor(gender_m))) + geom_density(alpha = 0.5)

# Table of concordance between general categories
df %>% tbl_summary(include = c(egfr_bucket), by = crcl_bucket,
                   label = list(egfr_bucket ~ "eGFR BSAadj Bucket")) %>% 
  modify_caption("**CrCl Bucket**")

# Cefepime
cefepime <- df %>% 
  mutate(egfr_dose = if_else(egfr_di > 60, "2g q8",
                             if_else(egfr_di >=30 & egfr_di <=60, "2g q12",
                                     if_else(egfr_di < 30 & egfr_di >= 11, "2g q24",
                                             "1g q24"))),
         crcl_dose = if_else(crcl > 60, "2g q8",
                             if_else(crcl >=30 & crcl <=60, "2g q12",
                                     if_else(crcl < 30 & crcl >= 11, "2g q24",
                                             "1g q24")))) %>% 
  mutate(egfr_dose  = factor(egfr_dose, levels = c("1g q24", "2g q24", "2g q12", "2g q8")),
         crcl_dose  = factor(crcl_dose, levels = c("1g q24", "2g q24", "2g q12", "2g q8"))) %>% 
  mutate(cef_dose_conc = egfr_dose == crcl_dose)

cefepime %>% tbl_summary(include = c(egfr_dose), by = crcl_dose)

# Meropenem
meropenem <- df %>% 
  mutate(egfr_dose = if_else(egfr_di > 50, "1g q8",
                             if_else(egfr_di >25 & egfr_di <=50, "1g q12",
                                     if_else(egfr_di <= 25 & egfr_di >= 10, "500mg q12",
                                             "500mg q24"))),
         crcl_dose = if_else(crcl > 50, "1g q8",
                             if_else(crcl >25 & crcl <=50, "1g q12",
                                     if_else(crcl <= 25 & crcl >= 10, "500mg q12",
                                             "500mg q24")))) %>% 
  mutate(egfr_dose  = factor(egfr_dose, levels = c("500mg q24", "500mg q12", "1g q12", "1g q8")),
         crcl_dose  = factor(crcl_dose, levels = c("500mg q24", "500mg q12", "1g q12", "1g q8"))) %>% 
  mutate(mero_dose_conc = egfr_dose == crcl_dose)

meropenem %>% tbl_summary(include = c(egfr_dose), by = crcl_dose,
                          label = list(egfr_dose ~ "Meropenem Dose by eGFR BSAadj")) %>% 
  modify_caption("**Meropenem Dose by CrCl**")

