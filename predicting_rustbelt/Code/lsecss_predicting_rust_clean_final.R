### PREDICTING RUST BELT STATUS
### Cleaning of lsecss metropolitan region files

### 0. Setup ------

rm(list = ls()) 	
options("scipen"=100, "digits"=4)

set.seed(123)

setwd("/Users/jack/Dropbox/Documents/LCS CSS Hackathon") # Dropbox

# Load packages

library(dplyr)
library(tidyr)
library(data.table)
library(stringr)

### 1. Load raw data ------
# Data downloaded from OECD

dta_gdp <- fread("Data/gdp_cities.csv")
dta_inc <- fread("Data/income_inequality.csv")
dta_lab <- fread("Data/lab_market.csv")
dta_pop_age <- fread("Data/population_age.csv")
dta_pop <- fread("Data/population.csv")

dta_all <- rbindlist(list(dta_gdp,dta_inc,dta_lab,dta_pop_age,dta_pop))

rm(dta_gdp,dta_inc,dta_lab,dta_pop_age,dta_pop)

### 2. Apply filters ------

# filter to specific variables

dta_all_filter <- dta_all %>% filter(VAR == "EMP_R" | VAR == "UNEMP_R" |
                                       VAR == "POP_DEP_RATIO_OLD" | VAR == "POP_DEP_RATIO_YOUNG" | 
                                       VAR == "POP_DENS" | VAR == "GINI_INC" | 
                                       VAR == "EQU_HOU_DISP_INC" | VAR == "GDP_PC" |
                                       VAR == "GDP_PC" | VAR == "GDP_SHARE" | 
                                       VAR == "POP" | VAR == "POP_0_14" | 
                                       VAR == "POP_65MORE" | VAR == "LABOUR_PRODUCTIVITY" |
                                       VAR == "LF" | VAR == "PART_R")

### 3. Drop a bunch of stuff

dta_all_filter <- dta_all_filter %>% select(METRO_ID,VAR,TIME,Value)

### 4. Make wide ------

dta_all_filter_wide <- spread(data = dta_all_filter, key = "VAR", value = "Value")

### 5. Select variables again ------

dta_all_filter_wide <- select(dta_all_filter_wide, - EQU_HOU_DISP_INC, - GINI_INC)
dta_all_filter_wide <- dta_all_filter_wide[complete.cases(dta_all_filter_wide),]
dta_all_filter_wide <- as_tibble(dta_all_filter_wide)

### 6. Average  / take changes over time

dta_all_filter_wide_tm <- dta_all_filter_wide %>%  group_by(METRO_ID) %>%
  mutate(emp_r_ch = (EMP_R - lag(EMP_R,1))/lag(EMP_R,1)*100,
         gdp_pc_ch = (GDP_PC - lag(GDP_PC,1))/lag(GDP_PC,1)*100,
         pop_dep_ratio_young_ch =  (POP_DEP_RATIO_YOUNG - lag(POP_DEP_RATIO_YOUNG,1))/lag(POP_DEP_RATIO_YOUNG,1)*100,
         pop_dep_ratio_old_ch =  (POP_DEP_RATIO_OLD - lag(POP_DEP_RATIO_OLD,1))/lag(POP_DEP_RATIO_OLD,1)*100,
         unemp_R_ch = (UNEMP_R - lag(UNEMP_R,1))/lag(UNEMP_R,1)*100,
         pop_65more_ch = (POP_65MORE - lag(POP_65MORE,1))/lag(POP_65MORE,1)*100,
         pop_ch = (POP - lag(POP,1))/lag(POP,1)*100,
         labour_productivity_ch = (LABOUR_PRODUCTIVITY - lag(LABOUR_PRODUCTIVITY,1))/lag(LABOUR_PRODUCTIVITY,1)*100,
         lf_ch = (LF - lag(LF,1))/lag(LF,1)*100,
         part_r_ch = (PART_R - lag(PART_R,1))/lag(PART_R,1)*100)


dta_all_filter_wide_avg <- dta_all_filter_wide_tm %>% group_by(METRO_ID) %>%
  summarize(emp_r = mean(EMP_R), gdp_pc = mean(GDP_PC, na.rm = T), gdp_share = mean(GDP_SHARE, na.rm = T),
            pop_dens = mean(POP_DENS), pop_dep_ratio_old = mean(POP_DEP_RATIO_OLD),
            pop_dep_ratio_young = mean(POP_DEP_RATIO_YOUNG), unemp_R = mean(UNEMP_R),
            pop = mean(POP, na.rm = T), pop_65more = mean(POP_65MORE, na.rm = T), 
            labour_productivity = mean(LABOUR_PRODUCTIVITY, na.rm = T), lf = mean(LF, na.rm = T),
            part_r = mean(PART_R, na.rm = T),
            emp_r_ach = mean(emp_r_ch, na.rm = T), gdp_pc_ach = mean(gdp_pc_ch, na.rm = T),
            pop_dep_ratio_young_ach = mean(pop_dep_ratio_young_ch, na.rm = T), 
            pop_dep_ratio_old_ach = mean(pop_dep_ratio_old_ch, na.rm = T),
            unemp_R_ach = mean(unemp_R_ch, na.rm = T),
            pop_65more_ach = mean(pop_65more, na.rm = T),
            pop_ach = mean(pop_ch, na.rm = T),
            labour_productivity_ach = mean(labour_productivity_ch, na.rm = T),
            lf_ach = mean(lf_ch, na.rm = T),
            part_r_ach = mean(part_r_ch, na.rm = T))  

### Add "Rustbelt" indicator --------------------------------------------------------------------------------

#US
rustnames <- c("US033","US055","US035","US045",
               "US038","US069","US081","US097",
               "US106","US134","US107",
               "US039","US065","US103",
               "US117","US115","US141")

#Germany
ruhrnmames <- c("DE006","DE010","DE015","DE501")

dta_all_filter_wide_avg$rustbelt <- NA
dta_all_filter_wide_avg$rustbelt[with(dta_all_filter_wide_avg, which(str_sub(METRO_ID, 1, 2)=="US"))] <- 0
dta_all_filter_wide_avg$rustbelt[with(dta_all_filter_wide_avg, which(str_sub(METRO_ID, 1, 2)=="DE"))] <- 0
dta_all_filter_wide_avg$rustbelt[with(dta_all_filter_wide_avg, which(METRO_ID %in% c(rustnames,ruhrnmames)))] <- 1

### 99. Export cleaned data

saveRDS(dta_all_filter_wide_avg, file = "Data/dat_all_clean.RDS") 
