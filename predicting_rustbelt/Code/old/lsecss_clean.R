### Cleaning of lsecss files

### 0. Setup ------
 
rm(list = ls()) 	
options("scipen"=100, "digits"=4)

set.seed(123)

#setwd("/Users/jack/Dropbox/Documents/LCS CSS Hackathon") # Dropbox

# Load packages

library(dplyr)
library(tidyr)
#library(gtools)
#library(caret)
#library(readr)
#library(resample)
#library(doBy)
#library(corrplot)
#library(rattle)
#library(doParallel)
library(data.table)
library(stringr)

### 1. Load raw data ------

dta_gdp <- fread("Data/gdp_cities.csv")
dta_inc <- fread("Data/income_inequality.csv")
dta_lab <- fread("Data/lab_market.csv")
dta_pop_age <- fread("Data/population_age.csv")
dta_pop <- fread("Data/population.csv")

dta_all <- rbindlist(list(dta_gdp,dta_inc,dta_lab,dta_pop_age,dta_pop))

rm(dta_gdp,dta_inc,dta_lab,dta_pop_age,dta_pop)

dta_US <- dta_all[str_sub(METRO_ID, 1, 2)=="US", , ]





