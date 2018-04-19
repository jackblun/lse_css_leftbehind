### Cleaning of lsecss files

### 0. Setup ------

rm(list = ls()) 	
options("scipen"=100, "digits"=4)

set.seed(123)

setwd("/Users/jack/Dropbox/Documents/LCS CSS Hackathon") # Dropbox

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

### 2. Apply filters ------

# filter to US only

dta_US <- dta_all[str_sub(METRO_ID, 1, 2)=="US", , ]

# filter to specific variables

dta_US_filter <- dta_US %>% filter(VAR == "EMP_R" | VAR == "UNEMP_R" |
                                     VAR == "POP_DEP_RATIO_OLD" | VAR == "POP_DEP_RATIO_YOUNG" | 
                                     VAR == "POP_DENS" | VAR == "GINI_INC" | 
                                     VAR == "EQU_HOU_DISP_INC" | VAR == "GDP_PC" |
                                     VAR == "GDP_PC" | VAR == "GDP_SHARE")

### 3. Label rust belt ------

rustnames <- c("US033","US055","US035","US045",
               "US038","US069","US081","US097",
               "US106","US134","US107")


dta_US_filter$rustbelt <- 0
dta_US_filter$rustbelt[which(dta_US_filter$METRO_ID %in% rustnames)] <- 1

# Drop a bunch of stuff

dta_US_filter <- dta_US_filter %>% select(`Metropolitan areas`,VAR,TIME,Value,rustbelt)


### 4. Make wide ------

dta_US_filter_wide <- spread(data = dta_US_filter, key = "VAR", value = "Value")

### 5. Select variables again ------

dta_US_filter_wide <- select(dta_US_filter_wide, - EQU_HOU_DISP_INC, - GINI_INC)

#dta_US_filter_wide <- dta_US_filter_wide[complete.cases(dta_US_filter_wide),]

### 6. Average over time

#dta_US_filter_wide_avg <- group_by("")

### -------------------------------------------------------------------------------------------------------
# average and time change
### -------------------------------------------------------------------------------------------------------
dta_US_filter_wide <- as.data.table(dta_US_filter_wide)

test <- with(dta_US_filter_wide, lapply(list(EMP_R,GDP_PC,GDP_SHARE,POP_DENS,POP_DEP_RATIO_OLD,POP_DEP_RATIO_YOUNG,UNEMP_R), function(c) ave(c, `Metropolitan areas`, FUN=function(x) c(NA, diff(x)/x[-length(x)]) )))

dta_US_filter_wide$growth_EMP_R <- test[[1]]
dta_US_filter_wide$growth_GDP_PC <- test[[2]]
dta_US_filter_wide$growth_GDP_SHARE <- test[[3]]
dta_US_filter_wide$growth_POP_DENS <- test[[4]]
dta_US_filter_wide$growth_POP_DEP_RATIO_OLD <- test[[5]]
dta_US_filter_wide$growth_POP_DEP_RATIO_YOUNG <- test[[6]]
dta_US_filter_wide$growth_UNEMP_R <- test[[7]]

cols <- colnames(dta_US_filter_wide)[3:ncol(dta_US_filter_wide)]
dta_US_avg <- dta_US_filter_wide[ , sapply(.SD, function(x) list(mean = mean(x, na.rm=T))), .SDcols = cols, by=`Metropolitan areas`]   


### ----------------------------------------------------------------------------------------------------

### densities US

dta_ana <- dta_US_filter_wide

colnames(dta_ana)

vars <- colnames(dta_ana)[4:ncol(dta_ana)]

for (n in vars){
  dens1 <- with(dta_ana[which(dta_ana$rustbelt==0), ], density(get(n),na.rm=T))
  dens_rust <- with(dta_ana[which(dta_ana$rustbelt==1), ], density(get(n),na.rm=T) )
  max_y <- max(max(dens1$y),max(dens_rust$y))
  plot(dens1, ylim=c(0,max_y), col='blue', main=n)
  lines(dens_rust, col='red')
}

### ----------------------------------------------------------------------------------------------------

### Germany

dta_GER <- dta_all[str_sub(METRO_ID, 1, 2)=="DE", , ]

ruhr <- c("Essen","Dortmund","Bochum","Duisburg")

#clean

# filter to specific variables

dta_GER_filter <- dta_GER %>% filter(VAR == "EMP_R" | VAR == "UNEMP_R" |
                                     VAR == "POP_DEP_RATIO_OLD" | VAR == "POP_DEP_RATIO_YOUNG" | 
                                     VAR == "POP_DENS" | VAR == "GINI_INC" | 
                                     VAR == "EQU_HOU_DISP_INC" | VAR == "GDP_PC" |
                                     VAR == "GDP_PC" | VAR == "GDP_SHARE")

### 3. Label Ruhr --------

dta_GER_filter$ruhr <- 0
dta_GER_filter$ruhr[which(dta_GER_filter$`Metropolitan areas` %in% ruhr)] <- 1

# Drop a bunch of stuff

dta_GER_filter <- dta_GER_filter %>% select(`Metropolitan areas`,VAR,TIME,Value,ruhr)


### 4. Make wide ------

dta_GER_filter_wide <- spread(data = dta_GER_filter, key = "VAR", value = "Value")

### 5. Select variables again ------

dta_GER_filter_wide <- select(dta_GER_filter_wide, - EQU_HOU_DISP_INC, - GINI_INC)



dta_GER_ana <- dta_GER_filter_wide

### ----------------------------------------------------------------------------------------------------

### densities Germany

vars <- colnames(dta_GER_ana)[4:ncol(dta_GER_ana)]

for (n in vars){
  dens1 <- with(dta_GER_ana[which(dta_GER_ana$ruhr==0), ], density(get(n),na.rm=T))
  dens_ruhr <- with(dta_GER_ana[which(dta_GER_ana$ruhr==1), ], density(get(n),na.rm=T) )
  max_y <- max(max(dens1$y),max(dens_ruhr$y))
  plot(dens1, ylim=c(0,max_y), col='blue', main=n)
  lines(dens_ruhr, col='red')
}
