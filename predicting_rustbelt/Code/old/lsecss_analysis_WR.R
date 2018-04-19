### Analysis of lsecss files

### 0. Setup ------

rm(list = ls()) 	
options("scipen"=100, "digits"=4)

set.seed(123)

#setwd("/Users/jack/Dropbox/Documents/LCS CSS Hackathon") # Jack Dropbox
setwd("/Users/wolfgang/Dropbox/LCS CSS Hackathon")

# Load packages

#pacman::p_load(dplyr,tidyr,caret,data.table,stringr,corrplot,caretEnsemble,tmap,tmaptools)

library(dplyr)
library(tidyr)
#library(gtools)
library(caret)
#library(readr)
#library(resample)
#library(doBy)
#library(corrplot)
#library(rattle)
#library(doParallel)
library(data.table)
library(stringr)
library(corrplot)
#library(caretEnsemble)
library(tmap)
library(tmaptools)



all_clean <- readRDS(file = "Data/dat_all_clean.RDS")

### 1. Load clean data ------

#USclean <- readRDS(file = "Data/dat_US_clean")
all_clean <- ungroup(all_clean)

all_clean$rustbelt <- as.factor(all_clean$rustbelt)
levels(all_clean$rustbelt) <- c("Not Rustbelt","Rustbelt")

USclean <- all_clean[with(all_clean, which(str_sub(METRO_ID, 1, 2)=="US")), ]

DEclean <- all_clean[with(all_clean, which(str_sub(METRO_ID, 1, 2)=="DE")), ]

#### Mapping -------------------------------------------------------------------------------------------

de.met <- read_shape("Data/shapefiles/OECD_MA_DEU/DEU_MAs_2016.shp", as.sf = T)
de.map <- append_data(de.met, DEclean, key.shp = "METRO", key.data = "METRO_ID")

#de.state <- read_shape("Data/shapefiles/Germany_shapefile/de_10km.shp", as.sf = T)
de.state <- read_shape("Data/shapefiles/DEU_adm_shp/DEU_adm0.shp", as.sf = T)

#de.state1 <- subset(de.state, STUSPS != "AK" & STUSPS != "HI" & STUSPS != "PR")

rb.de <- tm_shape(de.state) + tm_polygons(id = "STATEFP") +
  tm_shape(de.map) + tm_polygons("rustbelt", id = "METRO", palette= "Reds") +
  tm_layout(
    #title = "US Metropolitan Areas",
    #title.size = 2.5,
    #title.position = c("left","top"),
    legend.title.size = 0.001,
    legend.text.size = 2.5,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0)

save_tmap(rb.de, "Output/rb_de.png")


#names(us.met)[2] <- "Metropolitan areas"

us.map <- append_data(us.met, USclean, key.shp = "METRO", key.data = "METRO_ID")

us.state <- read_shape("Data/shapefiles/us_shapefile/cb_2017_us_state_20m.shp", as.sf = T)
us.state1 <- subset(us.state, STUSPS != "AK" & STUSPS != "HI" & STUSPS != "PR")

#us.state  <- filter(us.state, STUSPS != "AK")

rb.us <- tm_shape(us.state1) + tm_polygons(id = "STATEFP") +
  tm_shape(us.map) + tm_polygons("rustbelt", id = "METRO", palette= "Reds") +
  tm_layout(
    #title = "US Metropolitan Areas",
    #title.size = 2.5,
    #title.position = c("left","top"),
    legend.title.size = 0.001,
    legend.text.size = 2.5,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0)

#save_tmap(rb.us, "Output/rb_us.png")

#USclean$METRO_ID <- NULL

### Predictive Model ------------------------------------------------------------------------------------------

# Train / test split

USclean <- as_tibble(USclean)

levels(USclean$rustbelt) <- c("not_rustbelt","rustbelt") # need to recode levels

index <- createDataPartition(y = USclean$rustbelt, p = .5, list = FALSE)
training <- select(USclean, - METRO_ID)[index, ]
test <- USclean[-index, ]

# upsample

training <- upSample(x = training[, -ncol(training)],y = training$rustbelt)    

training$rustbelt <- training$Class
training$Class <- NULL

# Train control set up

reg.ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE)

cls.ctrl <- trainControl(method = "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                         number = 10, repeats = 5,
                         classProbs = TRUE, summaryFunction = twoClassSummary,
                         savePredictions = "final", allowParallel = TRUE)


### 4.3 random forest

set.seed(1895)
rf.fit <- train(rustbelt ~ ., data = training, trControl = cls.ctrl,
                method = "ranger", metric = "ROC",
                preProcess = c("nzv", "center", "scale"), 
                importance = 'impurity')
rf.fit
plot(rf.fit)

rf.fit_us <- readRDS(file = "code/rf_fit_mod.RDS")
plot(rf.fit_us)

rf.preds <- predict(rf.fit_us, newdata = test)
head(rf.preds, 20)
rf.prob.preds <- predict(rf.fit_us, newdata = test, type = "prob") #class probabilities
head(rf.prob.preds)

postResample(pred = rf.preds, obs = test$rustbelt)
confusionMatrix(data = rf.preds, reference = test$rustbelt)


## predict germany

rf.preds_de <- predict(rf.fit_us, newdata = DEclean)
rf.prob.preds <- predict(rf.fit_us, newdata = DEclean, type = "prob") #class probabilities

DEclean$rf.preds <- predict(rf.fit_us, newdata = DEclean)
DEclean$rf.prob.preds <- predict(rf.fit_us, newdata = DEclean, type = "prob")[,2]

levels(DEclean$rf.preds) <- c("Not rustbelt (Prediction)","Rustbelt (Prediction)") # need to recode levels

de.map <- append_data(de.met, DEclean, key.shp = "METRO", key.data = "METRO_ID")


rb.de_pred <- tm_shape(de.state) + tm_polygons(id = "STATEFP") +
  tm_shape(de.map) + tm_polygons("rf.preds", id = "METRO", palette= "YlOrRd") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    #title.position = c("left","top"),
    legend.title.size = 0.001,
    legend.text.size = 1.5,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0) 

rb.de_pred_prob <- tm_shape(de.state) + tm_polygons(id = "STATEFP") +
  tm_shape(de.map) + tm_polygons("rf.prob.preds", id = "METRO", 
                                 palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0)
tm_layout(attr.outside = TRUE, legend.outside = TRUE)


save_tmap(rb.de_pred_prob, "Output/rb_de_pred_prob.png")


### ----------------------------------------------------------------------------------------------------

### densities Germany

vars <- colnames(DEclean)[4:(ncol(DEclean)-1)]

for (n in vars){
  png(filename=paste0("Output/distributions/germany_",n,".png"), units="cm", width=20, height=20, res=400)
  dens1 <- with(DEclean[which(DEclean$rustbelt=='Not Rustbelt'), ], density(get(n),na.rm=T))
  dens_rustbelt <- with(DEclean[which(DEclean$rustbelt=='Rustbelt'), ], density(get(n),na.rm=T) )
  max_y <- max(max(dens1$y),max(dens_rustbelt$y))
  plot(dens1, ylim=c(0,max_y), col='blue', main=n)
  lines(dens_rustbelt, col='red')
  dev.off()
}

### densities US

vars <- colnames(USclean)[4:(ncol(USclean)-1)]

for (n in vars){
  png(filename=paste0("Output/distributions/US_",n,".png"), units="cm", width=20, height=20, res=400)
  dens1 <- with(USclean[which(USclean$rustbelt=='Not Rustbelt'), ], density(get(n),na.rm=T))
  dens_rustbelt <- with(USclean[which(USclean$rustbelt=='Rustbelt'), ], density(get(n),na.rm=T) )
  max_y <- max(max(dens1$y),max(dens_rustbelt$y))
  plot(dens1, ylim=c(0,max_y), col='blue', main=n)
  lines(dens_rustbelt, col='red')
  dev.off()
}



