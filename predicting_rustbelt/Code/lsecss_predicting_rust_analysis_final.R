### PREDICTING RUST BELT STATUS
### Analysis of lsecss metropolitan region files

### 0. Setup ------

rm(list = ls()) 	
options("scipen"=100, "digits"=4)

set.seed(123)

setwd("/Users/jack/Dropbox/Documents/LCS CSS Hackathon") # Jack Dropbox

# Load packages

library(dplyr)
library(tidyr)
library(caret)
library(data.table)
library(stringr)
library(corrplot)
library(caretEnsemble)
library(tmap)
library(tmaptools)

### 1. Load clean data ------
# Data cleaned in R cleaning file

all.dat <- readRDS("Data/dat_all_clean.RDS")
USclean <- filter(all.dat, substr(METRO_ID,1,2) == "US")

USclean$rustbelt <- as.factor(USclean$rustbelt)
levels(USclean$rustbelt) <- c("Not Rustbelt","Rustbelt")

### 2.Mapping US rust belt

us.met <- read_shape("Data/shapefiles/OECD_MA_USA/USA_MAs_2016.shp", as.sf = T)
us.map <- append_data(us.met, USclean, key.shp = "METRO", key.data = "METRO_ID")
us.state <- read_shape("Data/shapefiles/us_shapefile/cb_2017_us_state_20m.shp", as.sf = T)
us.state1 <- subset(us.state, STUSPS != "AK" & STUSPS != "HI" & STUSPS != "PR")

rb.us <- tm_shape(us.state1) + tm_polygons(id = "STATEFP") +
  tm_shape(us.map) + tm_polygons("rustbelt", id = "METRO", palette= "YlOrRd") +
  tm_layout(
    legend.title.size = 0.001,
    legend.text.size = 2.5,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0,
    frame = F)
save_tmap(rb.us, "Output/rb_us.png")

### 3. Unidimensional plots, correlations, visualisations ------

hist(USclean$emp_r)
hist(USclean$pop_dep_ratio_young)
M<-cor(select(USclean, - rustbelt, - METRO_ID))
corrplot(M, method="circle")

### 4. Models predicting rust belt ------

# Regressions
reg.mod <- glm(as.numeric(rustbelt) ~ ., data = select(USclean, - METRO_ID))
summary(reg.mod)

# T-test
t.test(emp_r ~ rustbelt, data = USclean)
t.test(gdp_pc ~ rustbelt, data = USclean)
t.test(gdp_share ~ rustbelt, data = USclean)
t.test(pop_dens ~ rustbelt, data = USclean)
t.test(pop_dep_ratio_old ~ rustbelt, data = USclean)
t.test(pop_dep_ratio_young ~ rustbelt, data = USclean)

# Train / test split

levels(USclean$rustbelt) <- c("not_rustbelt","rustbelt") # need to recode levels

index <- createDataPartition(y = USclean$rustbelt, p = .5, list = FALSE)
training <- select(USclean, - METRO_ID)[index, ]
test <- USclean[-index, ]

# upsample

training <- upSample(x = training[, -ncol(training)],
                     y = training$rustbelt)    
training$Class <- NULL

# Train control set up

reg.ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE)

cls.ctrl <- trainControl(method = "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                         number = 10, repeats = 5,
                         classProbs = TRUE, summaryFunction = twoClassSummary,
                         savePredictions = "final", allowParallel = TRUE)

### 4.1 logit

set.seed(1895)

glm.fit <- train(rustbelt ~ ., data = training, trControl = cls.ctrl,
                 method = "glm", family = "binomial", metric = "ROC",
                 preProcess = c("nzv", "center", "scale"))
glm.fit
plot(varImp(glm.fit))

confusionMatrix(glm.fit)


glm.preds <- predict(glm.fit, newdata = test)
head(glm.preds, 20)
glm.prob.preds <- predict(glm.fit, newdata = test, type = "prob") #class probabilities
head(glm.prob.preds)

postResample(pred = glm.preds, obs = test$rustbelt)
confusionMatrix(data = glm.preds, reference = test$rustbelt)

### 4.2 glmnet

glmnet.fit <- train(rustbelt ~ ., data = training, trControl = cls.ctrl,
                    method = "glmnet", metric = "ROC",
                    preProcess = c("nzv", "center", "scale"),
                    tuneGrid = expand.grid(alpha = 0:1,
                                           lambda = seq(0.0001, 1, length = 20)))
glmnet.fit
plot(varImp(glmnet.fit))

plot(glmnet.fit)

confusionMatrix(glmnet.fit)

glmnet.preds <- predict(glmnet.fit, newdata = test)
head(glmnet.preds, 20)
glmnet.prob.preds <- predict(glmnet.fit, newdata = test, type = "prob") #class probabilities
head(glmnet.prob.preds)

postResample(pred = glmnet.preds, obs = test$rustbelt)
confusionMatrix(data = glmnet.preds, reference = test$rustbelt)

### 4.3 random forest

set.seed(1895)
rf.fit <- train(rustbelt ~ ., data = training, trControl = cls.ctrl,
                method = "ranger", metric = "ROC",
                preProcess = c("nzv", "center", "scale"), 
                importance = 'impurity')
rf.fit
plot(rf.fit)
saveRDS(rf.fit, file = "Code/rf_fit_mod")

# Make variable importance plots
vimp <- varImp(rf.fit)
vimpdat <- as.data.frame(vimp$importance)
vimpdat$var <- row.names(vimpdat)
vimpdat <- arrange(vimpdat, - Overall)
vimpdat <- vimpdat[1:5,]
vimpdat$var <- c("Population (change 2000-15)","Labour Force (change 2000-15)",
                     "Population dependancy Ratio","Population Density","Participation Rate")
vimpdat$var <- factor(vimpdat$var, levels = vimpdat$var[order(vimpdat$Overall, decreasing = F)])
ggplot(data = vimpdat, aes(x = var, y = Overall)) + geom_col(fill = "darkorange") + 
  coord_flip() + xlab("") + ylab("") + theme(plot.title = element_text(size=22), 
                                             axis.title = element_text(size=30),
                                             axis.text =  element_text(size=20)) + 
  ggsave("Output/varimp_rf.png")
#plot(varImp(rf.fit), top = 5)

rf.preds <- predict(rf.fit, newdata = test)
head(rf.preds, 20)
rf.prob.preds <- predict(rf.fit, newdata = test, type = "prob") #class probabilities
head(rf.prob.preds)

postResample(pred = rf.preds, obs = test$rustbelt)
confusionMatrix(data = rf.preds, reference = test$rustbelt)

# Plot areas guessed to be rustbelt in full dataset 

USclean$rf.preds <- predict(rf.fit, newdata = USclean)
USclean$rf.prob.preds <- predict(rf.fit, newdata = USclean, type = "prob")[,2]

levels(USclean$rf.preds) <- c("Not rustbelt (Prediction)","Rustbelt (Prediction)") # need to recode levels

us.map <- append_data(us.met, USclean, key.shp = "METRO", key.data = "METRO_ID")

us.state <- read_shape("Data/shapefiles/us_shapefile/cb_2017_us_state_20m.shp", as.sf = T)
us.state1 <- subset(us.state, STUSPS != "AK" & STUSPS != "HI" & STUSPS != "PR")

#us.state  <- filter(us.state, STUSPS != "AK")

rb.us <- tm_shape(us.state1) + tm_polygons(id = "STATEFP") +
  tm_shape(us.map) + tm_polygons("rf.preds", id = "METRO", palette= "YlOrRd") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    #title.position = c("left","top"),
    legend.title.size = 0.001,
    legend.text.size = 1.5,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0) 
save_tmap(rb.us, "Output/rb_us_pred.png")


rb.us <- tm_shape(us.state1) + tm_polygons(id = "STATEFP") +
  tm_shape(us.map) + tm_polygons("rf.prob.preds", id = "METRO", 
                                 palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0 ,frame = FALSE)
  #tm_layout(attr.outside = TRUE, legend.outside = TRUE)
save_tmap(rb.us, "Output/rb_us_pred_probs.png")

### 4.4 Multiple models simultaneously

#set.seed(1895)

#models <- caretList(rustbelt ~ ., data = training, trControl = cls.ctrl, metric = "ROC",
#                    tuneList = list(logit = caretModelSpec(method = "glm", family = "binomial"),
#                                    elasticnet = caretModelSpec(method = "glmnet", tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 20))),
#                                    rf = caretModelSpec(method = "ranger")),
#                    preProcess = c("nzv", "center", "scale"))
#models


#models.preds <- lapply(models, predict, newdata = test)
#models.preds <- data.frame(models.preds)
#head(models.preds, 10)

#bwplot(resamples(models)) #dotplot

### 4.5 Predict into UK

uk.met <- read_shape("Data/shapefiles/OECD_MA_GBR/GBR_MAs_2016.shp", as.sf = T)
uk.map <- read_shape("Data/shapefiles/uk_shapefile/GBR_adm0.shp", as.sf = T)

# Check map working okay
testmap.uk <- tm_shape(uk.map) + tm_polygons(id = "METRO") +
  tm_shape(uk.met) + tm_polygons(id = "ID_0")
testmap.uk

# Load clean non-US data

all.dat <- readRDS("Data/dat_all_clean.RDS")
uk.dat <- filter(all.dat, substr(METRO_ID,1,2) == "UK")

uk.dat$rustbelt.pred <- predict(object = rf.fit, newdata = uk.dat)
uk.dat$rustbelt.pred.prob <- predict(object = rf.fit, newdata = uk.dat, type = "prob")[,2]

uk.map.dat <- append_data(uk.met, uk.dat, key.shp = "METRO", key.data = "METRO_ID")

# Predicted classes

levels(uk.dat$rustbelt.pred) <- c("Not rustbelt (Prediction)","Rustbelt (Prediction)") # need to recode levels
rb.uk.pred <- tm_shape(uk.map) + tm_polygons(id = "ID_0") + 
  tm_shape(uk.map.dat) + tm_polygons("rustbelt.pred", id = "METRO", palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0,frame = FALSE)
#tm_layout(attr.outside = TRUE, legend.outside = TRUE)
save_tmap(rb.uk.pred, "Output/rb_uk_pred.png")

# Predicted probabilities
rb.uk.pred.prob <- tm_shape(uk.map) + tm_polygons(id = "ID_0") + 
  tm_shape(uk.map.dat) + tm_polygons("rustbelt.pred.prob", id = "METRO", palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0) +
    tm_layout(attr.outside = F, legend.outside = F,frame = FALSE)
save_tmap(rb.uk.pred.prob, "Output/rb_uk_pred_probs.png")


### 4.6 Predict into Italy

it.met <- read_shape("Data/shapefiles/OECD_MA_ITA/ITA_MAs_2016.shp", as.sf = T)
it.map <- read_shape("Data/shapefiles/ita_shapefile/ITA_adm0.shp", as.sf = T)

# Check map working okay
testmap.it <- tm_shape(it.map) + tm_polygons(id = "METRO") +
  tm_shape(it.met) + tm_polygons(id = "ID_0")
testmap.it

# Load clean non-US data

all.dat <- readRDS("Data/dat_all_clean.RDS")
it.dat <- filter(all.dat, substr(METRO_ID,1,2) == "IT")

it.dat$rustbelt.pred <- predict(object = rf.fit, newdata = it.dat)
it.dat$rustbelt.pred.prob <- predict(object = rf.fit, newdata = it.dat, type = "prob")[,2]

it.map.dat <- append_data(it.met, it.dat, key.shp = "METRO", key.data = "METRO_ID")

# Predicted classes

levels(it.dat$rustbelt.pred) <- c("Not rustbelt (Prediction)","Rustbelt (Prediction)") # need to recode levels
rb.it.pred <- tm_shape(it.map) + tm_polygons(id = "ID_0") + 
  tm_shape(it.map.dat) + tm_polygons("rustbelt.pred", id = "METRO", palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0 ,frame = FALSE)
#tm_layout(attr.outside = TRUE, legend.outside = TRUE)
save_tmap(rb.it.pred, "Output/rb_it_pred.png")

# Predicted probabilities
rb.it.pred.prob <- tm_shape(it.map) + tm_polygons(id = "ID_0") + 
  tm_shape(it.map.dat) + tm_polygons("rustbelt.pred.prob", id = "METRO", palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0) +
  tm_layout(attr.outside = F, legend.outside = F,frame = FALSE)
save_tmap(rb.it.pred.prob, "Output/rb_it_pred_probs.png")

### 4.7 Predict into France

fr.met <- read_shape("Data/shapefiles/OECD_MA_FRA/FRA_MAs_2016.shp", as.sf = T)
fr.map <- read_shape("Data/shapefiles/fra_shapefile/FRA_adm0.shp", as.sf = T)

# Check map working okay
testmap.fr <- tm_shape(fr.map) + tm_polygons(id = "METRO") +
  tm_shape(fr.met) + tm_polygons(id = "ID_0")
testmap.fr

# Load clean non-US data

all.dat <- readRDS("Data/dat_all_clean.RDS")
fr.dat <- filter(all.dat, substr(METRO_ID,1,2) == "FR")

fr.dat$rustbelt.pred <- predict(object = rf.fit, newdata = fr.dat)
fr.dat$rustbelt.pred.prob <- predict(object = rf.fit, newdata = fr.dat, type = "prob")[,2]

fr.map.dat <- append_data(fr.met, fr.dat, key.shp = "METRO", key.data = "METRO_ID")

# Predicted classes

levels(fr.dat$rustbelt.pred) <- c("Not rustbelt (Prediction)","Rustbelt (Prediction)") # need to recode levels
rb.fr.pred <- tm_shape(fr.map) + tm_polygons(id = "ID_0") + 
  tm_shape(fr.map.dat) + tm_polygons("rustbelt.pred", id = "METRO", palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0,frame = FALSE)
#tm_layout(attr.outside = TRUE, legend.outside = TRUE)
save_tmap(rb.fr.pred, "Output/rb_fr_pred.png")

# Predicted probabilities
rb.fr.pred.prob <- tm_shape(fr.map) + tm_polygons(id = "ID_0") + 
  tm_shape(fr.map.dat) + tm_polygons("rustbelt.pred.prob", id = "METRO", palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0) +
  tm_layout(attr.outside = F, legend.outside = F, frame = FALSE)
save_tmap(rb.fr.pred.prob, "Output/rb_fr_pred_probs.png")

### 4.8 Predict into Germany

ger.met <- read_shape("Data/shapefiles/OECD_MA_DEU/DEU_MAs_2016.shp", as.sf = T)
ger.map <- read_shape("Data/shapefiles/DEU_adm_shp/DEU_adm0.shp" , as.sf = T)

# Check map working okay
testmap.ger <- tm_shape(ger.map) + tm_polygons(id = "METRO") +
  tm_shape(ger.met) + tm_polygons(id = "ID_0")
testmap.ger

# Load clean non-US data

all.dat <- readRDS("Data/dat_all_clean.RDS")
ger.dat <- filter(all.dat, substr(METRO_ID,1,2) == "DE")

ger.dat$rustbelt.pred <- predict(object = rf.fit, newdata = ger.dat)
ger.dat$rustbelt.pred.prob <- predict(object = rf.fit, newdata = ger.dat, type = "prob")[,2]

ger.map.dat <- append_data(ger.met, ger.dat, key.shp = "METRO", key.data = "METRO_ID")

# Predicted classes

levels(ger.dat$rustbelt.pred) <- c("Not rustbelt (Prediction)","Rustbelt (Prediction)") # need to recode levels
rb.ger.pred <- tm_shape(ger.map) + tm_polygons(id = "ID_0") + 
  tm_shape(ger.map.dat) + tm_polygons("rustbelt.pred", id = "METRO", palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0, frame = FALSE)
#tm_layout(attr.outside = TRUE, legend.outside = TRUE)
save_tmap(rb.ger.pred, "Output/rb_ger_pred.png")

# Predicted probabilities
rb.ger.pred.prob <- tm_shape(ger.map) + tm_polygons(id = "ID_0") + 
  tm_shape(ger.map.dat) + tm_polygons("rustbelt.pred.prob", id = "METRO", palette= "YlOrRd", title = "") +
  tm_layout(
    #title = "US Metropolitan Areas - Predictions",
    #title.size = 2.5,
    title.position = c("left","top"),
    legend.title.size = 2,
    legend.text.size = 1.2,
    legend.position = c("left","bottom"),
    legend.bg.color = "white",
    legend.bg.alpha = 0) +
  tm_layout(attr.outside = T, legend.outside = T, frame = FALSE)
save_tmap(rb.ger.pred.prob, "Output/rb_ger_pred_probs.png")
