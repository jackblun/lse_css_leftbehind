if (!require("pacman")) install.packages("pacman")
# installs those packages that need installing, and loads the rest
pacman::p_load(dplyr, readr, grep, data.table, magrittr, tidyr)

#######################################
# Prep
#######################################

# Import dataset
d <- readRDS("E:/Bibliotheken/Desktop/cssatlse/dat_all_clean.RDS")
View(d)
table(d_new$rustbelt)

# Map in newest rustbelt metro codes
metro_codes <- read_csv("GitHub/lse_css_leftbehind/data/Aggregated Metro Codes.csv")
metro_codes$rustbelt_new <- 1
metro_codes <- rename(metro_codes, METRO_ID = metro_codes)

d_new <- d %>% left_join(metro_codes, by = "METRO_ID")
d_new$rustbelt <- case_when(d_new$rustbelt_new == 1 ~ 1,
                            is.na(d_new$rustbelt_new) ~ 0,
                            TRUE ~ 0)
d_new <- select(d_new, -rustbelt_new)

# Take out all national observations (because we want average of met. areas, not all)
d2 <- d_new %>% filter(nchar(METRO_ID) == 5)

# Create new variables relative to national averages
d2 <- d2 %>% mutate(country = substr(METRO_ID, 0, 2)) # Country index
d2 %<>% data.table
d2 %<>% gather(var, val, 2:23)
d2 %<>% data.table
d2[, mval := mean(val, na.rm = TRUE), by = .(country, var)] 
d2[, dval := val - mval]
d2 %<>% select(METRO_ID, rustbelt, country, var, dval) %>% spread(var, dval)

# Subset all rustbelt regions
drb <- d2 %>% filter(!is.na(rustbelt), rustbelt == 1)

#######################################
# Prep
#######################################

drb_pca1 <- select(drb, -METRO_ID, -rustbelt, -emp_r, -unemp_R, -country)
pr.out1 <- prcomp(drb_pca1, scale = TRUE)
pr.out1$rotation
biplot(pr.out1, scale=0)
pr.var1 = pr.out1$sdev^2
pve1 = pr.var1/sum(pr.var1)
pve1
plot(pve1, xlab ="Principal Component", ylab ="Proportion of Variance 
     Explained", ylim = c(0, 1), type='b')
plot(cumsum(pve1), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type='b')

drb_pca2 <- drb %>% select(-METRO_ID, -rustbelt, -country) %>% na.omit()
pr.out2 <- prcomp(drb_pca2, scale = TRUE)
pr.out2$rotation
biplot(pr.out2, scale=0)
pr.var2 = pr.out2$sdev^2
pve2 = pr.var2/sum(pr.var2)
pve2
plot(pve2, xlab ="Principal Component", ylab ="Proportion of Variance 
     Explained", ylim = c(0, 1), type='b')
plot(cumsum(pve2), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type='b')

results1 <- data.frame(pr.out1$rotation[,1:2])
results1$var <- rownames(pr.out1$rotation)

results2 <- data.frame(pr.out2$rotation[,1:2])
results2$var <- rownames(pr.out2$rotation)

results1 %>% arrange(desc(PC1))
results1 %>% arrange(desc(PC2))

results2 %>% arrange(desc(PC1))
results2 %>% arrange(desc(PC2))

# Results 2 looks more usable

###########################
# K-MEANS CLUSTERING
###########################

km.out1 <- kmeans(drb_pca1, 4, nstart = 200)
cbind(drb$METRO_ID, km.out1$cluster)

# Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- drb_pca1
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main="Elbow plot - k-means clustering")

# Two seems optimal - but results in only one US region being classified in the second group
km.out1 <- kmeans(drb_pca1, 2, nstart = 200)
cbind(drb$METRO_ID, km.out1$cluster)

# So choose four
km.out1 <- kmeans(drb_pca1, 4, nstart = 200)
cbind(drb$METRO_ID, km.out1$cluster)

# Result: No real clusters. US and UK/EU are not sufficiently different to be distinguished
# given the data

###########################
# HIERARCHICAL CLUSTERING
###########################

# hc.complete = hclust(dist(x), method = "complete")
# cutree(hc.complete, 2)
# par(mfrow = c(1, 3))
# plot(hc.complete, main = "Complete Linkage", xlab = "", 
#      sub = "", cex = .9)