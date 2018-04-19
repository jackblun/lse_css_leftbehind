##Are Rust-Belt metro areas different to normal areas based off observables?

library(tidyverse)
library(rstanarm)
library(broom)
library(quantreg)
library(dummies)
library(latex2exp)

metro.data <- read_csv('data/Clean Metro Data.csv')

remove.constant.cols <- function(dataframe){
  # Function that drops any columns with zero variance to avoid perfect collinearity
  df <- dataframe[, apply(dataframe, 2, var, na.rm = TRUE) != 0]
  return(df)
}

#### Exploring some initial variables of interest using a bayesian hierarchical model with country level groupings ####


# Using the baseline year of 2000 to test whether rust belt regions are significantly different from non rust belt regions using a number of variables
gdp.share.fit <- stan_glmer(Value ~ rust | country, 
                            data = metro.data %>% 
                              filter(Year == 2000) %>% 
                              filter(VAR == 'GDP_SHARE'))
unemployment.rate.fit <- stan_glmer(Value ~ rust | country,
                                    data = metro.data %>% 
                                      filter(Year == 2000) %>% 
                                      filter(VAR == 'UNEMP_R'))
pop.share.fit <- stan_glmer(Value ~ rust | country,
                            data = metro.data %>% 
                              filter(Year == 2000) %>% 
                            filter(VAR == "POP_SHARE"))
pop.dep.ratio.old.fit <- stan_glmer(Value ~ rust | country,
                                    data = metro.data %>% 
                                      filter(Year == 2000) %>% 
                                      filter(VAR == "POP_DEP_RATIO_OLD"))
pop.dep.ratio.young.fit <- stan_glmer(Value ~ rust | country,
                                      data = metro.data %>% 
                                        filter(Year == 2000) %>% 
                                        filter(VAR == "POP_DEP_RATIO_YOUNG"))



#### Exploring quantile effects ####

create.dummies <- function(data.to.use, var.name){
  ## A function to split the country column into multiple dummy variable columns
  df <- dummy.data.frame(as.data.frame(data.to.use), names = 'country') %>% 
    as.tibble %>% 
    filter(Year == 2000) %>% 
    filter(VAR == var.name) %>% 
    subset(select = -c(X1,
                       METRO_ID,
                       `Metropolitan areas`,
                       `Unit`,
                       `PowerCode`,
                       `Reference Period`,
                       `type`,
                       `nchar`,
                       VAR,
                       Year)) %>% 
    remove.constant.cols
  return(df)
}

metro.dummies.pop.dep.old <- create.dummies(metro.data, "POP_DEP_RATIO_OLD")
 


qr.pop.dep.ratio.old <- rq(Value ~ . + 0,
                           data = metro.dummies.pop.dep.old,
                           tau=c(seq(0.05, .95, .05)))

qr.pop.dep.ratio.old.tidy <- tidy(qr.pop.dep.ratio.old) %>% 
  filter(term == 'rust')

qr.pop.dep.old.plot <- ggplot(qr.pop.dep.ratio.old.tidy, aes(tau, estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  ylim(-5, 5) +
  ggtitle('Dependency-ratio-old regressed on rust-belt dummy by quantile with country fe')
qr.pop.dep.old.plot



metro.dummies.unemp.share <- create.dummies(metro.data, 'UNEMP_SHARE') 





qr.unemp_share <- rq(Value ~. + 0,
                     data = metro.dummies.unemp.share,
                     tau = c(seq(0.05, .95, .05)))

qr.unemp.results <- tidy(qr.unemp_share) %>% 
  filter(term == 'rust')



qr.unemp.plot <- ggplot(qr.unemp.results, aes(tau, estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  ylim(-5, 5) +
  ggtitle('Unemployment share regressed on rust-belt dummy by quantile with country fe')


qr.unemp.plot



gdp_share.dummies <- create.dummies(metro.data, 'GDP_SHARE')

qr.gdp <- rq(Value ~ . + 0,
             data = gdp_share.dummies,
             tau = c(seq(0.05, .95, .05)))

qr.gdp.results <- tidy(qr.gdp) %>% 
  filter(term == 'rust')


qr.gdp.plot <- ggplot(qr.gdp.results, aes(tau, estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  ylim(-5, 5) +
  ggtitle('GDP share regressed on rust-belt dummy by quantile with country fe', subtitle = 'Missing error bars are >100')
qr.gdp.plot


## Significant Differences ####
posterior.unemp <- as.array(unemployment.rate.fit)
unemployment.observables <- bayesplot::mcmc_intervals(posterior.unemp, pars = c('b[rust country:US]',
  'b[rust country:UK]',
  'b[rust country:BE]',
                                                    'b[rust country:FR]',
                                                    'b[rust country:IT]')) +
  ggtitle('Do Rust Belt areas have higher unemployment?', subtitle = TeX('$U_{jc} = \\alpha_c + \\beta_cD_{jc} + \\eta_{jc}$'))

unemployment.observables


get.significant.rust.differences <- function(model, variable.name = 'var'){
  # Find any cases where rust belt metro areas are different to non-rust belt areas.
  non.zero <- tidy(model, parameters = 'varying', intervals = TRUE) %>% 
    filter(term == 'rust') %>% 
    filter(!data.table::between(0, lower = lower, upper = upper)) %>% 
    mutate(variable = variable.name)
  return(non.zero)
}


glmer.func <- function(var.to.use){
  # A function that takes one variable and applies the GLM to it
  metro.data.2000 <- metro.data %>% 
    filter(Year == 2000)
  model <- stan_glmer(Value ~ rust | country,
                      data = metro.data.2000 %>% 
                        filter(VAR == var.to.use))
  return(model)
}

# Applying the balance test to every variable in that dataset and then collecting significant terms.
var.list <- unique(metro.data$VAR)

sig.diff <- lapply(var.list, function(x) get.significant.rust.differences(glmer.func(x), variable.name = x))

sig.diff.table <- data.table::rbindlist(sig.diff)

