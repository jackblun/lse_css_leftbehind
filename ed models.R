library(tidyverse)
library(rstanarm)
library(broom)
library(quantreg)
library(dummies)

metro.data <- read_csv('data/Clean Metro Data.csv')


#### Are Rust-Belt metro areas different to normal areas based off observables? ####

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


get.significant.rust.differences <- function(model, variable.name){
  non.zero <- tidy(model, parameters = 'varying', intervals = TRUE) %>% 
    filter(term == 'rust') %>% 
    filter(!data.table::between(0, lower = lower, upper = upper)) %>% 
    mutate(variable = variable.name)
  return(non.zero)
}

 gdp.share.non.zero <- get.significant.rust.differences(gdp.share.fit, 'gdp share')
 unemployment.non.zero <- get.significant.rust.differences(unemployment.rate.fit, 'unemp rate')
 pop.share.non.zero <- get.significant.rust.differences(pop.share.fit, 'pop share')
 pop.dep.old.non.zero <- get.significant.rust.differences(pop.dep.ratio.old.fit, 'pop dep ratio old')
 pop.dep.young.non.zero <- get.significant.rust.differences(pop.dep.ratio.young.fit, 'pop dep ratio young')
 
 rust.significant.differences <- rbind(gdp.share.non.zero,
       unemployment.non.zero,
       pop.share.non.zero,
       pop.dep.old.non.zero,
       pop.dep.young.non.zero)
rust.significant.differences


#### Exploring quantile effects ####

metro.dummies <- dummy.data.frame(as.data.frame(metro.data), names = 'country') %>% 
  as.tibble %>% 
  filter(Year == 2000) %>% 
  filter(VAR == "POP_DEP_RATIO_OLD") %>% 
  subset(select = -c(X1,
                     METRO_ID,
                     `Metropolitan areas`,
                     `Unit`,
                     `PowerCode`,
                     `Reference Period`,
                     `type`,
                     `nchar`,
                     VAR,
                     Year))  

country.dummies <- dummy(metro.data$country)

metro.test <- cbind(country.dummies, metro.data$Value, metro.data$rust)


qr.pop.dep.ratio.old <- rq(Value ~ . + 0,
                           data = metro.dummies,
                           tau=c(.05, .25, .5, .75, .95))

qr.pop.dep.ratio.old.tidy <- tidy(qr.pop.dep.ratio.old) %>% 
  filter(term == 'rust')

ggplot(qr.pop.dep.ratio.old.tidy, aes(tau, estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

lm(Value ~ . + 0,
   data = metro.dummies)
