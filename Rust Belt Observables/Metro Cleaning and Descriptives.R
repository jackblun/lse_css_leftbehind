## Creating a unified metro dataset and some exploratory descriptives.
library(tidyverse)
library(ggplot2)
library(data.table)


## Data cleaning and merging ####
metro.gdp <- read_csv('data/gdp_cities.csv')
metro.inequ <- read_csv('data/gdp_cities.csv')
metro.labour <- read_csv('data/lab_market.csv')
metro.pop <- read_csv('data/population.csv')
metro.pop.age <- read_csv('data/population_age.csv')
metro.rust.codes <- read_csv('data/Aggregated Metro Codes.csv')


metro.data <- rbindlist(list(metro.gdp,
                             metro.inequ,
                             metro.labour,
                             metro.pop,
                             metro.pop.age))


metro.data <- metro.data %>% 
  separate(METRO_ID, into = 'country',
           sep = '[0-9]', extra = 'drop', remove = FALSE)

metro.data <- metro.data %>% 
  mutate(type = ifelse(METRO_ID %in% metro.rust.codes$metro_codes, 'Rust Belt', 'Non Rust Belt'),
         rust = ifelse(METRO_ID %in% metro.rust.codes$metro_codes, 1, 0))

metro.data <- metro.data %>% 
  subset(select = -c(Variables,
                     TIME,
                     `Unit Code`,
                     `PowerCode Code`,
                     `Reference Period Code`,
                     `Flag Codes`,
                     `Flags`))
metro.data$type <- as.factor(metro.data$type)

metro.data$nchar <- nchar(metro.data$country)
metro.data <- metro.data %>% 
  filter(nchar == 2)
# write.csv(metro.data, file = 'data/Clean Metro Data.csv')
                       
#### Descriptive Statistics ####
  


metro.non.rust <- metro.data %>% 
  subset(select = -c(rust))




ggplot(metro.data %>% 
         filter(Year == 2000) %>% 
         filter(VAR == 'GDP_SHARE'), aes(Value)) +
  geom_histogram()


ggplot(metro.data %>% 
         filter(Year == 2000) %>% 
         filter(VAR == 'GDP_SHARE'), aes(Value)) +
  geom_histogram(data = metro.non.rust %>% 
                   filter(Year == 2000) %>% 
                   filter(VAR == 'GDP_SHARE'), fill = 'grey', alpha = 0.5) +
  geom_histogram(colour = 'black') +
  facet_wrap(~rust) +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle('Metro GDP as a share of Country GDP')



spain.metro <- metro.data %>%
  filter(country == 'US') %>% 
  filter(VAR == 'LABOUR_PRODUCTIVITY')

spain.metro.rust <- spain.metro %>% 
  subset(select = -c(rust))

ggplot(spain.metro, aes(Year, Value)) +
  geom_jitter(data = spain.metro.rust, colour = 'grey', alpha = 0.2) +
  geom_jitter() +
  facet_wrap(~ rust) +
  guides(colour = FALSE) +
  theme_bw()



within.country.plot <- function(country_func, variable){
  
  title <- paste0('Country: ', country_func, ' Variable: ', variable)  
  country.data <- metro.data %>% 
    filter(country == country_func) %>% 
    filter(VAR == variable)
  
  country.data.subset <- country.data %>% 
    subset(select = -c(type))
  
  ggplot(country.data, aes(Year, Value, group = METRO_ID, colour = METRO_ID)) +
    geom_line(data = country.data.subset, colour = 'grey', alpha = 0.3) +
    geom_line() +
    facet_wrap(~type) +
    guides(colour = FALSE) +
    theme_bw() +
    ggtitle(title)
}


  

employment.share.US <- within.country.plot('US', 'EMP_SHARE')
lab.prod.US <- within.country.plot('US', "LABOUR_PRODUCTIVITY")
pop.dependence <- within.country.plot('US',"POP_DEP_RATIO_OLD")

employment.share.US
lab.prod.US



within.country.plot('US', "POP_DEP_RATIO_OLD")
within.country.plot('BE',"POP_DEP_RATIO_OLD" )

within.country.plot('US', 'UNEMP_R')

within.country.plot('DE', 'POP_DEP_RATIO_OLD')
pop.dependence




#### Potential Presentation Plots ####
final.US.gdp.share.plot <- within.country.plot('US', 'GDP_SHARE') +
  ggtitle('Metropolitan US GDP Share')
final.US.gdp.share.plot

final.US.unemployment.plot <- within.country.plot('US', 'UNEMP_R') +
  ggtitle('Metropolitan US Unemployment Rate')
final.US.unemployment.plot

