
library(tidyverse)

dat <- read.csv("tidytuesday/data/2018-10-30/r_downloads_year.csv")

# See which countries have downloaded the file more, per population 

# First get the population for each country. Or the ratio of the population with internet access? 

# for simplicity we keep the top 10 countries 
top_countries <- 
  dat %>%  
  filter(is.na(country)==F) %>% 
  group_by(country) %>% 
  tally() %>%  
  top_n(10) %>% 
  select(country)

scope <- dat %>% filter(country %in% top_countries$country)

scope %>% 
  group_by(date,country,os) %>% 
  count() %>% 
  ggplot(aes(lubridate::month(date),n,color=country)) +
  geom_smooth(se=F)

dat %>% 
  group_by(date) %>% 
  count()
