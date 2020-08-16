library(tidyverse)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

combos_occuring_more_than_10 <- coffee_ratings %>% 
  mutate(grading_date = lubridate::mdy(grading_date),
         rounded_date = lubridate::floor_date(grading_date, unit = "month"),
         country_species = str_c(country_of_origin,": ",species)) %>%
  filter(rounded_date >= '2015-01-01') %>% 
  # only keep the combos that were rated 10+ times
  group_by(country_species) %>% 
  summarize(ratings = n_distinct(rounded_date)) %>% 
  filter(ratings >= 10) 

coffee_ratings %>% 
  mutate(grading_date = lubridate::mdy(grading_date),
         rounded_date = lubridate::floor_date(grading_date, unit = "quarter"),
         country_species = str_c(country_of_origin,": ",species)) %>%
  # only keep the combos that were rated 10+ times
  filter(country_species %in% combos_occuring_more_than_10$country_species,
         rounded_date >= '2015-01-01' ) %>% 
  group_by(rounded_date,country_species) %>% 
  summarize(avg_score = mean(total_cup_points),
            n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(rounded_date,avg_score,color=country_species)) +
  geom_point() +
  geom_line()

