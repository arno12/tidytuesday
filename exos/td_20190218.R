# Tidy tuesday phds
library(tidyverse)
library(hrbrthemes)

phd <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

phd %>% 
  filter(major_field == "Psychology" & !field %in% c("Social sciences","Clinical psychology")) %>% 
  ggplot(aes(year,n_phds,color=field)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~broad_field) +
  theme_ipsum_rc() +
  labs(title = "An example graph", subtitle="test",)
