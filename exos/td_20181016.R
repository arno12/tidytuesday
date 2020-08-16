
dat <- read.csv("data/2018-10-16/recent-grads.csv")

library(tidyverse)
library(extrafont)
font_import()

plot <- dat %>% 
  ggplot(aes(Unemployment_rate,Median,label=str_to_title(Major),color=ShareWomen)) +
  geom_point() +
  geom_text(check_overlap = T,vjust = -0.5, nudge_y = 0.1,size=2.5) +
  theme_minimal() +
  scale_color_gradient(name="Share of Women",low = "#003366",high = "#eacc5d" ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels=scales::percent) +
  xlab("Unemployment Rate") +
  ylab("Median Income") + 
  ggtitle("Median Income and Unemployment Rate") +
  theme(text=element_text(family = "Roboto Light",size = 8),title = element_text(size=12))

ggsave(plot,filename = "data/2018-10-16/income_unemp.png",width = 8,height = 5)



