library(tidyverse)
library(tidytuesdayR)
library(extrafont)

# loadfonts(device = "win")

tuesdata <- tidytuesdayR::tt_load(2020, week = 31)

penguins <- tuesdata$penguins

penguins %>% 
  group_by(species,island) %>% 
  count(sort = T)

penguins %>% 
  filter(!is.na(sex)) %>% 
  mutate(id = row_number()) %>% 
  group_by(species, year, sex) %>% 
  summarize(bill_length_mm = mean(bill_length_mm),
            bill_depth_mm = mean(bill_depth_mm),
            flipper_length_mm = mean(flipper_length_mm),
            body_mass_g = mean(body_mass_g)) %>% 
  pivot_longer(cols = bill_length_mm:body_mass_g, names_to = "measure", values_to = "value") %>%
  pivot_wider(
    names_from = c(sex, measure),
    values_from = value,
  ) %>% 
  mutate(bill_length_diff = (female_bill_length_mm / male_bill_length_mm)/ ((female_bill_length_mm+male_bill_length_mm)/2),
         bill_depth_diff = (female_bill_depth_mm / male_bill_depth_mm)/ ((female_bill_depth_mm+male_bill_depth_mm)/2),
         flipper_length_diff = (female_flipper_length_mm / male_flipper_length_mm)/ ((female_flipper_length_mm+male_flipper_length_mm)/2),
         body_mass_diff = (female_body_mass_g / male_body_mass_g)/ ((female_body_mass_g+male_body_mass_g)/2)) %>% 
  select(species, year, bill_length_diff:body_mass_diff) %>% 
  pivot_longer(cols = bill_length_diff:body_mass_diff, names_to = "measure_difference", values_to = "value") %>% 
  mutate(measure_difference = factor(measure_difference,levels = c("bill_depth_diff","bill_length_diff","flipper_length_diff","body_mass_diff"))) %>% 
  ggplot(aes(measure_difference, value, fill=measure_difference, label=str_c("+",round(value*100,2),"%"))) +
  geom_col() +
  geom_label(size = 3,
             nudge_y = .005) +
  scale_fill_viridis_d(alpha = 0.7) +
  facet_grid(species~year) +
  theme_minimal() +
  labs(title = "Penguin proportion differences across gender and species, over time",
       subtitle = "Despite having longer and deeper bills, males are virtually the same weight as females, across species",
       x = "",
       y = "Male proportion difference (vs. female)",
       caption = "Source: Dr. Kristen Gorman and the Palmer Station, Antarctica LTER | Viz: @NosyOwl") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=c("bill_depth_diff" = "Bill Depth", 
                              "bill_length_diff" = "Bill Length",
                              "flipper_length_diff" = "Flipper Length",
                              "body_mass_diff" = "Body Mass")) +
  theme(text = element_text(family = "Tw Cen MT"),
        axis.text.x = element_text(face="bold",size=7),
        legend.position="none") +
  ggsave("R/tidytuesday/exos/2020_w31_penguins.png", dpi = 320, height = 8, width = 8)


  