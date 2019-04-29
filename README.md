# Stanford-Police-Data

library(tidyverse)
library(cowplot)

#import data
url <- "https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv"

combined_data <- readr:: read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv") %>% 
  mutate(location = str_to_title(location)) %>% 
  mutate_if(is.character, as.factor)

#state names
states <- read_delim("states.txt", delim = "-", col_names = c("state_name", "state"), trim_ws = TRUE) %>% 
  mutate_if(is.character, as.factor)

#fixing variable type
combined_data$driver_race <- as.character(combined_data$driver_race)

#compare stop and search rates between race
relative2white <- 
  combined_data %>% 
  group_by(state, location) %>% 
  mutate(stop_rate_relative = stop_rate/stop_rate[driver_race == "White"],
         search_rate_relative = search_rate/search_rate[driver_race == "White"]) %>% 
  select(location, state, driver_race, stops_per_year, stop_rate, search_rate, stop_rate_relative, search_rate_relative) %>% 
  ungroup()


#state to select and highlight
sel_state <- "SC"
number_counties <- 3
main_color <- "LightBlue"
signal_color <- "black"

#fixing bull shit pt 2
relative2white$state <- as.character(relative2white$state)

#summarised dataset by states
avg_srr_all <-
  relative2white %>% 
  filter(driver_race == "Black", !is.na(stop_rate_relative)) %>% 
  group_by(state) %>% 
  summarise(avg_srr = weighted.mean(stop_rate_relative, w = stops_per_year)) %>% 
  ungroup() %>% 
  left_join(states, by = "state") %>% 
  mutate(state = fct_reorder(state, avg_srr), 
    fillcolor = if_else(as.character(state) == sel_state, "Signal", "Main"))
main_plot_title <- paste("Black drivers in", avg_srr_all$state_name[avg_srr_all$fillcolor == "Signal"], "are getting stopped by the police\n", round(avg_srr_all$avg_srr[avg_srr_all$fillcolor == "Signal"]),
                         "times more frequently than white drivers")

plot_main <-
avg_srr_all %>% 
  ggplot(aes(x = state, y = avg_srr, fill = fillcolor))+
  geom_col ()+
  geom_text(aes(label = round(avg_srr,1)), nudge_y = 0.15)+
  coord_flip()+
  scale_fill_manual(values = c("Signal" = signal_color, "Main" = main_color), guide = FALSE) +
  labs(x = "State", title = main_plot_title, caption = "Data: The Stanford Open Policing Project\nvisualization: Jerry Slutsky") +
  scale_x_discrete(breaks = avg_srr_all$state, labels = avg_srr_all$state_name) + #state_names
  scale_y_continuous(expand = c(0,0), limits = c(0, max(avg_srr_all$avg_srr)*1.1)) + #dynamic scale
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0, size = 14),
        plot.caption = element_text(size = 9, hjust = 0))

#relative stop rate by counties in the selected state
srr_combined <-

#counties with the highest relative stop rate
bind_rows(
  relative2white %>% 
    filter(driver_race == "Black",
           !is.na(stop_rate_relative),
           state == sel_state) %>% 
    arrange(-stop_rate_relative) %>% 
    head(number_counties) %>% 
    mutate(type = "worst"),

#counties with the lowest relative stop rate
relative2white %>% 
  filter(driver_race == "Black",
         !is.na(stop_rate_relative),
         state == sel_state) %>% 
  arrange(stop_rate_relative) %>% 
  head(number_counties) %>% 
  mutate(type = "best")
) %>% 
  mutate(location = str_squish(str_remove(location, "County"))) #remove "County" from location name
         
#Subtitle for subplot
title_subplot <- paste("The", number_counties, "least problematic and most problematic counties\nin", sel_state)

#subplot
plot_sub_counties <-
  srr_combined %>% 
  mutate(location = fct_reorder(location, stop_rate_relative)) %>% 
  ggplot(aes(x = location, y = stop_rate_relative))+
  geom_col(aes(fill = type), width = 0.7)+
  geom_text(aes(label = round(stop_rate_relative,1)), nudge_y = 1, size = 3)+
  coord_flip()+
  scale_fill_manual(values = c("best" = "green4", "worst" = "IndianRed2"), labels = c("Least problematic counties", "Most problematic counties"))


cowplot::ggdraw()+
  draw_plot(plot_main)+
  draw_plot(plot_sub_counties, x = 0.5, y = 0, width = 0.5, height = 0.5)
cowplot::ggsave("stop_rate.jpg", width = 12, height = 9)

![stop_rate](https://user-images.githubusercontent.com/48695787/56916925-23c94400-6a88-11e9-849a-7ae219c1db38.jpg)
