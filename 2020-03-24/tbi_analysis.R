## Tidy Tuesday on Brain Injuries 
# 3/24/202
# By Nyssa Silbiger

### Load libraries #########
library(tidyverse)
library(packcircles)
library(ggthemes)
library(patchwork)
library(ggsci)
library(gganimate)

# https://github.com/njsilbiger/TidyTuesday_CSUN/blob/master/Brain_Injury_032420/braininjurycode.R
# load the data #############
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

# replace NA with mean
tbi_age <- 
  tbi_age %>% 
  group_by(age_group, type) %>% 
  mutate(number_est = replace(number_est, 
                              is.na(number_est), 
                              mean(number_est, na.rm = TRUE))) %>% 
  mutate(rate_est = replace(rate_est, 
                            is.na(rate_est), 
                            mean(rate_est, na.rm=TRUE)))

# Make a plot with # of injuries #########
total<-tbi_age %>%
  filter(age_group != "0-17") %>%
  mutate(age_group = factor(age_group, 
                            levels =c("0-4", "5-14","15-24","25-34","35-44","45-54","55-64","65-74","75+", "Total")))%>% 
  ggplot(aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  theme_solarized() +
  scale_fill_jama() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Total number of injuries") +
  theme(legend.title = element_blank())

# plot with # of deaths #############
deaths <- tbi_age %>%
  filter(age_group != "0-17") %>%
  filter(type == "Deaths") %>%
  mutate(age_group = factor(age_group, 
                            levels =c("0-4", "5-14","15-24","25-34","35-44","45-54","55-64","65-74","75+", "Total")))%>% 
  ggplot(aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  theme_solarized() +
  scale_fill_jama() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Total number of deaths") +
  theme(legend.title = element_blank())

## calculate percent of deaths per category ###########
dead <- tbi_age %>%
  filter(age_group != "0-17") %>%
  filter(type == "Deaths") %>%
  rename("dead" = "number_est") %>%
  select(age_group,injury_mechanism, dead)

tbi_age %>%
  filter(age_group != "0-17") %>%
  group_by(age_group,injury_mechanism) %>%
  summarise(total = sum(number_est)) %>%
  left_join(dead) %>%
  mutate(perc.dead = 100*(dead/total)) %>%
  ungroup() %>%
  mutate(age_group = factor(age_group, 
                            levels =c("0-4", "5-14","15-24","25-34","35-44","45-54","55-64","65-74","75+", "Total"))) %>% 
  ggplot(aes(x = age_group, y = perc.dead, fill = injury_mechanism)) +
  geom_bar(stat = "identity") +
  theme_solarized() +
  scale_fill_jama() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Percentage of deaths") +
  theme(legend.title = element_blank())

# group plot of the numbers   #######
p1 <- total + deaths + plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")


##### YEAR DATA
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')

tbi_year %>% 
  filter(injury_mechanism != "Total") %>% 
  ggplot() + 
  geom_bar(aes(factor(injury_mechanism), rate_est, fill = type),
           position='dodge', stat='identity') + 
  coord_flip() + 
  facet_wrap(~year)

# https://github.com/opus1993/myTidyTuesday/blob/master/BrainInjury/BrainInjury.md
p <- tbi_year %>% 
  filter(injury_mechanism != "Total") %>% 
  ggplot(aes(x = injury_mechanism,y = number_est, fill = type )) +
  geom_bar(position = "dodge", stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE, option = "A" ) +  
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  coord_flip() +
  transition_states(year, transition_length = 10, state_length = 1) +
  labs(title = "Traumatic Brain Injury (TBI) Incidence in {closest_state}",
       subtitle = "US CDC", x = "Cause of Injury", y = "Count",
       caption = paste0("Jim Gruman ", Sys.Date()))+
  theme(plot.title.position = "plot") +  
  scale_y_log10(labels = scales::comma) 
  