library(tidyverse)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(mapproj)
library(usmap)

# download data
tuesdata <- tidytuesdayR::tt_load('2020-02-25')
measles <- tuesdata$measles

# preprocess the data, get summary stats
measles_summary <- 
  measles %>% 
  select(2, 9, 10, 15, 16) %>% 
  filter(mmr != -1) %>% 
  na.omit() %>% 
  mutate(total_vaccination = round(enroll * mmr/100), 0) %>% 
  select(1:6)

# group_by state
measles_by_state <-
  measles_summary %>% 
  group_by(state) %>% 
  summarize(total_vaccination = sum(total_vaccination),
            lat = mean(c(max(lat), min(lat))),
            long = mean(c(max(lng), min(lng))))

# merge with statepop
names(measles_by_state)[1] <- "full"
names(measles_by_state)[2] <- "value"

measles_by_state_merged <- 
  measles_by_state %>%
  inner_join(statepop, by = "full") %>% 
  select(1, 2, 5, 6)

# plot the map
plot_usmap(data = measles_by_state_merged, values = "value", 
           labels = TRUE, label_color = "white") + 
  scale_fill_gradient(low = "#ADD8E6", high = "#E13D3D") +
  labs(x = "", y = "",
       title = "Total number of vaccinations",
       subtitle = "Total number of vaccinations - School's Measles, Mumps, and Rubella (MMR)\n in a given state.",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN",
       x = "", y = "") + 
  theme(
    plot.title = element_text(margin = margin(b = 10),
                              color = "#22222b", face = "bold", size = 17, family = "Arial"),
    plot.subtitle = element_text(margin = margin(b = 25),
                                 color = "#22222b", size = 12, family = "Arial"),
    plot.caption = element_text(margin(t = 20),
                                color = "#22222b", size = 10, family = "Arial"),
    legend.position = "right",
    legend.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = "#F7F7F7")
  )
