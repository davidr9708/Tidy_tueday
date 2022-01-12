library(tidytuesdayR)
library(tidyverse)
library(mice)

# Getting the data
Colonies <- tt_load("2022-01-11")

Colony <- Colonies$colony

# Cleaning
Colony_summary <- Colony %>% mutate(season = fct_recode(months, 'Summer' = "April-June",
                                      'Summer' = "July-September",
                                      'Winter' = "October-December",
                                      'Winter' = "January-March")) %>% 
  group_by(season, year) %>% 
  summarise(loss = sum(colony_lost)/sum(colony_n)*100) %>% ungroup()

## Imputing missing values
imputed_values <- mice(Colony_summary,m=5,maxit=5,meth='pmm')
Colony_imputed <- complete(imputed_values, 2)

#Plotting
png('summer_winter_colony_losses.png', width = 4000, height = 2000, res = 300)

Colony_imputed %>% mutate(colony_index = ifelse(year == 2015, loss, 0)) %>%
  group_by(season) %>%
  mutate(colony_change = ((loss/max(colony_index)))) %>%
  ggplot(aes(x = year, y = colony_change, color = season, label = season),fontface='bold') + 
           geom_line(size =1)  + geom_text(aes(x = 2021.2, y = ifelse(year==2021,colony_change,NA))) + 
  scale_color_brewer(type = 'qual', palette = 'Dark2', direction = -1) +
  scale_x_continuous(n.breaks = 6) +scale_y_continuous(limits = c(-0.05,0.4), labels = scales::percent_format(accuracy = 1)) +
  annotate(label =c('Losses~bold(increase)~by~time~"for"~"both,"','~summer~and~winter...', '...but~losses~changes~are~higher~"for"','bold(summer)~than~winter.'), geom = 'text', x = c(2015.5,2015.27,2020, 2019.82), y = c(0.15, 0.14,0.30,0.29),parse = TRUE,
           size =4)+
  labs(title = 'Summer is getting more harmful for bee colonies',
       subtitle = 'Colonies loss changes over time',
       y = '% Colonies loss increment') + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 10, vjust = 0.7, hjust = 0.95,face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank())

dev.off()
