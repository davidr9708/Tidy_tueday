library(tidyverse)

# Getting the data
Link  <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
covid <- read.csv(Link)

# Cleaning the data
Last_vaccine_rate <- covid %>% 
  group_by(location) %>% 
  mutate(max_vaccin    = max(people_fully_vaccinated_per_hundred, na.rm = TRUE),
         Herd_inmunity = ifelse(people_fully_vaccinated_per_hundred < 80, 'NO', 'Yes')) %>%
  
  filter(max_vaccin == people_fully_vaccinated_per_hundred,
         max_vaccin <= 100) %>% 
  select(location,
         people_fully_vaccinated_per_hundred,
         Herd_inmunity) %>% 
  ungroup()

# Selecting %countries with fully vaccination < 80%
Perc_low_vaccination <- Last_vaccine_rate %>% 
  mutate(number_countries = n()) %>%
  group_by(Herd_inmunity) %>%
  summarise(Perc_country = round((n()/mean(number_countries))*100)) %>%
  ungroup() %>%
  .[.$Herd_inmunity == 'NO',] %>% 
  .$Perc_country
  
# Saving the plot
png('Covid_vaccination.png', width = 5000, height = 2500, res = 300)

## Plotting
Last_vaccine_rate %>% ggplot(aes(x   = people_fully_vaccinated_per_hundred, 
            fill = Herd_inmunity)) +
  geom_dotplot(method   = 'histodot', 
               binwidth = 2)  +
  
  scale_x_continuous(limits = c(0,110),
                     labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values=c('darkred', 'darkgray')) +

  ### Text
  annotate(label = c(paste0('"',Perc_low_vaccination, '%"'),'bold(countries)', 'have~bold(less~than~"80%")', 'people~fully~vaccinated'), 
           geom  = 'text', 
           color = 'darkred',
           x     = c(34.5, 46.2, 49.2, 50.2), 
           y     = c(0.75, 0.8,0.755, 0.71),  
           size  = c(18,7,5.5, 5.5), 
           parse = TRUE) +
  
  labs(title    = 'WE STILL HAVE A LONG WAY TO GO',
       subtitle = '% Fully vaccinated people in the world countries - Jan 14/2021',
       x        = '% Fully Vaccinated People',
       caption  = "\nSource: Mathieu, E., Ritchie, H., Ortiz-Ospina, E. et al. A global database of COVID-19 vaccinations. Nat Hum Behav (2021).\nDaniel Rodriguez | @davidr9708") + 
  
  theme(plot.title       = element_text(size = 40, 
                                        face = "bold", 
                                        hjust = 0.07),
        plot.subtitle    = element_text(size = 25, 
                                        hjust = 0.07),
        axis.ticks.y     = element_blank(), 
        axis.title.y     = element_blank(),
        axis.text.y      = element_blank(),
        axis.text.x      = element_text(color = "grey50"),
        axis.title.x     = element_text(color = "grey50"),
        legend.position  = "none",
        panel.background = element_blank(),
        plot.caption     = element_text(color = "grey50",
                                        size = 10,
                                        hjust = -0.002))

dev.off()
