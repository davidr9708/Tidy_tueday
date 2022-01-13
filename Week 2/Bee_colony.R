library(tidytuesdayR)
library(tidyverse)
library(mice)
library(lubridate)
library(geomtextpath)

# Getting the data
Colonies <- tt_load("2022-01-11")

Colony <- Colonies$colony

# Cleaning
Colony_summary <- Colony %>% 
  mutate(day = fct_recode(months, '/04/01' = "April-June",
                                                     '/06/01' = "July-September",
                                                     '/09/01' = "October-December",
                                                     '/01/01' = "January-March"),
                          date_complete = ymd(paste0(year,day))) %>%
  filter(state != 'United States') %>%
  select(date_complete, state, colony_reno_pct, colony_lost_pct) 
  
## Imputing missing values
imputed_values <- mice(Colony_summary,m = 5,maxit = 5, meth='pmm')
Colony_imputed <- complete(imputed_values, 2)

#Plotting
png('summer_winter_colony_losses.png', width = 4000, height = 2000, res = 300)


Colony_imputed %>% 
  gather(pct_type, pct ,colony_reno_pct, colony_lost_pct)  %>%
  mutate(pct_type = fct_recode(pct_type, 'Colonies lost' = 'colony_lost_pct',
                                         'Colonies renovated' = 'colony_reno_pct')) %>%
  ggplot(aes(y = pct, x = date_complete, color = pct_type)) +
  ## Geoms
  geom_point(alpha = 0.02, size = 2) +
  geom_textsmooth(aes(label = pct_type, colour = pct_type),
                    method = "loess", formula = y ~ x, size = 5, linetype = 3, 
                    fontface = 2, linewidth = 1.5) + 
  ## Scales
  scale_color_brewer(type = 'qual', palette = 'Dark2', direction = -1) + 
  scale_x_date(date_breaks = "1 years", date_labels = "%Y",  expand = c(0.02, 10))+
  scale_y_continuous(limits = c(0,10), labels = function(x) paste0(x, "%")) +
  labs(title = 'Shall not colony renewal be higher than colony loss?',
       subtitle = 'Losing more Honey Bee Colonies than restoring',
       y = '%Honey Bee Colonies',
       caption = "\nDaniel Rodriguez | @davidr9708") + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.04),
        plot.subtitle = element_text(size = 14,  hjust = 0.025),
        plot.caption = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank())


dev.off()
