library(tidytuesdayR)
library(tidyverse)
# Getting the data
Airmen <- tt_load(2022, week = 6)[[1]]

library(showtext)
font_add_google(name = "Bangers", family = "bangers")
font_add_google(name = "Saira", family = "saira")
showtext_auto()

west <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR",
          "WA", "IL" ,"IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")

Airmen_clean <- Airmen %>%
  filter(pilot_type == 'Single engine', !is.na(state)) %>% group_by(state) %>%
  summarise(Rate = round(sum(number_of_aerial_victory_credits)*100/n()),
            No_pilots = n()) %>%
  mutate(Rank = rank(desc(Rate)),
         West = ifelse(state %in% west, ifelse(Rank<11,'Ty','y'), ifelse(Rank<11,'Tn','n')),
         state = fct_reorder(state, Rate),
         state = fct_recode(state, 
                            'Washington' = 'WA',
                            'Wisconsin' = 'WI',
                            'Oregon' = 'OR',
                            'Oklahoma' = 'OK',
                            'Iowa' = 'IA',
                            'Missouri' = 'MO',
                            'Kansas' = 'KS',
                            'Florida' = 'FL',
                            'Nebraska' = 'NE',
                            'California' = 'CA',
                            'Tennessee' = 'TN',
                            'New York' = 'NY',
                            'West Virginia' = 'WV',
                            'Virginia' = 'VA',
                            'Georgia' = 'GA',
                            'Connecticut' = 'CT',
                            'Indiana' = 'IN',
                            'South Carolina' = 'SC',
                            'Illinois' = 'IL',
                            'Ohio' = 'OH',
                            'Texas' = 'TX',
                            'Massachusetts' = 'MA',
                            'Alabama' ='AL',
                            'New Jersey' = 'NJ',
                            'Maryland' = 'MD',
                            'Pennsylvania' = 'PA',
                            'North Carolina' = 'NC',
                            'District of Columbia' = 'DC'
                            )
         ) %>% 
  filter(Rate > 0) %>% arrange(Rate)
  
Airmen_clean %>% ggplot(aes(x = Rate, y = state, label = Rate)) +
  geom_bar(aes(fill = West), stat = 'identity') + 
  geom_rect(aes(xmin = 0, xmax = 250, ymin=18.5, ymax = 28.5),alpha = 0.02, fill = 'lightgray', color = 'white') +
  geom_bar(aes(fill = West), stat = 'identity') + 
  geom_text(hjust = 1.2, color = 'white') +
  scale_x_continuous(limits = c(0,250), expand = c(0,0)) +
  scale_fill_manual(values = c('lightgray', 'gray', 'darkblue', 'lightblue')) +
  annotate(geom = 'text', 
           label = c('The 10 first states were mostly from','West and Middle West Region',
                     '...but the next positions were mostly from','the other regions'), 
           x= c(150, 150, 150, 150), y =c(26,25, 12, 11), color = c('black', 'darkblue', 'black', 'darkgray'), size = 5,
           fontface = c('plain', 'bold','plain', 'bold')) +
  labs(title = "Which states had the best single engine pilots?",
       subtitle = "Number Aerial Victory Credits per 100 single engine pilots",
       caption = "Data: The Tuskegee Airmen Challenge | Visualization: Daniel Rodriguez (@davidr9708)") +
  theme(plot.background  = element_blank(),
        panel.background = element_blank(),
        legend.position  = "none",
        plot.title       = element_text(colour = 'black', 
                                        family="Saira", 
                                        face = "bold", 
                                        size=30),
        plot.subtitle    = element_text(colour = 'black', 
                                        family="karla", 
                                        size= 14, 
                                        face = 'bold'),
        plot.margin      = margin(0.7, 0.5, 0.5, 1, "cm"),
        axis.title.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank(),
        axis.title.y     = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.text.y      =  element_text(family = 'Saira', 
                                         face   = ifelse(Airmen_clean$West == "Ty", "bold", 'plain'),
                                         colour = ifelse(Airmen_clean$West == "Ty", "darkblue", 
                                                        ifelse(Airmen_clean$West == "y", "lightblue",
                                                               "darkgray"))),
        plot.caption     = element_text(color = "darkgray",
                                        size = 10,
                                        margin = margin(t = 15, b = 5)))
