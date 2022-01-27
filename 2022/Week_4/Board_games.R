library(tidytuesdayR)
library(tidyverse)
library(showtext)

# Getting the data
Games   <- tt_load('2022-01-25')
details <- Games[[1]]
ratings <- Games[[2]]

## Merging the data sets
Games_merged <- merge(details, ratings, by = 'id')

# Plotting
## Fonts
font_add_google(name = "Bangers", family = "bangers")
font_add_google(name = "Saira", family = "saira")
showtext_auto()

## Graph
png('Board_games.png', width = 1300, height = 700, res = 100,units = 'px')

Games_merged %>%
  ggplot(aes(y = average, 
             x = playingtime))    +
  geom_smooth(colour = 'yellow')  +
  
  ## Scales
  scale_x_log10() +
  scale_y_continuous(breaks = c(5,7,9))  + 
  coord_cartesian(ylim = c(5, 9)) +
  
  ## Text
  annotate(geom  = 'text',
           label = c('"Usually,"~bold(quick~games)',
                     '~have~a~bold(not~bad)~"score..."', 
                     '"...it"~looks~like~people~bold(enjoy~more)~the',
                     '~games~with~a~~bold(longer~duration...)',
                     '"...maybe"~longer~games~let~you',
                     '~~~~bold(interact~better~with~others...)',
                     '...but~bold(too~long)~games',
                     '~~might~start~to~bold(bore.)'),
           y     = c(6.2, 6, 7.1,6.9, 8.1, 7.9 , 8.6, 8.4), 
           x     = c(2,2, 40,40, 500,500, 10000, 10000), 
           color = 'darkgray',
           size  = 4,
           parse = TRUE) +
  ## Labels
  labs(title    = "MORE time to enjoy board games!",
       subtitle = "AVERAGE RATING vs log(PLAYING TIME)",
       caption  = "Data: Board Games Geek | Kaggle\nVisualization: Daniel Rodriguez | @davidr9708",
       x        = "log(playing Time)",
       y        = "Average Rating 1/10") +
  ## Additional features
  theme(panel.background = element_blank(),
        panel.grid       = element_blank(),
        plot.background  =  element_rect(fill = 'black'),
        legend.position  = "none",
        plot.margin      = unit(c(1, 0.1, 0.1, 0.1), "cm"),
        plot.caption     = element_text(color = "darkgray", size = 10, hjust = 0.94,  margin = margin(t = 15, b = 5)),                   
        plot.title       = element_text(colour = 'white',   size = 50, hjust = 0.02, family="bangers", face = "bold"),
        plot.subtitle    = element_text(colour = 'white',   size = 14, hjust = 0.02, family="saira",   face = 'bold'),
        axis.title.x     = element_text(color = "white",    vjust = 5, hjust = 0.05, face = 'bold'),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank(),
        axis.title.y     = element_text(color = "white",    vjust = -15, hjust = 0.03 , face = 'bold'),
        axis.text.y      = element_text(color = "gray", hjust = -120),
        axis.ticks.y     = element_blank())
        

dev.off()
