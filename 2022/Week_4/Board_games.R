library(tidytuesdayR)
library(tidyverse)
library(showtext)

font_add_google(name = "Bangers", family = "bangers")
font_add_google(name = "Saira", family = "saira")
showtext_auto()

Games <- tt_load('2022-01-25')
details <- Games[[1]]
ratings <- Games[[2]]
Games_merged <- merge(details, ratings, by = 'id')


png('Board_games.png', width = 1300, height = 700, res = 100,units = 'px')
Games_merged %>%
  ggplot(aes(y = average, x = playingtime)) +
  scale_x_log10() +
  geom_smooth(colour = 'yellow')  +
  scale_y_continuous(breaks = c(5,7,9))  + coord_cartesian(ylim=c(5, 9)) +
  annotate(geom = 'text',
           label = c('"Usually,"~bold(quick~games)',
                     '~have~a~bold(not~bad)~"score..."', 
                     '"...it"~looks~like~people~bold(enjoy~more)~the',
                     '~games~with~a~~bold(longer~duration...)',
                     '"...maybe"~longer~games~let~you',
                     '~~~~bold(interact~better~with~others...)',
                     '...but~bold(too~long)~games',
                     '~~might~start~to~bold(bore.)'),
           y = c(6.2, 6, 7.1,6.9, 8.1, 7.9 , 8.6, 8.4), 
           x = c(2,2, 40,40, 500,500, 10000, 10000), color = 'darkgray',
           parse = TRUE, size = 4) +
  labs(title = "MORE time to enjoy board games!",
       subtitle = "AVERAGE RATING vs log(PLAYING TIME)",
       x = "log(playing Time)",
       y = "Average Rating 1/10",
       caption = "Data: Board Games Geek | Kaggle\nVisualization: Daniel Rodriguez | @davidr9708") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background =  element_rect(fill = 'black'),
        legend.position = "none",
        plot.title = element_text(colour = 'white', family="bangers", face = "bold", size= 50, hjust = 0.02),
        plot.subtitle = element_text(colour = 'white', family="saira", size = 14, hjust = 0.02, face = 'bold'),
        axis.title.x  = element_text(color = "white", face = 'bold', hjust = 0.05, vjust = 5),
        axis.text.x  =  element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.y = element_text(color = "white", face = 'bold', hjust = 0.03, vjust = -15),
        axis.ticks.y = element_blank(),
        axis.text.y  =  element_text(color = "gray", hjust = -120),
        plot.margin = unit(c(1, 0.1, 0.1, 0.1), "cm"),
        plot.caption = element_text(
          color = "darkgray",
          size = 10,
          margin = margin(t = 15, b = 5), hjust = 0.94))

dev.off()
