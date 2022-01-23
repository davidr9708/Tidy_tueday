library(tidytuesdayR)
library(tidyverse)
library(boot)
library(showtext)
library(patchwork)

# Getting the data
Chocolate <- tt_load("2022-01-18")[[1]]

# Wrangling the data
same_country <- Chocolate %>% 
  filter(company_location == country_of_bean_origin) %>%
  select(rating)

different_country  <- Chocolate %>% 
  filter(company_location != country_of_bean_origin) %>%
  select(rating)
  
# Bootstrapping
## Function
mean_boot <- function(data, indx){
  df  <- data[indx,]
  med <- mean(df$rating)
  return(med)
}

different_boot <- boot(data = different, statistic= mean_boot, R = 3000)
different_ci   <- boot.ci(different_boot , type="basic")

same_boot <- boot(data = same, statistic= median_boot, R = 3000)
same_ci   <- boot.ci(same_boot , type="basic")

manufacturer <- c("Different country\nto the manufacturer's","Same country\nas the manufacturer's")
mean_  <- round(c(different_ci$t0, same_ci$t0), 2)  
low    <- round(c(different_ci$basic[,4], same_ci$basic[,4]),2)
high   <- round(c(different_ci$basic[,5], same_ci$basic[,5]), 2)
ci_boot <- data.frame(mean_, low, high, manufacturer)

# Plotting
## Fonts
font_add_google("Karla", "karla")
font_add_google("Lobster", "lobster")
showtext_auto()

## Saving
png('Chocolates.png', width = 1300, height = 700, res = 100,units = 'px')

ggplot(ci_boot, aes(x = mean_, y = 1, fill = manufacturer, label = manufacturer,
                    color = manufacturer,
                    xmax =high, xmin=low, ymin = 0, ymax= 1)) + 
  geom_rect(color = 'white') +
  geom_text(aes(x = ifelse(manufacturer =="Same country\nas the manufacturer's", low-0.27, low + 0.3), 
                y = 0.5), size = 7,  family="lobster") +  
  scale_x_continuous(breaks = seq(1,5, by = 0.5), limits = c(1,4)) +
  scale_fill_manual(values = c("sienna3","tan4")) +
  scale_color_manual(values = c("sienna3","tan4"))+
  annotate(geom = 'text', 
           label = c('The~difference~between~the~ratings','is~bold(less~than ~"0.3...")', 
                     '"...it"~is~bold(too~low)','~to~be~"crucial..."',
                     '"...it"~is~better~to~use~a~bean','from~bold(the~same~country)~due','to~importation~costs'), 
           x = c(1.3,1.3,1.6, 1.6,1.9,1.9, 1.9), 
           y = c(0.7,0.65, 0.5, 0.45, 0.3,0.25,0.2), parse = TRUE, size = 4.5,
           family = "karla", color = 'tan3') +
  labs(title = "What's better, a bean from the same country or a different one?",
       subtitle = "Bootstrapped confidence intervals of mean chocolate rating by bean origin",
       x = "Rating",
       caption = "Data: Flavors of Cacao | Visualization: Daniel Rodriguez (@davidr9708)") +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(colour = 'sienna4', family="lobster", face = "bold", size=30),
        plot.subtitle = element_text(colour = 'sienna4', family="karla", size= 14, hjust = 0.01, face = 'bold'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.title.x  = element_text(color = "sienna4", face = 'bold'),
        axis.text.x  = element_text(color = "sienna4"),
        axis.ticks.x  = element_line(color = "tan3"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(
          color = "tan3",
          size = 10,
          margin = margin(t = 15, b = 5)))



dev.off()

