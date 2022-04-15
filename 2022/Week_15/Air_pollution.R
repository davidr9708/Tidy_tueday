# Libraries
library(ggtext)
library(tidytuesdayR)
library(tidyverse)
library(cowplot)
library(patchwork)

# Getting the data
air_pollution_raw <- tt_load(2022, week = 15)

death_source <- air_pollution_raw$death_source 
fuel_gdp     <- air_pollution_raw$fuel_gdp

# Function to find the mode (It is used to fill the NA's values in continent data)
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv <- uniqv[!is.na(uniqv)]
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Cleaning the data
Data <- inner_join(death_source, fuel_gdp, 
                   by = c('Entity', "Year")) %>% 
  group_by(Entity) %>%
  mutate(Continent = getmode(Continent)) %>% 
  filter(!is.na(Continent)) %>%
  rename(Risk   = `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Rate)`,
         Access = `Access to clean fuels and technologies for cooking (% of population)`,
         GDP    = `GDP per capita, PPP (constant 2017 international $)`)

## Correlation's values
cor_Access_risk <- round(cor(Data$Access, Data$Risk, use = 'complete.obs'),2)
cor_Access_GDP  <- round(cor(Data$Access, Data$GDP, use = 'complete.obs'),2)
cor_GDP_risk    <- round(cor(Data$GDP, Data$Risk, use = 'complete.obs'), 2)

# Plotting
## Fonts
sysfonts::font_add_google("Anton", "anton")
showtext::showtext_auto()

## Plot for the multiplot
Risk_access <- Data %>% 
  ggplot(aes(y = Risk, x = Access)) + 
  geom_smooth(alpha = 0.5, 
              color = '#000084', 
              fill  = 'lightgray') +
  labs(subtitle = "<span style='color:#000084'>**Higher access**</span> to clean fuels relates to <span style='color:#000084'>**less deaths**</span><br>by indoor air pollution...") +
  xlab('Access to clean fuels and technologies for cooking') +
  ylab('Mortality risk by\nindoor air pollution') +
  annotate(geom   = 'text', 
           label  = paste('R =',cor_Access_risk), 
           x      = 30, 
           y      = 50, 
           size   = 12,
           color  = '#000084', 
           family = 'anton') +
  theme(plot.background  = element_blank(),
        panel.background = element_blank(),
        legend.position  = "none",
        plot.subtitle    = element_markdown(colour = 'black', 
                                            family="anton", 
                                            size= 15,
                                            vjust = -1),
        plot.margin      = margin(0.7, 0.5, 0.5, 1, "cm"),
        axis.text.x      = element_text(family = 'anton', 
                                        colour = "lightgray"),
        axis.title.x     = element_text(family = 'anton', 
                                        colour = "darkgray"),
        axis.ticks.x     = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.text.y      = element_text(family = 'anton', 
                                        colour = "gray"),
        axis.title.y     = element_text(family = 'anton', 
                                        colour = "darkgray"))


Access_GDP <- Data %>% 
  ggplot(aes(y =Access, x = GDP)) + geom_smooth(alpha = 0.5, color = '#EDAA25', fill = 'lightgray') +
  scale_x_log10() +
  labs(subtitle ="...because, even when <span style='color:#EDAA25'>**higher accesss**</span><br>associates with <span style='color:#EDAA25'>**higher GDP**</span>...") +
  ylab('Access to clean fuels and\ntechnologies for cooking') +
  xlab('GDP per capita') + 
  annotate(geom = 'text', label = paste('R =',cor_Access_GDP), 
           x = 1500, y = 50, size = 7, color = '#EDAA25',family = 'anton') +
  theme(plot.background  = element_blank(),
        panel.background = element_blank(),
        legend.position  = "none",
        plot.subtitle    = element_markdown(family = 'anton', 
                                            colour = "black",
                                            size = 15),
        plot.margin      = margin(0.7, 0.5, 0.5, 1, "cm"),
        axis.text.x      =  element_text(family = 'anton', 
                                         colour = "gray"),
        axis.title.x     =  element_text(family = 'anton', 
                                         colour = "gray"),
        axis.ticks.x     = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.title.y     = element_text(family = 'anton', 
                                        colour = "gray"),
        axis.text.y      = element_text(family = 'anton', 
                                        colour = "gray"))


Risk_GDP <- Data %>% 
  ggplot(aes(y = Risk, x = GDP)) + 
  geom_smooth(alpha = 0.5, 
              color = '#757A62', 
              fill  = 'lightgray') +
  scale_x_log10()  +
  labs(subtitle = "...the correlation between <span style='color:#757A62'>**GDP**</span><br>and <span style='color:#757A62'>**this mortality risk**</span> is <span style='color:#757A62'>**WEAKER!**</span>") +
  xlab('GDP per capita') +
  annotate(geom   = 'text', 
           label  = paste('R =', cor_GDP_risk), 
           x      = 100, 
           y      = 100, 
           size   = 7, 
           color  = '#757A62', 
           family = 'anton') +
  ylab('Mortality risk by\nindoor air pollution') +
  theme(plot.background  = element_blank(),
        panel.background = element_blank(),
        legend.position  = "none",
        plot.title       = element_text(colour = 'black', 
                                        family = "anton",
                                        size   = 15),
        plot.subtitle    = element_markdown(colour = 'black', 
                                            family = "anton", 
                                            size   = 15),
        plot.margin      = margin(0.7, 0.5, 0.5, 1, "cm"),
        axis.title.x      =  element_text(family = 'anton', 
                                          colour = "gray"),
        axis.text.x      =  element_text(family = 'anton', 
                                         colour = "gray"),
        axis.ticks.x     = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.text.y      =  element_text(family = 'anton', 
                                         colour = "gray"),
        axis.title.y      =  element_text(family = 'anton', 
                                          colour = "gray"),
        plot.caption     = element_text(color  = "darkgray",
                                        size   = 15,
                                        margin = margin(t = 15, b = 5)))

## Extra text 
extra <- ggdraw() + 
  draw_label("...this is not due to an increment of GDP...",
             hjust = 0.8, 
             vjust = -1.4,
             size  = 15,
             fontfamily = 'anton')

## Multiplot 
png('air_pollution.png', width = 1300, height = 700, res = 100,units = 'px')

((Risk_access) + ((extra/Access_GDP/ Risk_GDP) + plot_layout(heights = c(1,10, 10)))) +
  plot_annotation(title  = "NOT CONFOUNDING: CLEAN FUELS & TECHNOLOGIES\nREDUCE DEATH BY INDOOR AIR POLLUTION",
                  caption = "Data: Our world in data | Visualization: Daniel Rodriguez (@davidr9708)") &
  theme(plot.title   = element_text(colour = 'black', 
                                    family="anton", 
                                    size=25,
                                    face = 'bold'),
        plot.caption = element_text(colour = 'gray', 
                                    family="anton",
                                    face = 'bold'),
        plot.margin  = margin(0.7, 0.5, 0.5, 1, "cm"))

dev.off()
