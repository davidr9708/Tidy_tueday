---
title: "Chocolates"
output: html_document
---

### Plot's main message
Using beans from the same country or a different one doesn't make a **relevant** difference in the chocolate rating.

### Getting the data

The data come from [Flavors of Cacao](http://flavorsofcacao.com/chocolate_database.html). It was taken from the `tidytuesdayR` library.

```{r, message = FALSE, warning=FALSE, results='hide'}
library(tidytuesdayR)
Chocolate <- tt_load("2022-01-18")[[1]]
```

### Wrangling

To build the **bootstrapped** confidence intervals, I need to create two data frames, one with the `rating` of the chocolates where the bean was from the **same country** of the manufacturer and other where they were **different countries**.

```{r, message = FALSE, warning=FALSE}
library(tidyverse)

same_country <- Chocolate %>% 
  filter(company_location == country_of_bean_origin) %>%
  select(rating)

different_country  <- Chocolate %>% 
  filter(company_location != country_of_bean_origin) %>%
  select(rating)
```
```{r, echo=FALSE}
head(same_country, 3)
head(different_country, 3)
```
*Pd: I only took the `rating` column because it was the only one that I was going to need.*

### Bootstrapping

First, I defined the `mean` function to use for bootstrapping:

```{r mean_boot}
mean_boot <- function(data, indx){
  df  <- data[indx,]
  med <- mean(df$rating)
  return(med)
}
```

Then, using the functions of the library `boot`, I bootstrapped the data for `same_country` and `different_country`, and saved them in a new object.

```{r bootstrapping, message=FALSE, warning=FALSE}
library(boot)

different_boot <- boot(data = different_country, 
                       statistic = mean_boot, 
                       R = 3000)

same_boot      <- boot(data = same_country, 
                       statistic = mean_boot, 
                       R = 3000)
```

Finally, I reckoned the confidence intervals from the bootstrapped data.

```{r bootstrapped ci, message = FALSE, warning = FALSE}
different_ci <- boot.ci(different_boot, type ="basic")
same_ci      <- boot.ci(same_boot, type ="basic")
```
```{r, echo=FALSE}
print(different_ci)
print(same_ci)
```

### Plotting

I already have the data needed to plot, now I have to put it together into a data frame. 

```{r}
manufacturer <- c("Different country\nto the manufacturer's",
                  "Same country\nas the manufacturer's")
mean_  <- round(c(different_ci$t0, same_ci$t0), 2)  
low    <- round(c(different_ci$basic[,4], same_ci$basic[,4]),2)
high   <- round(c(different_ci$basic[,5], same_ci$basic[,5]), 2)
ci_boot <- data.frame(mean_, low, high, manufacturer)
```

*PD: The factors in `manufacturer` used `\n` to make easier the plotting.*

I needed to get some fonts for the graph.

```{r, message=FALSE, warning=FALSE}
library(showtext)

font_add_google("Karla", "karla")
font_add_google("Lobster", "lobster")

showtext_auto() 
```

Finally, I built the graph.

```{r}
ci_boot %>%
  ggplot(aes(x = mean_, y = 1, 
             xmax =high, xmin=low, ymin = 0, ymax= 1,
             fill = manufacturer, color = manufacturer,
             label = manufacturer)) +
  
  # Geoms
  geom_rect(color = 'white') +
  geom_text(aes( y = 0.5, x = ifelse(manufacturer == "Same country\nas the manufacturer's", low - 0.32, low + 0.34)), 
            size = 7,  family="lobster") +  
  
  # Scales
  scale_x_continuous(breaks = seq(1,5, by = 0.5), limits = c(1,4)) +
  scale_fill_manual(values = c("sienna3","tan4")) +
  scale_color_manual(values = c("sienna3","tan4"))+
  
  # Additional text
  annotate(geom = 'text', 
           label = c('The~difference~between~the~ratings',
                     'is~bold(less~than ~"0.3...")', 
                     '"...it"~is~bold(too~low)','~to~be~"crucial..."',
                   '"...it"~is~better~to~use~a~bean','from~bold(the~same~country)~due',
                   'to~importation~costs'), 
           x = c(1.3,1.3,1.6, 1.6,1.9,1.9, 1.9), 
           y = c(0.7,0.65, 0.5, 0.45, 0.3,0.25,0.2), 
           parse = TRUE, 
           size = 4.5, family = "karla", color = 'tan3') +
  
  labs(title = "What's better, a bean from the same country or a different one?",
       subtitle = "Bootstrapped confidence intervals of mean chocolate rating by bean origin",
       x = "Rating",
       caption = "Data: Flavors of Cacao | Visualization: Daniel Rodriguez (@davidr9708)") +
  
  # Additional features
  theme(plot.background  = element_blank(),
        panel.background = element_blank(),
        legend.position  = "none",
        plot.title       = element_text(colour = 'sienna4', 
                                        family="lobster", 
                                        face = "bold", 
                                        size=30),
        plot.subtitle    = element_text(colour = 'sienna4', 
                                        family="karla", 
                                        size= 14, 
                                        hjust = 0.01, 
                                        face = 'bold'),
        plot.margin      = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.title.x     = element_text(color = "sienna4", 
                                        face = 'bold'),
        axis.text.x      = element_text(color = "sienna4"),
        axis.ticks.x     = element_line(color = "tan3"),
        axis.title.y     = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.text.y      = element_blank(),
        plot.caption     = element_text(color = "tan3",
                                        size = 10,
        margin           = margin(t = 15, b = 5)))
```
