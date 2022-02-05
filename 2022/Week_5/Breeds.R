
library(showtext)
library()
font_import()
font_add_google(name = "Lobster")
font_add_google(name = "Bangers")
showtext_auto()
font_families_google()
# Getting data
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

clean_breeds <- cbind(breed_traits, breed_rank_all[,-1])%>% 
  janitor::clean_names() %>% 
  pivot_longer(x2013_rank:x2020_rank, names_to = "year", values_to = "rank") %>% 
  filter(coat_length %in% c('Long', 'Medium', 'Short'), !is.na(rank)) %>%
  mutate(year = parse_number(year)) %>% 
  mutate(ranking = ifelse(rank < 98, 
                          'High', 'Low')) %>% 
  group_by(coat_length) %>%
  mutate(Total = n()) %>%
  group_by(ranking,
           coat_length) %>%
  mutate(worst_dog =max(rank))

x_plot <- c(2.5, 2, 2.7)
y_plot <- c(1,2.5,4)



breeds_to_plot <-clean_breeds %>%
  group_by(ranking,
           coat_length) %>%
  summarise(percent = round(n()/mean(Total)*100)) %>% group_by(ranking) %>% arrange(coat_length) %>%
  filter(ranking == 'Low') %>%
  mutate(
    x = x_plot,
    y = y_plot) %>% cbind(Image = breed_length)

# Rank by coat
Short_coat_Low_rank <- clean_breeds %>% 
  filter(ranking == 'Low', coat_length == 'Short', rank == worst_dog) %>% .$image

Long_coat_Low_rank <- clean_breeds %>% 
  filter(ranking == 'Low' & coat_length == 'Long', rank == worst_dog) %>% .$image

Medium_coat_Low_rank <- clean_breeds %>% 
  filter(ranking == 'Low' & coat_length == 'Medium', rank == worst_dog) %>% .$image

breed_length <- c(Long_coat_Low_rank[1], Medium_coat_Low_rank[1], Short_coat_Low_rank[1])

# Labels 
  ## Percentage
  Long_percent <- paste0('"',breeds_to_plot$percent[breeds_to_plot$coat_length == 'Long'],'%"')
  Short_percent <- paste0('"', breeds_to_plot$percent[breeds_to_plot$coat_length == 'Short'],'%"')
  Medium_percent <- paste0('"', breeds_to_plot$percent[breeds_to_plot$coat_length == 'Medium'],'%"')
  ## General
  dog_coats <- c('~~"for"~dog','breeds~with')
  dog_lengh <- c('bold(SHORT~COATS)', 'bold(MEDIUM~COATS)', 'bold(LONG~COATS)')

  ## All
  text_lables <- c(Long_percent, Medium_percent, Short_percent, 
                   rep(dog_coats,3), dog_lengh)

  x_percent <- c(4.7, 5.2, 4.7)
  x_general <- c(5.4, 5.6, 6.7,7, 5.4, 5.6)
  x_breed   <- c(5.1, 6, 5.1)


  y_percent <- c(1, 2.8, 4)
  y_general <- c(1.1,0.9,3, 2.6, 4.1 ,3.9)
  y_breed   <- c(0.6, 2, 3.64)

  
  
  x_labels <- c(x_percent, x_general,x_breed)
  y_labels <- c(y_percent, y_general,y_breed)
  
  
  png('Dog_Breeds.png', width = 1100, height = 800, res = 100,units = 'px')  
  # Plot
breeds_to_plot %>%
  ggplot(.) +
 # No 2-9 images
  geom_image(aes(x, y, image = Image), size = c(0.2,0.4,0.2), asp = 1.2)  +
  labs(title = 'Medium coat lenght are\n the MOST UNPOPULAR',
          subtitle = 'Likelihood to be a bad rated breed',
       caption  = "Data: American Kennel Club\nVisualization: Daniel Rodriguez | @davidr9708") +
  coord_fixed(xlim = c(0, 10), ylim = c(0, 5), expand = FALSE, clip = "off") +
  annotate(geom = 'text', 
           x = x_labels, 
           y= y_labels, family ="Bangers",
           label = text_lables, size = c(c(11,25,11),c(6,6,11,11,6,6, 10,18,10)), 
           color = c('gray', 'darkgray','gray', 'gray','gray','darkgray','darkgray','gray','gray', 'gray','darkgray','gray'),parse = TRUE) +
  
theme(panel.background = element_blank(),
        panel.grid       = element_blank(),
        plot.background  = element_blank(),
        plot.title       = element_text(size = 60, face = 'bold', family ='Bangers'),
        plot.subtitle    = element_text(size = 25, face = 'italic',family ='Bangers', hjust = 0.02),
        plot.caption     = element_text(color = "darkgray", size = 10, hjust = 0.94,  margin = margin(t = 15, b = 5)),  
        axis.title.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank(),
        axis.title.y     = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank())
dev.off()  

