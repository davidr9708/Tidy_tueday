# Libraries ####
library(pacman)
p_load(tidyverse,
       cowplot  ,
       magick   ,
       tibble   
       )

# Data to plot ####
raw_data_plot <- as.data.frame(coef(summary(model3
                                            )
                                    )
                               )

data_plot <- rownames_to_column(raw_data_plot,
                                "term"
                                ) %>%
    filter(term != '(Intercept)'
           ) %>%
    mutate(Estimate = ifelse(Estimate <0   ,
                             exp(1/Estimate
                                 )*-1      ,
                             exp(Estimate
                                 )
                             ),
           term     = str_replace_all(term       , 
                                      '[:punct:]', 
                                      ' '
                                      ),
           Category = str_extract(term                                                   ,
                                  'Subject|Governance|Geodemographic group|Accreditation'
                                  ),
           term     = str_replace_all(term                                                        ,
                                      'Subject|Governance|Area Geodemographic group|Accreditation', 
                                      ''
                                      ),
           term     = fct_reorder(term         , 
                                  Estimate     , 
                                  .desc = FALSE
                                  ),
           ranking  = rank(-Estimate
                           ),
           type     = ifelse(ranking <= 3, 
                             'Top'        ,
                             'Bottom' 
                             )
           ) %>%
    arrange(Estimate
            )

# Plot ####
my_plot<- data_plot %>%
      ggplot(aes(y = term    , 
                 x = Estimate,
                 label = term,
                 fill = type
                 )
             ) + 
    ## Geoms
      geom_bar(stat = 'identity', 
               width = 0.8
               ) +
      geom_text(aes(x = ifelse(Estimate > 0,
                               -0.2        ,
                               0.2
                               ),
                    color = type
                    ),
                size  = 4.8,
                vjust = 0  ,
                hjust = ifelse(data_plot$Estimate > 0, 
                               1                     , 
                               0
                               )
                )+
      geom_text(aes(x = ifelse(Estimate > 0,
                               -0.2        ,
                               0.2 
                               ),
                    color = type,
                    label = paste('|'     ,
                                  Category
                                  )
                    ),
                size  = 3  ,
                vjust = 1.4,
                hjust = ifelse(data_plot$Estimate > 0, 
                               1                     , 
                               0
                               )
      ) +
      geom_text(aes(x = ifelse(Estimate > 0  ,  
                               Estimate + 0.2, 
                               Estimate - 0.2
                               ),
                    color = type,
                    label = paste(round(Estimate,
                                        1
                                        ), 
                                  'times'
                                  )
                    ),
                hjust = ifelse(data_plot$Estimate > 0, 
                               0, 
                               1
                               )
      ) +
      ## Annotate
      annotate(geom      = 'text'  , 
               hjust     = 0       , 
               fontface  = 'italic',
               x         = rep(20,
                               4
                               ), 
               y         = c(5.5,
                             5.5,
                             5.1,
                             3.5
                             ),
               color     = c('darkblue',
                             'black'   ,
                             'darkblue', 
                             'black'
                             ),
               label     = c('Unknown governance', 
                             '                                    and',
                             'subjects about utilities or Services',
                             'are the best indicators for small size...\n\n\n...further analysis on them might show us\nthe root causes for small size and \nlead decisions'
                             )
               ) +
      ## Plot text
      labs(title    = "Indicators for small size in England's Musseums"                 ,
           subtitle = "How much the odds ratio of small size increases for each factor?",
           caption  = "Data: Museweb | Plot's Author: @davidr9708"
           ) +
      ## Scales
      scale_x_continuous(labels = function(y) y + 1,
                         expand = c(0,
                                    0
                                    )              ,
                         limits = c(-8,
                                    37
                                    )
                         ) +
      scale_fill_manual(values = c('gray', 
                                   'darkblue'
                                   )
                        ) +
      scale_color_manual(values = c('darkgray', 
                                    'darkblue'
                                    )
                         ) +
    ## Theme
      theme(panel.background = element_blank(),
            plot.margin      = unit(c(1, 
                                      1, 
                                      1, 
                                      2), 
                                    "cm"
                                    ), 
            plot.title       = element_text(size = 29, 
                                            hjust = 0.48
                                            ),
            axis.ticks       = element_blank(),
            axis.text        = element_blank(),
            axis.title       = element_blank(),
            legend.position  = 'none'         , 
            plot.subtitle    = element_text(size =19    , 
                                            hjust = 0.41
                                            ), 
            plot.caption     = element_text(color = 'gray'
                                            )  
            )

# Saving plot ####
plot <- ggdraw() + 
  draw_plot(my_plot
            ) +
  draw_image("Images/England_flat.png", 
             x     = -0.40, 
             y     = 0.4  , 
             scale = 0.08
             ) +
  draw_image("Images/Museum.png", 
             x = -0.33   , 
             y = 0.44    , 
             scale = 0.20
             )
  
ggsave(file = 'Museums_factors_plot.png', 
       limitsize = FALSE                ,
       width = 14.72                    , 
       height = 7
       )


