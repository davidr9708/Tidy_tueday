# Libraries
library(tidyverse)

# Functions for logistic regression assumptions
## Linearity
verify.linearity <- function(data, model){
  
    numeric_data <- data %>%
        dplyr::select_if(is.numeric) 
    
    predictors        <- colnames(numeric_data)
    probabilities     <- predict(model, type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

    ## Bind the logit and tidying the data for plot
    numeric_data <- numeric_data %>%
        mutate(logit = log(probabilities/(1-probabilities))) %>%
        gather(key   = "predictors"     , 
               value = "predictor.value", 
               -logit)
    
    plot <- numeric_data %>% 
        ggplot(aes(logit, 
                   predictor.value
                   )
               )  +
        geom_point(size  = 0.5, 
                   alpha = 0.5
                   ) +
        geom_smooth(method = "loess"
                    ) +
        facet_wrap(~predictors, 
                   scales = "free_y"
                   ) +
        labs(title = "Linear assumption"
             ) +
        theme_bw()
    
    print(plot)
}

# Influential values
verify.influential.values <- function(data, model, output){
  
    model.data <- augment(model) %>% 
        mutate(index = 1:n()) 
  
    plot <- ggplot(model.data, 
                   aes(index, 
                       .std.resid))            + 
        geom_point(aes_string(color = output), 
                   alpha = .5)                  +
        labs(title = "Influential values"
             ) +
        theme_bw()
    
    print(plot)
    
    influential_values <- model.data %>% 
        filter(abs(.std.resid) > 3)
    
    No_values <- nrow(influential_values)
    
    print(paste0('There are ', No_values, ' influential values.'))
    print(influential_values)
}
