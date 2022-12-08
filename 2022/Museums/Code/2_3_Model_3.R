# Load data ####
load('Data/Exploratory_data.RData')
load('Data/model_2.RData')

# Cleaning for regression ####
data_model3 <- exploratory_data %>%
    mutate(Subject                   = relevel(fct_other(Subject, 
                                                        keep = c('Local_Histories'    , 
                                                                 'Medicine_and_health', 
                                                                 'Buildings'          , 
                                                                 'Services'           , 
                                                                 'Utilities'          
                                                                 )
                                                        ), 
                                               ref = 'Other'
                                               ),
           Area_Geodemographic_group = relevel(fct_other(Area_Geodemographic_group, 
                                                         keep = c('Rural-Urban Fringe', 
                                                                  'Town Living'       
                                                                  )
                                                         ), 
                                               ref = 'Other'
                                               ),
           Governance                = relevel(factor(Governance
                                                      ), 
                                               ref = 'Government'
                                               )
           ) %>%
    dplyr::select(Size                     ,     
                  Accreditation            , 
                  Governance               , 
                  Subject                  , 
                  Area_Geodemographic_group
                  )

# Building regression 3 ####
model3 <- glm(Size~. , 
              family = 'binomial', 
              data   = data_model3
              )

print(summary(model3
              )
      )

# Verifying assumptions ####
    ## Linearity doesn't apply here

    ## Multicollinearity
    print(car::vif(model3
                   )
          )

    ## Influential factors
    verify.influential.values(data   = data_model3 %>% select(-Size),
                              model  = model3,
                              output = 'Size'
                              )
    
# Comparing against previous model ####
lrtest(model3, 
       model2
       )

# Save model 3 ####
if(!dir.exists('Data')){
  dir.create('Data')
}
save(file = 'Data/model_3.RData', model3)
    