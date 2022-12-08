# Load data ####
load('Data/Exploratory_data.RData')

# Cleaning for regression ####
data_model2 <- exploratory_data %>%
    mutate(Subject            = relevel(fct_other(Subject,
                                                  keep = c('Utilities'          , 
                                                           'Services'           , 
                                                           'Medicine_and_health',
                                                           'Leisure_and_sport'  , 
                                                           'Rural_Industry'     ,
                                                           'Local_Histories'    , 
                                                           'Mixed'              , 
                                                           'Arts'               , 
                                                           'Buildings'          
                                                                  )
                                                  ), 
                                        ref = 'Other'
                                        ),
    Area_Geodemographic_group = relevel(fct_other(Area_Geodemographic_group, 
                                                  keep = c('Rural-Urban Fringe', 
                                                           'Suburban Traits'   , 
                                                           'Town Living'       )), 
                                        ref = 'Other'
                                        ),
    Governance                = relevel(factor(Governance
                                               ), 
                                        ref = 'Government'
                                        )
           ) %>%
    dplyr::select(Size                          ,     
                  Accreditation                 , 
                  Governance                    , 
                  Subject                       , 
                  Area_Geodemographic_group     ,
                  Area_Deprivation_index_health ,
                  Area_Deprivation_index_housing,
                  Area_Deprivation_index_crime  ,
                  )


# Building regression 3 ####
model2 <- glm(Size~. , 
              family = 'binomial', 
              data = data_model2
              )

print(summary(model2
              )
      )
# Verifying assumptions ####
    ## Linearity 
    verify.linearity(data  = data_model2 %>% select(-Size),
                     model = model2
                     )
    ## Multicollinearity 
    print(car::vif(model2
             )
          )
    ## Influential factors 
    verify.influential.values(data   = data_model2,
                              model  = model2,
                              output = 'Size'
                              )
# Save model 2 ####
if(!dir.exists('Data')){
    dir.create('Data')
}
save(file = 'Data/model_2.RData', model2)

