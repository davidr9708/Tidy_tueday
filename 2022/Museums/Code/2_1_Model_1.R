# Load data ####
load('Data/Exploratory_data.RData')

# Cleaning for regression ####
data_model1 <- exploratory_data %>%
    mutate(Subject                   = relevel(fct_other(Subject, 
                                                         keep = c('Utilities'          , 
                                                                  'Services'           , 
                                                                  'Medicine_and_health',
                                                                  'Leisure_and_sport'  , 
                                                                  'Rural_Industry'     ,
                                                                  'Local_Histories'    , 
                                                                  'Mixed'              , 
                                                                  'Arts'               , 
                                                                  'Buildings'          )
                                                         ), 
                                               ref = 'Other'
                                               ),
           Area_Geodemographic_group = relevel(fct_other(Area_Geodemographic_group, 
                                                         keep = c('Rural-Urban Fringe', 
                                                                  'Suburban Traits'   , 
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
    dplyr::select(Size                             ,     
                  Accreditation                    , 
                  Governance                       , 
                  Subject                          , 
                  Area_Geodemographic_group        ,
                  Area_Deprivation_index_employment,
                  Area_Deprivation_index_health    ,
                  Area_Deprivation_index_housing   ,
                  Area_Deprivation_index_income    ,
                  Area_Deprivation_index_crime     ,
                  Area_Deprivation_index_education ,
                  Area_Deprivation_index_services  
                  )


# Building regression 3 ####
model1 <- glm(Size~. , 
              family = 'binomial', 
              data   = data_model1
              )

print(summary(model1
              )
      )
# Verifying assumptions ####
    ## Linearity 
    verify.linearity(data  = data_model1 %>% select(-Size),
                     model = model1
                     )

    ## Multicollinearity
    print(car::vif(model1)
          )

    ## Influential factors
    verify.influential.values(data   = data_model1,
                              model  = model1,
                              output = 'Size'
                              )
# Save model 1 ####
if(!dir.exists('Data')){
    dir.create('Data')
}
save(file = 'Data/model_1.RData', model1)
