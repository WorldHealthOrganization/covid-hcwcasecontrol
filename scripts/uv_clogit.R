# https://epirhandbook.com/univariate-and-multivariable-regression.html#base-r-3


uv_clogit <- function(d, explanatory_vars){
  
  models <- explanatory_vars %>%       # begin with variables of interest
    str_c("case_control ~ ", . , " + strata(site.godata)") %>%         # combine each variable into formula ("outcome ~ variable of interest")
    
    # iterate through each univariate formula
    map(                               
      .f = ~ clogit(                   # pass the formulas one-by-one to clogit()
        formula = as.formula(.x),      # within clogit(), the string formula is .x
        data = d)) %>%                 # dataset
    
    # tidy up each of the glm regression outputs from above
    map(
      .f = ~tidy(
        .x, 
        exponentiate = TRUE,           # exponentiate 
        conf.int = TRUE)) %>%          # return confidence intervals
    
    # collapse the list of regression outputs in to one data frame
    bind_rows() %>% 
    
    # round all numeric columns
    mutate(across(where(is.numeric), round, digits = 2))
  
  
  return(models)
}
