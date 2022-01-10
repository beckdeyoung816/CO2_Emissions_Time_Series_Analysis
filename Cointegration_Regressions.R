##-------------------------------------------------------------##
##                    Cointegration_Regressions.R
##-------------------------------------------------------------##

# This script runs the cointegration regression models and stores the results tidily

library(tidyverse)
library(cointReg)
library(tsDyn)
library(aTSA)
source("Prep_Data.R")

# Variables we have selected
var_to_model <- c("CPI", "mean_tmp", "mean_pre","CO2_emissions")

make_tab <- function(x, country){
  # Make a nice table of the coefficients and p values from a cointegration regression
  # @x: regression object
  # @country: Country to check
  cbind(Country = country, 
        Estimate = x$theta %>% round(3), # Coefficients
        Std.Err = x$sd.theta %>% round(3), # Standard Error
        "t value" = x$t.theta %>% round(3), # T values
        "p value" = x$p.theta %>% round(3)) %>% data.frame() %>% # P values
    tibble::rownames_to_column("var") %>% rbind("", c("", "", glue('y = {y_var}'), "", "", ""),.)
  
}

make_ecm_tab <- function(X, y, country){
  # Make a nice table of the coefficients and p values from an ECM
  # @X: regressor matrix
  # @y: response variable
  # @country: Country to check
  
  # Get and round coeffiecients of ecm
  aTSA::ecm(y, X %>% data.matrix(), output = F)$coefficients %>% round(3) %>% 
    data.frame() %>% tibble::rownames_to_column("var") %>%
    mutate(country = country) %>% dplyr::select(var, country, everything()) %>% 
    rbind("", c("", "", glue('y = {y_var}'), "", "", ""),.)
}

for (i in seq(1:4)){
  # Loop through possible y vars
  y_var <- var_to_model[i]
  X_vars <- var_to_model[-i]
  
for (country in c("Australia", "Netherlands", "United States")){
  
  # Create X and y matrices based on the selected response variable and country
  c <- full %>% filter(`Country Name` == country) %>% na.omit()
  X <- c %>% dplyr::select(all_of(X_vars))
  y <- c %>% dplyr::select(y_var) 
  
  # Run DOLS
  DOLS_tab <- cointReg(X,y, n.lag = 3, method = 'D') %>% make_tab(country)
  
  # Run FMOLS
  FMOLS_tab <- cointReg(X,y, n.lag = 3, method = 'FM') %>% make_tab(country)
  
  # Run IMOLS
  IMOLS_tab <- cointReg(X,y, n.lag = 3, method = 'IM') %>% make_tab(country)
  
  # Run ECM
  ecm <- make_ecm_tab(X, c %>% .[[y_var]], country)
  
  if (country == "Australia") {
    DOLS_results <- DOLS_tab
    FMOLS_results <- FMOLS_tab
    IMOLS_results <- IMOLS_tab
    ECM_results <- ecm
    
  }else {
      DOLS_results = rbind(DOLS_results, DOLS_tab)
      FMOLS_results = rbind(FMOLS_results, FMOLS_tab)
      IMOLS_results = rbind(IMOLS_results, IMOLS_tab)
      ECM_results <- rbind(ECM_results, ecm)
    }

}
  if (i == 1) {
    DOLS_results_full <- DOLS_results
    FMOLS_results_full <- FMOLS_results
    IMOLS_results_full <- IMOLS_results
    ECM_results_full <- ECM_results
    
  }else {
    DOLS_results_full = rbind(DOLS_results_full, DOLS_results)
    FMOLS_results_full = rbind(FMOLS_results_full, FMOLS_results)
    IMOLS_results_full = rbind(IMOLS_results_full, IMOLS_results)
    ECM_results_full = rbind(ECM_results_full, ECM_results)
  }
  
  
}

# VECM Models
vecm_au <- VECM(full %>% filter(`Country Name` == 'Australia') %>% 
                  na.omit()%>% dplyr::select(all_of(vars)), 
                r = 1, lag = 2, estim = "ML") 
vecm_nl <- VECM(full %>% filter(`Country Name` == 'Netherlands') %>% 
                  na.omit()%>% dplyr::select(all_of(vars)), 
                r = 2, lag = 2, estim = "ML") 
vecm_us <- VECM(full %>% filter(`Country Name` == 'United States') %>% 
                  na.omit()%>% dplyr::select(all_of(vars)), 
                r = 2, lag = 2, estim = "ML")

# Store results

write_csv(DOLS_results_full, "Results/DOLS_results_full.csv")  
write_csv(FMOLS_results_full, "Results/FMOLS_results_full.csv")  
write_csv(IMOLS_results_full, "Results/IMOLS_results_full.csv")  
write_csv(ECM_results_full, "Results/ECM_results_full.csv")  

