##-------------------------------------------------------------##
##                    Stationarity_Test.R
##-------------------------------------------------------------##

# This script performs unit root tests and tidily stores the results

library(tidyverse)
library(tseries)
library(aTSA)
library(urca)
source("Prep_Data.R")


prepare_for_test <- function(var, country){
  # Only select desired variables and remove missing values
  # @var: Variable to check
  # @country: Country to check
  full %>% filter(`Country Name` == country) %>% .[[var]] %>% na.omit() # remove NAs
}

stat_test <- function(var, country){
  # See ADF test results on different lags (NOt just the default)
  # @var: Variable to check
  # @country: Country to check
  # return: df with the p value of each lag 0-6
  prepare_for_test(var, country) %>%  
    stationary.test(nlag = 6, output = F) %>% .[[1]] %>% data.frame() %>% 
    mutate(country = country,
           var = var,
           stationary = p.value < 0.05) %>% # Determine if stationary based on p values
    select(var, country, everything())
}

DF_test <- function(var, country, k = 1, aug = F){
  # Perform Dickey-Fuller test on a time series
  # @var: Variable to check
  # @country: Country to check
  # @k: which lags to check
  # @aug: Whether or not to perform augmented DF test
  # return: if non-augmented, only the p-value. If augmented, the pvalue and highest lag tests
  ts <- prepare_for_test(var, country)
  if (! aug) return(ts %>% tseries::adf.test(k = k) %>% .[['p.value']] %>% round(3))
  else{
    result <- ts %>% tseries::adf.test() 
    return(c(result$parameter, result$p.value %>% round(3)))
  }
}

PP_test <- function(var, country){
  # Perform Phillips-Peron test on a time series
  # @var: Variable to check
  # @country: Country to check
  # return: The pvalue and highest lag tests
  result <- prepare_for_test(var, country) %>%  
    tseries::pp.test() 
  return(c(result$parameter, result$p.value %>% round(3)))
}

KPSS_test <- function(var, country){
  # Perform Kwiatkowski-Phillips-Schmidt-Shin test on a time series
  # @var: Variable to check
  # @country: Country to check
  # return: The pvalue
  ts <- prepare_for_test(var, country) 
  
  level_p <- ts %>% tseries::kpss.test(null = "Level") %>% .[['p.value']] %>% round(3)
  trend_p <- ts %>% tseries::kpss.test(null = "Trend") %>% .[['p.value']] %>% round(3)
  
  return(c(level_p, trend_p))
}

za_test <- function(var, country){
  # See Zivot Andrews Test results
  # @var: Variable to check
  # @country: Country to check
  # return: df with the p value of each lag 0-6
  test <- prepare_for_test(var, country) %>%  
    ur.za(model = 'both', lag = 3) # Allow for breaks in both intercept and linear trend with up to 6 lagged values
  
  data.frame(ZA_stat = test@teststat %>% round(3),
             ZA_crit_val = test@cval[2], # the second value is the critical value for alpha = 5%
             stationary_ZA = abs(test@teststat) > abs(test@cval[2])) # Determine if test stat is below crit value
}

ers_test <- function(var, country){
  # See Elliot, Rothenberg and Stock test
  # @var: Variable to check
  # @country: Country to check
  # return: df with the p value of each lag 0-6
  test <- prepare_for_test(var, country) %>%  
    ur.ers(type = 'P-test', lag.max = 3) # Allow for breaks in both intercept and linear trend with up to 6 lagged values
  
  data.frame(ers_stat = test@teststat  %>% round(3),
             ers_crit_val = test@cval[2], # the second value is the critical value for alpha = 5%
             stationary_ers = abs(test@teststat) < abs(test@cval[2])) # Determine if test stat is above crit value
}

# Get all desired variables to check stationarity for (Up to 3rd difference)
vars <- full %>% select(-`Country Name`, -Year, - contains('log'), -contains('lag')) %>% names()
named_vars <- vars
names(named_vars) <- named_vars # Need to name variables for map_df reasons
n_vars <- length(vars)

for (country in c("Australia", "Netherlands", "United States")){
  
  DF <-  map_dbl(vars, DF_test, country) %>% # Compute non-augmented DF for each var
    data_frame(var = vars, 
               country = rep(country, n_vars), 
               DF_p = .) %>% 
    mutate(stationary_df = DF_p < 0.05) # Determine if stationary based on p values
  
  DF <- map_df(named_vars, DF_test, country, aug = T) %>% # Compute augmented DF for each var
    data.table::transpose() %>% 
    rename(lag_adf = V1, ADF_p = V2) %>% 
    cbind(DF, .)  %>% 
    mutate(stationary_adf = ADF_p < 0.05) # Determine if stationary based on p values
  
  PP <- map_df(named_vars, PP_test, country) %>% # Compute PP test for each var
    data.table::transpose() %>% 
    rename(lag_pp = V1, PP_p = V2) %>% 
    cbind(DF, .)  %>% 
    mutate(stationary_PP = PP_p < 0.05) # Determine if stationary based on p values
  
  KPSS <-  map_df(named_vars, KPSS_test, country) %>% # Compute KPSS test for each var
    data.table::transpose() %>% 
    rename(kpss_level_p = V1, kpss_trend_p = V2) %>% 
    mutate(stationary_KPSS_level = kpss_level_p < 0.05,
           stationary_KPSS_trend = kpss_trend_p < 0.05) %>% # Determine if stationary based on p values
    select(kpss_level_p, stationary_KPSS_level, kpss_trend_p, stationary_KPSS_trend) %>% # Reorder
    cbind(PP, .) 
    
  ERS <- map_df(vars, ers_test, country) %>% # Compute ERS test for each var
    cbind(KPSS, .)
  
  ZA <- map_df(vars, za_test, country) %>% # Compute ZA test for each var
    cbind(ERS, .)
  
  if (country == "Australia") stationarity_results <- ZA
  else stationarity_results <- rbind(stationarity_results, ZA)

  # ADF test for 6 lags
  stat_test_results <- map_df(vars, stat_test, country)
  if (country == "Australia") stationarity_results2 <- stat_test_results
  else stationarity_results2 <- rbind(stationarity_results2, stat_test_results)
  
  rm(KPSS, PP, ZA, stat_test_results, DF, ERS, country)
}

# Reorder the data
stationarity_results <- stationarity_results %>% arrange(var, country)
stationarity_results2 <- stationarity_results2 %>% arrange(var, country)

# Write as csvs
write_csv(stationarity_results, "Stationarity_tests_results.csv")
write_csv(stationarity_results2, "Stationarity_tests_results2.csv")
