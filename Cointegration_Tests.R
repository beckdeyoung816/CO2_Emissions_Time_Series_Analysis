library(tidyverse)
library(tseries)
library(urca)
library(aTSA)
library(xtable)
source("Prep_Data.R")

prepare_for_co_test <- function(vars, country){
  # Only select desired variables and remove missing values
  # @vars: Variables to check
  # @country: Country to check
  full %>% filter(`Country Name` == country) %>% 
    dplyr::select(all_of(vars)) %>% na.omit() # remove NAs
}

JC_test <- function(vars, country){
  # Perform Johansen's Cointegration Test for both max eigenvalues and trace
  # @vars: Variables to check
  # @country: Country to check
  # return: Dataframe with the statistics for each test for r = 0 until r = m
  df <- prepare_for_co_test(vars, country) 
  max_eigs <- df %>% urca::ca.jo(spec = 'longrun') # Long run max eigenvalues test
  trace <- df %>% ca.jo(type = 'trace', spec = 'longrun') # Long run trace test
  
  # Create dataframes with the test statistics and critical values
  max_eigs_df <- max_eigs@cval %>% data.frame() %>% 
    tibble::rownames_to_column("r") %>% 
    mutate(JC_stat = max_eigs@teststat %>% round(3))
  
  trace_df <- trace@cval %>% data.frame(row.names = NULL) %>% 
    mutate(JC_trace_stat = trace@teststat %>% round(3))
  
  names(trace_df) <- c("JC_trace_10pt_cv", "JC_trace_5pt_cv", "JC_trace_1pt_cv", "JC_trace_stat")
  names(max_eigs_df) <- c("r", "JC_ME_10pt_cv", "JC_ME_5pt_cv", "JC_ME_1pt_cv", "JC_ME_stat")
  
  # Return results
  cbind(max_eigs_df, trace_df) %>% mutate(vars = list(vars), country = country) %>% 
    dplyr::select(vars, country, everything()) %>% rbind("")
}

EG_test <- function(y_var, vars, country){
  # Perform Engle-Granter test on a time series
  # @y_var: Response variable
  # @vars: Variables to check
  # @country: Country to check
  # return: The test statistics and p values of each type of test
  ts <- prepare_for_co_test(vars, country)
  y = ts %>% .[[y_var]] # Need y as a vector
  X = ts %>% dplyr::select(-y_var) %>% as.matrix.data.frame() # Get X into desired form
  coint.test(y, X, output = F) %>% round(3) %>% data.frame() %>% 
    tibble::rownames_to_column("Type") %>% filter(Type == "type 1") %>%  
    mutate(y_var = y_var, X = list(setdiff(vars, y_var)), country = country) %>% 
    dplyr::select(y_var,country, everything(),-Type, -X, -lag)
}

PO_test <- function(vars, country){
  # Perform Phillips-Ouliaris Cointegration test on a time series
  # @vars: Variables to check
  # @country: Country to check
  # return: The pvalue
  prepare_for_co_test(vars, country) %>%  
    tseries::po.test() %>% .[['p.value']] %>% round(3)
}

LK_test <- function(vars, country){
  # Perform Phillips-Ouliaris Cointegration test on a time series
  # @vars: Variables to check
  # @country: Country to check
  # return: The pvalue
  result <- prepare_for_co_test(vars, country) %>%  
    cajolst()
  
  result_df <- result@cval %>% data.frame() %>% 
    mutate(LK_stat = result@teststat %>% round(3)) %>% rbind("") 
  
  names(result_df) <- c("LK_10pt_cv", "LK_5pt_cv", "LK_1pt_cv", "LK_stat") 
  result_df
}

vars_to_model <- c("mean_tmp", "mean_pre", "CO2_emissions", "CPI")
#log_vars_to_model <- paste0(vars_to_model, "_log")

for (country in c("Australia", "Netherlands", "United States")){
  
  eg <- map_df(vars_to_model, EG_test, vars_to_model, country)
  po <- PO_test(vars_to_model, country) %>% data.frame(country = country,
                                                       PO_p = .)
  
  lk <- LK_test(vars_to_model, country) 
  jc <- JC_test(vars_to_model, country) %>% 
    cbind(lk)
  
  if (country == "Australia") {
    jc_results <- jc
    eg_results <- eg
    po_results <- po
  }
  else {
    jc_results <- rbind(jc_results, jc)
    eg_results <- rbind(eg_results, eg)
    po_results <- rbind(po_results, po)
  }
}

write_csv(jc_results, 'Results/Johanes_results.csv')
write_csv(eg_results, "Results/EG_results.csv")
write_csv(po_results, "Results/po_results.csv")


