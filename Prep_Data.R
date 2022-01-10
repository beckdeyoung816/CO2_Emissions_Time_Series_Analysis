##-------------------------------------------------------------##
##                    Prep_Data.R
##-------------------------------------------------------------##

# This script runs loads in, transforms, and cleans the data before use

library(tidyverse)
library(readxl)

# Helper function to clean data
clean <- function(df, var){
  minus_cols <- c("Country Name", "Country Code", 
                  "Indicator Name", "Indicator Code") # Columns to remove
  
  # Reshape data into long format
  df %>% gather(key = "Year", value = !!ensym(var), -all_of(minus_cols)) %>% 
    dplyr::select(-c("Country Code", "Indicator Name", "Indicator Code")) %>% # Remove columns
    type.convert(as.is = T) %>% # Convert numeric to numeric 
    arrange(`Country Name`, Year)
}

# Load in and clean data

CPI <- read_excel("Data/MVE_assignment_2021_dataset.xlsx", sheet= "Crop_production_index") %>% 
  clean('CPI')
elec <- read_excel("Data/MVE_assignment_2021_dataset.xlsx", sheet= "Electric_consumption")%>% 
  clean('Elec_Consumption')
co2 <- read_excel("Data/MVE_assignment_2021_dataset.xlsx", sheet= "CO2_emissions") %>% 
  clean("CO2_emissions")
c_vars <- read_excel("Data/MVE_assignment_2021_dataset.xlsx", sheet= "Climate variables") %>% 
  rename("Country Name" = cntry.name) %>% 
  arrange(`Country Name`, Year)

# Put all data into a single dataframe
full <- list(c_vars, co2, CPI, elec) %>% reduce(full_join)

original_vars = c("mean_tmp", "mean_pre", "mean_rad", 
                  "CO2_emissions", "CPI","Elec_Consumption")

var_names = c("Mean Temp", "Mean Precip", "Mean Rad", 
              "CO2 Emissions", "CPI","Electric Consumption")

full  <- full %>%
  group_by(`Country Name`) %>% 
  # log transform
  mutate(across(all_of(original_vars),
                .fns = ~ if_else(.x == 0 | is.na(.x), NA_real_, log10(.x)),
                .names = "{col}_log")) %>% 
  # Calculate first lag
  mutate(across(all_of(original_vars), 
                .fns = ~ lag(.x),
                .names = "{col}_lag_1"))  %>% 
  # Calculate second lag
  mutate(across(all_of(original_vars), 
                .fns = ~ lag(.x, k = 2),
                .names = "{col}_lag_2"))  %>% 
  # Calculate first difference
  mutate(across(all_of(original_vars), 
                .fns = ~ c(NA_real_, round(diff(.x), 2)),
                .names = "{col}_dif_1"))  %>% 
  # Calculate second difference
  mutate(across(all_of(original_vars), 
                .fns = ~ c(rep(NA_real_,2), round(diff(.x, differences = 2), 2)),
                .names = "{col}_dif_2")) %>% 
  # Calculate third difference
  mutate(across(all_of(original_vars), 
                .fns = ~ c(rep(NA_real_,3), round(diff(.x, differences = 3), 2)),
                .names = "{col}_dif_3")) %>% 
  ungroup()

rm(c_vars, co2, CPI, elec, clean) # Remove unnecessary files
