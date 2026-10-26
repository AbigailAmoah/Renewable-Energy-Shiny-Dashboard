setwd("C:/Users/abiga/Desktop/MIAMI/1SEM2/STA 504/PROJECT")
library(tidyverse)
library(janitor)


#loading data

data <- read.csv("WorldSustainabilityDataset.csv")


#cleaning data
data1 <- data %>%
  janitor::clean_names() %>%   # fixes column names (very useful)
  mutate(across(where(is.character), str_trim))  # remove spaces
colnames(data1)



#variable selection and column renaming

project_data <- data1 %>%
  clean_names() %>%
  select(
    country_name,
    year,
    continent,
    renewable_energy_consumption_of_total_final_energy_consumption_eg_fec_rnew_zs,
    renewable_electricity_output_of_total_electricity_output_eg_elc_rnew_zs,
    annual_production_based_emissions_of_carbon_dioxide_co2_measured_in_million_tonnes,
    gdp_per_capita_current_us_ny_gdp_pcap_cd,
    access_to_electricity_of_population_eg_elc_accs_zs
  ) %>%
  rename(
    country = country_name,
    renewable_energy = renewable_energy_consumption_of_total_final_energy_consumption_eg_fec_rnew_zs,
    renewable_electricity = renewable_electricity_output_of_total_electricity_output_eg_elc_rnew_zs,
    co2 = annual_production_based_emissions_of_carbon_dioxide_co2_measured_in_million_tonnes,
    gdp_per_capita = gdp_per_capita_current_us_ny_gdp_pcap_cd,
    electricity_access = access_to_electricity_of_population_eg_elc_accs_zs
  )


#save data

save(project_data, file = "project_data.RData")