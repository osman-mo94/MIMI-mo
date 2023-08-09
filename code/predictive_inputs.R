################################################################################
################### SCRIPT FOR CREATING PREDCITIVE INPUTS ######################
################################################################################

# Install and load required packages:
rq_packages <- c("readr", "tidyverse", "ggplot2", "janitor", "knitr",
                 "wesanderson", "ghibli", "ggthemes", "table1", "kableExtra",
                 "lubridate")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# DATA:

# Read in required modules of NLSS: 
cover <- read_csv("NLSS_data/Household/secta_cover.csv")
roster <- read_csv("NLSS_data/Household/sect1_roster.csv")
education <- read_csv("NLSS_data/Household/sect2_education.csv")
income <- read_csv("NLSS_data/Household/sect13_income.csv")
consumption <- read_csv("NLSS_data/Household/totcons.csv")
assets <- read_csv("NLSS_data/Household/sect10_assets.csv")
housing <- read_csv("NLSS_data/Household/sect14_housing.csv")
labour <- read_csv("NLSS_data/Household/sect4a1_labour.csv")
agriculture <- read_csv("NLSS_data/Household/sect18_agriculture.csv")

#-------------------------------------------------------------------------------

# CREATE DATAFRAME: 

# Select households who have completed the NLSS interview for inclusion in the
# data-frame: 

predictive_inputs <- cover %>% filter(interview_result == 1) %>% 
  dplyr::select(hhid)

# Further filter this df to include only those who have nutrient intake data: 
nutrient_data <- read_csv("nutrient_data/nga18_ai_basecase_MO_v1.csv")

nutrient_data <- nutrient_data %>% drop_na()

predictive_inputs <- predictive_inputs %>% 
  inner_join(nutrient_data, by = "hhid") %>% 
  dplyr::select(hhid)

rm(nutrient_data)

#-------------------------------------------------------------------------------

# HOUSEHOLD RESIDENTS AND GENDER:

# Join a column on number of residents in each houeshold, and proportion of that
# household that is male: 

predictive_inputs <- predictive_inputs %>% 
  left_join(roster %>% 
              group_by(hhid) %>% 
              summarise(n_residents = n()), 
            by = "hhid")  %>% 
  left_join(roster %>% 
              group_by(hhid) %>%
              summarise(proportion_male = sum(s01q02 == 1, na.rm = T)/n()),
            by = "hhid")  

# Ensure that variables are in correct format: 
class(predictive_inputs$n_residents)
class(predictive_inputs$proportion_male)

#-------------------------------------------------------------------------------

# I have removed this section for creating variables based on religion:

# RELIGION: 

# predictive_inputs <- predictive_inputs %>% 
#   left_join(roster %>% 
#               group_by(hhid) %>% 
#               summarise(proportion_christian = sum(s01q11 == 1, na.rm = T)/ n(),
#                         proportion_muslim = sum(s01q11 == 2, na.rm = T)/ n(),
#                         proportion_traditional = sum(s01q11 == 3, na.rm = T)/ n()), 
#             by = "hhid")
# 
# # Ensure that variables are in correct format: 
# class(predictive_inputs$proportion_christian)
# class(predictive_inputs$proportion_muslim)
# class(predictive_inputs$proportion_traditional)

#-------------------------------------------------------------------------------

# GEOGRAPHY: 

predictive_inputs <- predictive_inputs %>% 
  left_join(cover %>% dplyr::select(hhid, sector) %>% 
              rename(geography = sector) %>% 
              mutate(geography = dplyr::case_when(
                geography == 1 ~ "urban",
                geography == 2 ~ "rural", 
                TRUE ~ NA_character_
              )),
            by = "hhid")

# Ensure that variables are in the correct format: 
class(predictive_inputs$geography)
predictive_inputs$geography <- as.factor(predictive_inputs$geography)

#-------------------------------------------------------------------------------

# ASSET BASED MEASURES: 

# List assets based on asset codes
assets <- assets  %>% mutate(asset = dplyr::case_when(
  asset_cd == 301 ~ "sofa",
  asset_cd == 302 ~ "chairs",
  asset_cd == 303 ~ "table", 
  asset_cd == 304 ~ "mattress",
  asset_cd == 305 ~ "bed", 
  asset_cd == 306 ~ "mat",
  asset_cd == 307 ~ "sewing_machine",
  asset_cd == 308 ~ "gas_cooker",
  asset_cd == 309 ~ "electric_stove", 
  asset_cd == 310 ~ "table_gas_stove", 
  asset_cd == 311 ~ "kerosene_stove",
  asset_cd == 312 ~ "fridge", 
  asset_cd == 313 ~ "freezer", 
  asset_cd == 314 ~ "air_con", 
  asset_cd == 315 ~ "wash_machine", 
  asset_cd == 316 ~ "electric_clothes_dryer",
  asset_cd == 317 ~ "bicycle",
  asset_cd == 318 ~ "motorbike",
  asset_cd == 319 ~ "cars_vehicles", 
  asset_cd == 320 ~ "generator", 
  asset_cd == 321 ~ "fan", 
  asset_cd == 322 ~ "radio", 
  asset_cd == 323 ~ "cassette_recorder", 
  asset_cd == 324 ~ "hi_fi",
  asset_cd == 325 ~ "microwave", 
  asset_cd == 326 ~ "iron",
  asset_cd == 327 ~ "tv",
  asset_cd == 328 ~ "computer",
  asset_cd == 329 ~ "dvd_player", 
  asset_cd == 330 ~ "satellite_dish", 
  asset_cd == 331 ~ "musical_instrument", 
  asset_cd == 333 ~ "inverter", 
  asset_cd == 3021 ~ "plastic_chairs", 
  asset_cd == 3321 ~ "smart_phones",
  asset_cd == 3322 ~ "reg_mobile_phone",
  TRUE ~ NA_character_
))

# Select only required columns from assets df:
assets <- assets %>% dplyr::select(hhid, asset, s10q01)

# Create variables to indicate ownership of these assets in predictive_inputs: 
assets_filtered <- assets %>%
  filter(s10q01 == 1) %>%
  dplyr::select(hhid, asset, s10q01) %>%
  pivot_wider(names_from = asset, values_from = s10q01) %>% 
  dplyr::select(- "NA")

# Indicate non-ownership of items with a value of 0
assets_filtered <- assets_filtered %>% 
  replace(is.na(assets_filtered), 0)

# Ensure that variables are ordinal: 
assets_filtered <- assets_filtered %>%
  mutate(across(-hhid, ~factor(., levels = c(0, 1))))


# Filter for selected assets:
assets_filtered <- assets_filtered %>% 
  dplyr::select(hhid, radio, tv, smart_phones, reg_mobile_phone, fridge, cars_vehicles)

# Combine smart_phones and reg_mobile_phone into one variable:
assets_filtered <- assets_filtered %>% 
  mutate(mobile_phone = dplyr::case_when(
    smart_phones == 1 ~ 1,
    reg_mobile_phone == 1 ~ 1,
    TRUE ~ 0)) %>% 
  dplyr::select(-smart_phones, -reg_mobile_phone)

# Join to predictive_inputs df
predictive_inputs <- predictive_inputs %>%
  left_join(assets_filtered, by = "hhid") 

# Remove objects that are not required further: 
rm(list = c("assets", "assets_filtered"))


#-------------------------------------------------------------------------------

# HOUSING: 

# Will not use dwelling_type for now, but may use in a future iteration.

# Select relevant columns: 
housing <- housing %>% dplyr::select(hhid, s14q03, s14q11, s14q12, 
                                     # s14q02 (dwelling_type),
                                     s14q19, s14q27, s14q40) %>% 
  rename(dwelling_tenure = s14q03,
         # dwelling_type = s14q02,
         material_floor = s14q11,
         n_rooms = s14q12,
         electricity = s14q19, 
         water_source = s14q27, 
         toilet_facility = s14q40)

housing <- housing %>% 
#   mutate(dwelling_type = case_when(
#   dwelling_type == 1 ~ "bungalow", 
#   dwelling_type == 2 ~ "semi-detached house",
#   dwelling_type == 3 ~ "flat/apartment",
#   dwelling_type == 4 ~ "compound house",
#   dwelling_type == 5 ~ "huts/buildings (same compound)", 
#   dwelling_type == 6 ~ "huts/buildings (diff compound)",
#   dwelling_type == 7 ~ "tents",
#   dwelling_type == 8 ~ "improvised home",
#   dwelling_type == 9 ~ "office/shop",
#   dwelling_type == 10 ~ "incomplete building",
#   TRUE ~ NA_character_
# )) %>% 
  mutate(dwelling_free = case_when(
    dwelling_tenure == 2 ~ 1,
    dwelling_tenure == 3 ~ 1,
    TRUE ~ 0 
  )) %>% 
  mutate(dwelling_rented = case_when(
    dwelling_tenure == 4 ~ 1,
    TRUE ~ 0 
  )) %>%
  mutate(dwelling_owned = case_when(
    dwelling_tenure == 1 ~ 1,
    TRUE ~ 0 
  )) %>%
  mutate(material_floor = case_when(
    material_floor %in% c(1:2) ~ "0", 
    material_floor %in% c(3:7) ~ "1",
    TRUE ~ NA_character_
  )) %>% 
  mutate(electricity = ifelse(electricity == 2, 0, electricity)) %>% 
  mutate(water_source = case_when(
    water_source %in% c(1, 2, 3, 4, 5, 6, 8, 10, 11, 12, 14, 16) ~ "1", 
    water_source %in% c(7, 9, 13, 17) ~ "0", 
    TRUE ~ NA_character_
  )) %>% 
  mutate(open_defecaetion = case_when(
    toilet_facility == 12 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(toilet_unimproved = case_when(
    toilet_facility %in% c(4, 5, 8, 10, 11, 14) ~ 1, 
    TRUE ~ 0
  )) %>%
  mutate(toilet_improved = case_when(
    toilet_facility %in% c(1, 2, 3, 6, 7, 9) ~ 1, 
    TRUE ~ 0
  ))

  # Select only required columns from "housing": 
housing <- housing %>%
  dplyr::select(hhid, material_floor, n_rooms, electricity, water_source, 
                dwelling_free, dwelling_rented, dwelling_owned, 
                open_defecaetion, toilet_unimproved, toilet_improved)

# Ensure all variables in housing are numeric:
housing <- housing %>% mutate(across(-hhid, ~as.numeric(.)))

# Join to predictive inputs: 
predictive_inputs <- predictive_inputs %>% 
  left_join(housing, by = "hhid")

# Remove housing df: 
rm(housing)

# Calculate persons per sleeping room: 
predictive_inputs$n_per_room <- predictive_inputs$n_residents / predictive_inputs$n_rooms

#-------------------------------------------------------------------------------

# HOUSEHOLD CONSUMPTION AND EXPENDITURE: 

# Note that this is an adjusted value (spatially and temporally), per capita

predictive_inputs <- predictive_inputs  %>% 
  left_join(consumption %>% 
              dplyr::select(hhid, totcons_adj),
            by = "hhid")  %>% 
  rename(total_consumption = totcons_adj)

# Ensure that variable is numeric: 
class(predictive_inputs$total_consumption)

rm(consumption)

# Create a new variable for quintile of total consumption (1lowest, 5 highest): 
predictive_inputs <- predictive_inputs %>% 
  mutate(consumption_quintile = ntile(total_consumption, 5))

#-------------------------------------------------------------------------------

# OWNERSHIP OR ACCESS TO AGRICULTURAL LAND

# Select required columns from agriculture df and rename:
agriculture <- agriculture %>% dplyr::select(hhid, s18q01)
names(agriculture) <- c("hhid", "agricultural_land")

# Indicate no access to agricultural land with 0, and 1 for access: 
agriculture$agricultural_land[agriculture$agricultural_land == 2] <- 0 
agriculture$agricultural_land[is.na(agriculture$agricultural_land)] <- 0

# Join data to predictive_inputs df: 
predictive_inputs <- predictive_inputs %>% 
  left_join(agriculture, by = "hhid")

# Remove agriculture df: 
rm(agriculture)

#-------------------------------------------------------------------------------

# EDUCATION OF ADULTS:

# In this section, I will determine educational attainments of adult in the 
# household: 

# Need to first determine year of interview: 
cover$InterviewStart <- as.POSIXlt(cover$InterviewStart, 
                                   format = "%m/%d/%Y %H:%M:%OS")

cover$year_interview <- as.numeric(format(cover$InterviewStart, "%Y"))

# Join year of interview to roster data: 
roster <- roster %>% left_join(cover %>% dplyr::select(hhid, year_interview),
                               by = "hhid")

# Use year of interview and year of birth to determine which household members 
# were adults at the time of interview (over 18 years old):

roster$adult <- ifelse((roster$year_interview - roster$s01q06b) >= 18, 
                       "Yes", "No")

# Join this data to education df: 
education <- education %>% 
  left_join(roster %>% dplyr::select(hhid, indiv, adult), 
            by = c("hhid", "indiv"))

# Select columns of interest from education dataframe: 
education <- education %>% dplyr::select(hhid, indiv, adult, s02q08) %>% 
  rename(education_attainment = s02q08)

# If individuals answered "other", code as missing:
education$education_attainment[education$education_attainment == 13] <- NA

# Create variable to indicate if completed primary education: 
education$primary <- ifelse(education$education_attainment >= 2, 1, 0)

# Create variable to indicate if completed secondary education: 
education$secondary <- ifelse(education$education_attainment >= 6, 1, 0)

# Create variable to indicate if completed higher education: 
education$higher <- ifelse(education$education_attainment >= 7, 1, 0)

# Get proportion of adults in each household that have completed each stage of 
# education: 
education <- education %>% 
  filter(adult == "Yes") %>% 
  group_by(hhid) %>% 
  summarise(proportion_primary = sum(primary == 1, na.rm = T) / n(),
            proportion_secondary = sum(secondary == 1, na.rm = T) / n(),
            proportion_higher = sum(higher == 1, na.rm = T ) / n())

# Join these variables onto predictive inputs df: 
predictive_inputs <- predictive_inputs %>% left_join(education, by = "hhid")

# Check that variables are numeric: 
class(predictive_inputs$proportion_primary)
class(predictive_inputs$proportion_secondary)
class(predictive_inputs$proportion_higher)

# Remove education df: 
rm(education)

#-------------------------------------------------------------------------------

# Income section below not used.
# # INCOME:

# # (all income sources combined)
# household_income <- income %>% 
#   group_by(hhid) %>% 
#   summarise(total_income = sum(s13q02, na.rm = T))

# # Many households did not report their income, code these as NA:
# household_income$total_income[household_income$total_income == 0] <- NA

# # Join household income to predictive_inputs:
# predictive_inputs <- predictive_inputs %>% 
#   left_join(household_income, by = "hhid")

# # Remove household_income df (not required further):
# rm(list = c("household_income", "income"))

# # See how many households are missing data on total income: 
# length(which(is.na(predictive_inputs$total_income))) 

# # Given that the overwhelming majority of households are missing data on income, 
# # I will omit this as a predictive input.
# predictive_inputs <- predictive_inputs %>% dplyr::select(-total_income)

#-------------------------------------------------------------------------------

# OCCUPATION

# Note that 18 is the age of majority (adulthood) as specified by the Nigerian 
# constitution.

# In this section, I will create variables to indicate type of employment of 
# individuals in each household: 
labour <- labour %>% dplyr::select(hhid, indiv, s04aq04, s04aq06, s04aq09,
                                   s04aq11) %>% 
  rename(wage_salary = s04aq04,
         own_agriculture = s04aq06, 
         own_NFE = s04aq09, 
         trainee_apprentice = s04aq11)  %>% 
  mutate(across(c("wage_salary", "own_agriculture", "own_NFE"), 
                ~ifelse(. != 1, NA, .))) %>% 
  mutate(trainee_apprentice = ifelse(trainee_apprentice == 1 | 
                                     trainee_apprentice == 2, 1, NA))

labour$wage_salary[is.na(labour$wage_salary)] <- 0
labour$own_agriculture[is.na(labour$own_agriculture)] <- 0
labour$own_NFE[is.na(labour$own_NFE)] <- 0
labour$trainee_apprentice[is.na(labour$trainee_apprentice)] <- 0

# Filter df to include only adults (over 18):
labour <- labour %>% 
  left_join(roster %>% dplyr::select(hhid, indiv, adult),
            by = c("hhid", "indiv")) %>% 
  filter(adult == "Yes") %>% dplyr::select(-adult)

# Now create variables to indicate proportion of adults in each household who 
# are working in each type of employment: 
labour <- labour %>% 
  group_by(hhid) %>% 
  summarise(proportion_wage_salary = sum(wage_salary == 1, na.rm = T) / n(),
            proportion_own_agriculture = sum(own_agriculture == 1, na.rm = T) / n(),
            proportion_own_NFE = sum(own_NFE == 1, na.rm = T) / n(),
            proportion_trainee_apprentice = sum(trainee_apprentice == 1, na.rm = T) / n())

# Join these columns to predictive input df: 
predictive_inputs <- predictive_inputs %>% left_join(labour, by = "hhid")

# Check that these new variables are of class "numeric": 
class(predictive_inputs$proportion_wage_salary)
class(predictive_inputs$proportion_own_agriculture)
class(predictive_inputs$proportion_own_NFE)
class(predictive_inputs$proportion_trainee_apprentice)

# Remove labour df (no longer required): 
rm(labour)

#-------------------------------------------------------------------------------

# Remove objects no longer required: 
rm(list = c("cover", "roster"))

#-------------------------------------------------------------------------------

# FINALISE DATAFRAME

# Remove variables that are no longer required: 
predictive_inputs <- predictive_inputs %>% dplyr::select(-c("n_residents", 
                                                            "n_rooms"))

# Re-order variables: 
new_order <- c("hhid", "geography", "total_consumption", "consumption_quintile",
               "radio", "tv", "fridge", "cars_vehicles", "mobile_phone",
               "dwelling_free", "dwelling_rented", "dwelling_owned",
               "material_floor", "electricity", "water_source",
               "open_defecaetion", "toilet_unimproved",
               "toilet_improved", "n_per_room", "agricultural_land",
               "proportion_male", "proportion_primary", "proportion_secondary",
               "proportion_higher", "proportion_wage_salary",
               "proportion_own_agriculture", "proportion_own_NFE",
               "proportion_trainee_apprentice")

predictive_inputs <- predictive_inputs[,new_order]

rm(list = c("new_order", "income"))

################################################################################
############################## END OF SCRIPT ###################################
################################################################################
