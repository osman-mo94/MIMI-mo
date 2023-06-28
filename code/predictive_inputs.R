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

#-------------------------------------------------------------------------------

# CREATE DATAFRAME: 

# Select households who have completed the NLSS interview for inclusion in the
# data-frame: 

predictive_inputs <- cover %>% filter(interview_result == 1) %>% 
  select(hhid)

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

# RELIGION: 

predictive_inputs <- predictive_inputs %>% 
  left_join(roster %>% 
              group_by(hhid) %>% 
              summarise(proportion_christian = sum(s01q11 == 1, na.rm = T)/ n(),
                        proportion_muslim = sum(s01q11 == 2, na.rm = T)/ n(),
                        proportion_traditional = sum(s01q11 == 3, na.rm = T)/ n()), 
            by = "hhid")

# Ensure that variables are in correct format: 
class(predictive_inputs$proportion_christian)
class(predictive_inputs$proportion_muslim)
class(predictive_inputs$proportion_traditional)

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
  select(- "NA")

# Indicate non-ownership of items with a value of 0
assets_filtered <- assets_filtered %>% 
  replace(is.na(assets_filtered), 0)

# Ensure that variables are ordinal: 
assets_filtered <- assets_filtered %>%
  mutate(across(-hhid, ~factor(., levels = c(0, 1))))

# Join to predictive_inputs df
predictive_inputs <- predictive_inputs %>%
  left_join(assets_filtered, by = "hhid") 

# Remove objects that are not required further: 
rm(list = c("assets", "assets_filtered"))

#-------------------------------------------------------------------------------

# HOUSING: 

# Select relevant columns: 
housing <- housing %>% dplyr::select(hhid, s14q02, s14q03, 
                                     s14q09, s14q10, s14q11) %>% 
  rename(dwelling_type = s14q02, 
         dwelling_tenure = s14q03,
         material_walls = s14q09,
         material_roof = s14q10, 
         material_floor = s14q11)

housing <- housing %>% mutate(dwelling_type = case_when(
  dwelling_type == 1 ~ "bungalow", 
  dwelling_type == 2 ~ "semi-detached house",
  dwelling_type == 3 ~ "flat/apartment",
  dwelling_type == 4 ~ "compound house",
  dwelling_type == 5 ~ "huts/buildings (same compound)", 
  dwelling_type == 6 ~ "huts/buildings (diff compound)",
  dwelling_type == 7 ~ "tents",
  dwelling_type == 8 ~ "improvised home",
  dwelling_type == 9 ~ "office/shop",
  dwelling_type == 10 ~ "incomplete building",
  TRUE ~ NA_character_
)) %>% 
  mutate(dwelling_tenure = case_when(
    dwelling_tenure == 1 ~ "owned",
    dwelling_tenure == 2 ~ "free (authorised)", 
    dwelling_tenure == 3 ~ "free (unauthorised)", 
    dwelling_tenure == 4 ~ "rented"
  )) %>% 
  mutate(material_walls = case_when(
    material_walls == 1 ~ "mud", 
    material_walls == 2 ~ "stone",
    material_walls == 3 ~ "unburnt bricks", 
    material_walls == 4 ~ "burnt bricks", 
    material_walls == 5 ~ "cement/concrete", 
    material_walls == 6 ~ "wood or bamboo", 
    material_walls == 7 ~ "iron sheets", 
    material_walls == 8 ~ "cardboard", 
    TRUE ~ NA_character_
  )) %>% 
  mutate(material_roof = case_when(
    material_roof == 1 ~ "thatch", 
    material_roof == 2 ~ "corrugated iron", 
    material_roof == 3 ~ "clay tiles", 
    material_roof == 4 ~ "cement/concrete", 
    material_roof == 5 ~ "plastic sheet", 
    material_roof == 6 ~ "asbestos sheet", 
    material_roof == 7 ~ "mud", 
    material_roof == 8 ~ "span sheets", 
    material_roof == 9 ~ "step tiles", 
    material_roof == 11 ~ "zinc sheet",
    TRUE ~ NA_character_
  )) %>% 
  mutate(material_floor = case_when(
    material_floor == 1 ~ "sand/dirt/straw",
    material_floor == 2 ~ "smoothed mud", 
    material_floor == 3 ~ "cement/concrete",
    material_floor == 4 ~ "wood",
    material_floor == 5 ~ "tile", 
    material_floor == 6 ~ "terazo", 
    material_floor == 7 ~ "marble", 
    TRUE ~ NA_character_
  ))

# Ensure that the variables are categorical: 
housing <- housing %>% 
  mutate(across(-hhid, ~as.factor(.)))

# Join to predictive inputs: 
predictive_inputs <- predictive_inputs %>% 
  left_join(housing, by = "hhid")

# Remove housing object: 
rm(housing)

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

# EDUCATION:

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
education <- education %>% select(hhid, indiv, adult, s02q08) %>% 
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

# Remove eduction df (no longer required): 
rm(education)

# Check that variables are numeric: 
class(predictive_inputs$proportion_primary)
class(predictive_inputs$proportion_secondary)
class(predictive_inputs$proportion_higher)

#-------------------------------------------------------------------------------

# INCOME:

# (all income sources combined)
household_income <- income %>% 
  group_by(hhid) %>% 
  summarise(total_income = sum(s13q02, na.rm = T))

# Many households did not report their income, code these as NA:
household_income$total_income[household_income$total_income == 0] <- NA

# Join household income to predictive_inputs:
predictive_inputs <- predictive_inputs %>% 
  left_join(household_income, by = "hhid")

# Remove household_income df (not required further):
rm(list = c("household_income", "income"))

# See how many households are missing data on total income: 
length(which(is.na(predictive_inputs$total_income))) 

# Given that the overwhelming majority of households are missing data on income, 
# I will omit this as a predictive input.
predictive_inputs <- predictive_inputs %>% dplyr::select(-total_income)

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
  filter(adult == "Yes") %>% select(-adult)

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

# ??Participatory wealth ranking

# ??Subjective measures

#-------------------------------------------------------------------------------

# ENSURE THAT ALL VARIABLES ARE CORRECT TYPE (CATEGORICAL, CONTINOUS ETC.)

# Remove objects no longer required: 
rm(list = c("cover", "roster"))
