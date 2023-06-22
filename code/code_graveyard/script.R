################################################################################
####### EXPLORATORY ANALYSES OF NIGERIA LIVING STANDARDS SURVEY 2018-19 ########
################################################################################

# Install and load required packages:
rq_packages <- c("readr", "tidyverse", "ggplot2", "janitor")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# Read in data
cover <- read_csv("NGA_2018_LSS_v01_M_CSV/Household/secta_cover.csv")
food_consumption <- read_csv("NGA_2018_LSS_v01_M_CSV/Household/sect6b_food_cons.csv") 

# Data dictionary available at: https://microdata.worldbank.org/index.php/catalog/3827/data-dictionary/F48?file_name=secta_cover 

#-------------------------------------------------------------------------------

# SURVEY COVER:

View(cover)

# Check for any duplicates: 
cover %>% get_dupes() # No duplicates found


# See if there are any duplicates if we ignore household ID:
cover %>% get_dupes(-hhid) # No duplicates found

# How many households in survey:
nrow(cover) # 22587 households were included in this survey

# How many states are represented in the survey:
n_distinct(cover$state) # 37 states are represented in the survey

# Create a table to count number of households per state:
cover %>% count(state) %>% arrange(desc(n)) %>% knitr::kable()
# approximately 600 households per state (range 531 - 656) 

# Now view what proportion of households are in rural vs urban locations:
cover$sector <- ifelse(cover$sector == 1, "urban", 
                       ifelse(cover$sector == 2, "rural", NA))

cover %>% count(sector) %>% 
      mutate(percentage = round(n/sum(n) * 100, digits = 1)) %>% 
      knitr::kable()

# 68.8% of households are in rural locations and 31.2% are in urban locations

# What proportion of interviews were complete?:
cover %>% count(interview_result) %>% 
      mutate(percentage = round(n/sum(n) * 100, digits = 1)) %>% 
      knitr::kable()

# Interview result of 1 is defined as complete (97.9%)

#--------------------------------------------------------------------------------

# FOOD CONSUMPTION:
dim(food_consumption) # 2,454,987 rows and 26 columns
colnames(food_consumption)

# Add a column with name of food item based on "item_cd" for staple grains:
food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_cd == 10 ~ "Guinea corn/sorghum",
  item_cd == 11 ~ "Millet",
  item_cd == 13 ~ "Rice (local)",
  item_cd == 14 ~ "Rice (imported)",
  item_cd == 16 ~ "Maize flour",
  item_cd == 17 ~ "Yam flour",
  item_cd == 18 ~ "Cassava flour",
  item_cd == 19 ~ "Wheat flour",
  item_cd == 20 ~ "Maize (unshelled/on the cob)",
  item_cd == 22 ~ "Maize (shelled/off the cob)",
  item_cd == 23 ~ "Other grains and flour",
  item_cd == 25 ~ "Bread",
  item_cd == 26 ~ "Cake",
  item_cd == 27 ~ "Buns/pofpof/donuts",
  item_cd == 28 ~ "Biscuits",
  item_cd == 29 ~ "Meat pie/sausage roll",
  item_cd == 30 ~ "Cassava - roots",
  item_cd == 31 ~ "Yam - roots",
  item_cd == 32 ~ "Gari - white",
  item_cd == 33 ~ "Gari - yellow",
  item_cd == 34 ~ "Cocoyam",
  item_cd == 35 ~ "Plantains",
  item_cd == 36 ~ "Sweet potatoes",
  item_cd == 37 ~ "Potatoes",
  item_cd == 38 ~ "Other roots and tuber",
  item_cd == 40 ~ "Soya beans",
  item_cd == 41 ~ "Brown beans",
  item_cd == 42 ~ "White beans",
  TRUE ~ NA_character_
))

# Perhaps only need to focus on foods of interest in the command above

# The following column "s06bq01" identifies if the household consumed the food item in the previous 7 days:
food_consumption$consumed <- ifelse(food_consumption$s06bq01 == 1, "Yes", 
                                  ifelse(food_consumption$s06bq01 == 2, "No", NA))

#-------------------------------------------------------------------------------

# Create analysis data-set: 
analysis_df <- cover %>% select("hhid", "sector", "state", "zone")

# Add-in columns for staple grains - binary yes/no depending on if they have been consumed:
analysis_df <- analysis_df  %>%
                    left_join(food_consumption %>% 
                                filter(food_item == "Guinea corn/sorghum",
                                       consumed == "Yes") %>% 
                                       # Create a column in analysis_df for these individuals
                                        select("hhid", "consumed") %>%
                                        rename("Guinea corn/sorghum" = "consumed"),
                               by = "hhid")

# If the new column above contains NA, then replace with "No":
analysis_df$`Guinea corn/sorghum` <- ifelse(is.na(analysis_df$`Guinea corn/sorghum`), "No", 
                                           analysis_df$`Guinea corn/sorghum`)