################################################################################
################### SCRIPT FOR EXTRACTING TARGET VARIABLES #####################
################################################################################

# Install and load required packages:
rq_packages <- c("readr", "tidyverse", "ggplot2", "janitor", "knitr",
                 "wesanderson", "ghibli", "ggthemes")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# Read in data
cover <- read_csv("NLSS_data/Household/secta_cover.csv")
food_consumption <- read_csv("NLSS_data/Household/sect6b_food_cons.csv") 

# Data dictionary available at: https://microdata.worldbank.org/index.php/catalog/3827/data-dictionary/F48?file_name=secta_cover 

#-------------------------------------------------------------------------------

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

# Quantify how much of the food items were consumed

# Firstly rename columns with more useful names: 
food_consumption <- food_consumption %>% 
  rename(quantity_consumed = s06bq02a, 
         quantity_unit = s06bq02b,
         unit_other = s06bq02b_os,
         quantity_size = s06bq02c,
         conversion_factor = s06bq02_cvn)

#-------------------------------------------------------------------------------

# Create df for target variables, for each household ID: 
target_variables <- cover %>% dplyr::select("hhid")

# Add-in columns for target variable (staple grains) - binary yes/no depending on if they have been consumed:
target_variables <- target_variables %>%
  # Rice (local):
  left_join(food_consumption %>% 
              filter(food_item == "Rice (local)",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed") %>%
              rename("rice_local" = "consumed"),
            by = "hhid") %>% 
  # Rice (imported):
  left_join(food_consumption %>% 
              filter(food_item == "Rice (imported)",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed") %>%
              rename("rice_imported" = "consumed"),
            by = "hhid") %>% 
  # Maize flour: 
  left_join(food_consumption %>% 
              filter(food_item == "Maize flour",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed") %>%
              rename("maize_flour" = "consumed"),
            by = "hhid") %>% 
  # Wheat flour: 
  left_join(food_consumption %>% 
              filter(food_item == "Wheat flour",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed") %>%
              rename("wheat_flour" = "consumed"),
            by = "hhid") %>% 
  # Bread: 
  left_join(food_consumption %>% 
              filter(food_item == "Bread",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed") %>% 
              rename("bread" = "consumed"), 
            by = "hhid") %>% 
  # Cake: 
  left_join(food_consumption %>% 
              filter(food_item == "Cake",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed") %>% 
              rename("cake" = "consumed"), 
            by = "hhid") %>% 
  # Buns/Pofpof/Donuts: 
  left_join(food_consumption %>% 
              filter(food_item == "Buns/Pofpof/Donuts",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed") %>% 
              rename("buns_pofpof_donuts" = "consumed"), 
            by = "hhid") %>% 
  # Biscuits: 
  left_join(food_consumption %>% 
              filter(food_item == "Biscuits",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed") %>% 
              rename("biscuits" = "consumed"), 
            by = "hhid") 


# If the new columns above contains NA, then replace with "No":
target_variables$rice_local <- ifelse(is.na(target_variables$rice_local), "No", 
                                 target_variables$rice_local)

target_variables$rice_imported <- ifelse(is.na(target_variables$rice_imported), "No", 
                                    target_variables$rice_imported)

target_variables$maize_flour <- ifelse(is.na(target_variables$maize_flour), "No", 
                                  target_variables$maize_flour)

target_variables$wheat_flour <- ifelse(is.na(target_variables$wheat_flour), "No", 
                                  target_variables$wheat_flour)

target_variables$bread <- ifelse(is.na(target_variables$bread), "No", 
                            target_variables$bread)

target_variables$cake <- ifelse(is.na(target_variables$cake), "No", 
                           target_variables$cake)

target_variables$buns_pofpof_donuts <- ifelse(is.na(target_variables$buns_pofpof_donuts), "No", 
                                         target_variables$buns_pofpof_donuts)

target_variables$biscuits <- ifelse(is.na(target_variables$biscuits), "No", 
                               target_variables$biscuits)

# Assuming that bread, cake, buns/pofpof/donuts and biscuits all contain wheat flour, 
# consolidate these into 1 wheat_flour column, that will indicate "Yes" if any of
# the food items are consumed:

target_variables$wheat_flour <- ifelse(target_variables$bread == "Yes" | 
                                    target_variables$cake == "Yes" | 
                                    target_variables$buns_pofpof_donuts == "Yes" | 
                                    target_variables$biscuits == "Yes", "Yes", 
                                  target_variables$wheat_flour)

# Now remove the other columns to include only the 4 staple grains: local rice, 
# imported rice, maize flour, wheat flour.

target_variables <- target_variables %>% 
  dplyr::select(-bread, -cake, -buns_pofpof_donuts, -biscuits)

#-------------------------------------------------------------------------------

# Summary statistics for intake of staple grains:
View(target_variables)

riceloc_consumption <- target_variables %>% count(rice_local) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1)) 

riceimp_consumption <- target_variables %>% count(rice_imported) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1)) 

maizef_consumption <- target_variables %>% count(maize_flour) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1)) 

wheatf_consumption <- target_variables %>% count(wheat_flour) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1)) 


# Visualise percentage of households consuming each food in a barplot:

grain <- c("Rice (local)", "Rice (imported)", "Maize flour", "Wheat flour")

percentage_consumed <- c(riceloc_consumption[2,3], riceimp_consumption[2,3], 
                         maizef_consumption[2,3], wheatf_consumption[2,3])

percentage_consumed <- as.double(percentage_consumed)

grain_consumption <- data.frame(grain, percentage_consumed) %>% 
  arrange(desc(percentage_consumed))

colour_palette <- ghibli_palette("PonyoMedium", n = 4)

ggplot(grain_consumption, aes(x = reorder(grain, -percentage_consumed), 
                              y = percentage_consumed, fill = grain)) + 
  geom_col(show.legend = F) + labs(x = "Grain", y = "% of Households") +
  scale_fill_manual(values = colour_palette) + theme_pander()

# Remove objects that are not required further: 
rm(list = c("cover", "food_consumption", "grain_consumption", "maizef_consumption", 
            "riceimp_consumption", "riceloc_consumption", "wheatf_consumption",
            "colour_palette", "grain", "percentage_consumed"))
