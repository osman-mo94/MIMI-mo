################################################################################
################### SCRIPT FOR EXTRACTING TARGET VARIABLES #####################
################################################################################

# Install and load required packages:
rq_packages <- c("readr", "tidyverse", "ggplot2", "janitor", "knitr",
                 "wesanderson", "ghibli", "ggthemes", "table1")

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

# Extract data on staple grains from the food consumption module of NLSS: 

dim(food_consumption) # 2,454,987 rows and 26 columns
colnames(food_consumption)

# Add a column with name of food item based on "item_cd" for staple grains:
food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_cd == 13 ~ "Rice (local)",
  item_cd == 14 ~ "Rice (imported)",
  item_cd == 16 ~ "Maize flour",
  item_cd == 19 ~ "Wheat flour",
  item_cd == 25 ~ "Bread",
  item_cd == 26 ~ "Cake",
  item_cd == 27 ~ "Buns/pofpof/donuts",
  item_cd == 28 ~ "Biscuits",
  TRUE ~ NA_character_
))

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
         conversion_factor = s06bq02_cvn,
         quantity_purchased = s06bq03)

# Calculate the quantity consumed in standard units (kg/L)
food_consumption$quantity_kg_L <- food_consumption$quantity_consumed * food_consumption$conversion_factor

# Create a column if at least some of the food item was purchased (i.e. quantity > 0, and not NA):
food_consumption$food_purchased <- ifelse(food_consumption$quantity_purchased > 0, 
                                          "Yes", "No")

# If NA in food_purchased column, then replace with "No":
food_consumption$food_purchased <- ifelse(is.na(food_consumption$food_purchased), 
                                          "No", food_consumption$food_purchased)


#-------------------------------------------------------------------------------

# Create df for target variables, for each household ID: 
target_variables <- cover %>% dplyr::select("hhid")

# Add-in columns for target variable (staple grains) - binary yes/no depending on if they have been consumed and purchased:
target_variables <- target_variables %>%
  # Rice (local):
  left_join(food_consumption %>% 
              filter(food_item == "Rice (local)",
                     consumed == "Yes",
                     food_purchased == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed", "quantity_kg_L") %>%
              rename("rice_local" = "consumed",
                     "rice_local_kg" = "quantity_kg_L"),
            by = "hhid") %>% 
  # Rice (imported):
  left_join(food_consumption %>% 
              filter(food_item == "Rice (imported)",
                     consumed == "Yes",
                     food_purchased == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed", "quantity_kg_L") %>%
              rename("rice_imported" = "consumed",
                     "rice_imported_kg" = "quantity_kg_L"),
            by = "hhid") %>% 
  # Maize flour: 
  left_join(food_consumption %>% 
              filter(food_item == "Maize flour",
                     consumed == "Yes",
                     food_purchased == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed", "quantity_kg_L") %>%
              rename("maize_flour" = "consumed",
                     "maize_flour_kg" = "quantity_kg_L"),
            by = "hhid") %>% 
  # Wheat flour: 
  left_join(food_consumption %>% 
              filter(food_item == "Wheat flour",
                     consumed == "Yes",
                     food_purchased == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed", "quantity_kg_L") %>%
              rename("wheat_flour" = "consumed",
                     "wheat_flour_kg" = "quantity_kg_L"),
            by = "hhid") %>% 

# Baked goods do not necessarily need to have been purchased, as they could have been made using purchased flour:    

  # Bread: 
  left_join(food_consumption %>% 
              filter(food_item == "Bread",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed", "quantity_kg_L") %>% 
              rename("bread" = "consumed",
                     "bread_kg" = "quantity_kg_L"), 
            by = "hhid") %>% 
  # Cake: 
  left_join(food_consumption %>% 
              filter(food_item == "Cake",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed", "quantity_kg_L") %>% 
              rename("cake" = "consumed",
                     "cake_kg" = "quantity_kg_L"), 
            by = "hhid") %>% 
  # Buns/Pofpof/Donuts: 
  left_join(food_consumption %>% 
              filter(food_item == "Buns/Pofpof/Donuts",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed", "quantity_kg_L") %>% 
              rename("buns_pofpof_donuts" = "consumed",
                     "buns_pofpof_donuts_kg" = "quantity_kg_L"), 
            by = "hhid") %>% 
  # Biscuits: 
  left_join(food_consumption %>% 
              filter(food_item == "Biscuits",
                     consumed == "Yes") %>% 
              # Create a column in analysis_df for these individuals
              dplyr::select("hhid", "consumed", "quantity_kg_L") %>% 
              rename("biscuits" = "consumed",
                     "biscuits_kg" = "quantity_kg_L"), 
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

# Create a variable for all rice consumption (both local and imported):
target_variables$rice_combined <- ifelse(target_variables$rice_local == "Yes" |
                                           target_variables$rice_imported == "Yes",
                                         "Yes", "No")

# Calculate quantity of all rice consumed (both local and imported): 
target_variables$rice_combined_kg <- ifelse(is.na(target_variables$rice_local_kg), 
                                            0, target_variables$rice_local_kg) +
                                     ifelse(is.na(target_variables$rice_imported_kg), 
                                            0, target_variables$rice_imported_kg)

# Assuming that bread, cake, buns/pofpof/donuts and biscuits all contain wheat flour, 
# consolidate these into 1 wheat_flour column, that will indicate "Yes" if any of
# the food items are consumed:

target_variables$wheat_flour <- ifelse(target_variables$bread == "Yes" | 
                                        target_variables$cake == "Yes" | 
                                        target_variables$buns_pofpof_donuts == "Yes" | 
                                        target_variables$biscuits == "Yes", 
                                       "Yes", target_variables$wheat_flour)

# For these food items, multiply quantity consumed by proportion of that food 
# item that is wheat flour:

# Now remove the other columns to include only the 4 staple grains: local rice, 
# imported rice, maize flour, wheat flour.

# target_variables <- target_variables %>% 
#   dplyr::select(-bread, -cake, -buns_pofpof_donuts, -biscuits)

#-------------------------------------------------------------------------------

# Now, in order to calculate coverage and effective coverage, I will need
# nutrient intake data for each household.

# Read in csv file containing total base case apparent nutrient intake for each household: 
nutrient_intake <- read_csv("nutrient_data/nga18_ai_basecase_MO_v1.csv")

# Create a column to indicate whether or not there is adequate intake of each micronutrient:
nutrient_intake$vitamina_adequate <- ifelse(nutrient_intake$vitamina_in_rae_in_mcg_ai >= 490, 
                                             "Adequate", "Inadequate")

nutrient_intake$thiamine_adequate <- ifelse(nutrient_intake$thiamine_in_mg_ai >= 0.9, 
                                             "Adequate", "Inadequate")

nutrient_intake$riboflavin_adequate <- ifelse(nutrient_intake$riboflavin_in_mg_ai >= 1.3, 
                                               "Adequate", "Inadequate")

nutrient_intake$niacin_adequate <- ifelse(nutrient_intake$niacin_in_mg_ai >= 11, 
                                           "Adequate", "Inadequate")

nutrient_intake$vitaminb6_adequate <- ifelse(nutrient_intake$vitaminb6_in_mg_ai >= 1.3, 
                                              "Adequate", "Inadequate")

nutrient_intake$folate_adequate <- ifelse(nutrient_intake$folate_in_mcg_ai >= 250, 
                                          "Adequate", "Inadequate")

nutrient_intake$vitaminb12_adequate <- ifelse(nutrient_intake$vitaminb12_in_mcg_ai >= 2, 
                                              "Adequate", "Inadequate")

nutrient_intake$fe_adequate <- ifelse(nutrient_intake$fe_in_mg_ai >= 22.4, 
                                      "Adequate", "Inadequate")

nutrient_intake$zn_adequate <- ifelse(nutrient_intake$zn_in_mg_ai >= 10.2,
                                      "Adequate", "Inadequate")

# Merge nutrient_intake to target_variables:
target_variables <- target_variables %>% 
  left_join(nutrient_intake %>% dplyr::select("vitamina_adequate", "thiamine_adequate",
                                              "riboflavin_adequate", "niacin_adequate",
                                              "vitaminb6_adequate","folate_adequate",
                                              "vitaminb12_adequate", "fe_adequate", 
                                              "zn_adequate", "hhid")
            , by = "hhid")

# Create a summary table to show number of households with adequate intake of each micronutrient:

label(target_variables$vitamina_adequate) <- "Vitamin A"
label(target_variables$thiamine_adequate) <- "Thiamine"
label(target_variables$riboflavin_adequate) <- "Riboflavin"
label(target_variables$niacin_adequate) <- "Niacin"
label(target_variables$vitaminb6_adequate) <- "Vitamin B6"
label(target_variables$folate_adequate) <- "Folate"
label(target_variables$vitaminb12_adequate) <- "Vitamin B12"
label(target_variables$fe_adequate) <- "Iron"
label(target_variables$zn_adequate) <- "Zinc"

table1(~ vitamina_adequate + thiamine_adequate + riboflavin_adequate +niacin_adequate + 
       vitaminb6_adequate + folate_adequate + vitaminb12_adequate + fe_adequate + zn_adequate,
       data = target_variables)

# Household risk of MND defined as inadequate intake of 2 or more of:
# - Vitamin A
# - Folate
# - Zinc
# - Iron
# - Zinc

# For each household, count the number of these micronutrients for which intake is inadequate:
target_variables <- target_variables %>% 
  mutate(n_inadequate = rowSums(dplyr::select(., vitamina_adequate, folate_adequate,
                                              vitaminb12_adequate,fe_adequate, 
                                              zn_adequate) == "Inadequate"))

# Classify the household at risk of MND if >=2 inadequate: 
target_variables$risk_MND <- ifelse(target_variables$n_inadequate >= 2, 
                                    "Yes", "No")

#-------------------------------------------------------------------------------

# Read in roster data - this gives a roster of individuals living in each household: 
roster <- read_csv("NLSS_data/Household/sect1_roster.csv")

# Count number of individuals living in each household:
roster <- roster %>% 
  group_by(hhid) %>% 
  summarise(n = n())

# Merge this data to the target_variables dataframe: 
target_variables <- target_variables %>% 
  left_join(roster, by = "hhid") %>% 
  rename("n_residents" = "n")

#-------------------------------------------------------------------------------

# Now create visualisations for REACH, COVERAGE and EFFECTIVE COVERAGE:

#-------------------------------------------------------------------------------

# REACH

# Reach is defined as: 
# n of households with access to a fortification vehicle / total n of households

riceloc_reach <- target_variables %>% count(rice_local) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1)) 

riceimp_reach <- target_variables %>% count(rice_imported) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1)) 

ricecomb_reach <- target_variables %>% count(rice_combined) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1))

maizef_reach <- target_variables %>% count(maize_flour) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1)) 

wheatf_reach <- target_variables %>% count(wheat_flour) %>% 
  mutate(percentage = round(n/sum(n) * 100, digits = 1)) 


# Visualise percentage of households consuming each food in a barplot:

grain <- c("Rice (local)", "Rice (imported)", "Rice (combined)", "Maize flour", "Wheat flour")

reach <- c(riceloc_reach[2,3], riceimp_reach[2,3], ricecomb_reach[2,3], 
           maizef_reach[2,3], wheatf_reach[2,3])

reach <- as.double(reach)

# Create data-frame to show percentage of households with access to each fortification vehicle: 
grain_reach <- data.frame(grain, reach) %>% 
  arrange(desc(reach))

colour_palette <- ghibli_palette("PonyoMedium", n = 5)

ggplot(grain_reach, aes(x = reorder(grain, -reach), 
                              y = reach, fill = grain)) + 
  geom_col(show.legend = F) + labs(x = "Grain", y = "% of Households") +
  ggtitle("Reach of each fortification vehicle in Nigeria") +
  scale_fill_manual(values = colour_palette) + theme_pander()

# Save the plot:
# ggsave("figures/reach.png", width = 10, height = 6, dpi = 800)

#-------------------------------------------------------------------------------

# COVERAGE

# Coverage is defined as:
# n of households at risk of MND with access / total n of households at risk of MND:

riceloc_coverage <- target_variables %>% filter(risk_MND == "Yes") %>% 
  count(rice_local) %>% mutate(precentage = round(n/sum(n) * 100, digits = 1))

riceimp_coverage <- target_variables %>% filter(risk_MND == "Yes") %>% 
  count(rice_imported) %>% mutate(precentage = round(n/sum(n) * 100, digits = 1))

ricecomb_coverage <- target_variables %>% filter(risk_MND == "Yes") %>% 
  count(rice_combined) %>% mutate(precentage = round(n/sum(n) * 100, digits = 1))

maizef_coverage <- target_variables %>% filter(risk_MND == "Yes") %>% 
  count(maize_flour) %>% mutate(precentage = round(n/sum(n) * 100, digits = 1))

wheatf_coverage <- target_variables %>% filter(risk_MND == "Yes") %>% 
  count(wheat_flour) %>% mutate(precentage = round(n/sum(n) * 100, digits = 1))

coverage <- c(riceloc_coverage[2,3], riceimp_coverage[2,3], ricecomb_coverage[2,3], 
           maizef_coverage[2,3], wheatf_coverage[2,3])

coverage <- as.double(coverage)

grain_coverage <- data.frame(grain, coverage) %>% 
  arrange(desc(coverage))

colour_palette <- ghibli_palette("PonyoMedium", n = 5)

ggplot(grain_coverage, aes(x = reorder(grain, -coverage), 
                        y = coverage, fill = grain)) + geom_col(show.legend = F) + 
  labs(x = "Grain", 
  y = "% of Households at risk of MND with access to fortification vehicle") +
  ggtitle("Coverage of each fortification vehicle in Nigeria") +
  scale_fill_manual(values = colour_palette) + theme_pander()

# Save the plot:
# ggsave("figures/coverage.png", width = 10, height = 6, dpi = 800)

#-------------------------------------------------------------------------------


