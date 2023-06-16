################################################################################
################### EXPLORATORY ANALYSIS OF TARGET VARIABLES ###################
################################################################################

# Install and load required packages:
rq_packages <- c("readr", "tidyverse", "ggplot2", "janitor", "knitr",
                 "wesanderson", "ghibli", "ggthemes", "table1", "kableExtra",
                 "reshape2", "srvyr", "survey")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Source script to create data-frame of target variables: 
source("code/create_targets.R")

#-------------------------------------------------------------------------------

# SURVEY WEIGHTS

# Before starting exploratory analyses, add survey weights to data: 

# Get survey weights and enumeration areas from the cover module of the survey: 
cover <- read_csv("NLSS_data/Household/secta_cover.csv")

survey_weights <- cover %>% dplyr::select(hhid, wt_final, ea)

# Join to target_variables
target_variables <- target_variables %>% 
  left_join(survey_weights, by = "hhid")

rm(list = c("cover", "survey_weights"))

# Note that some of the survey weights are missing, find the index of these entries: 
which(is.na(target_variables$wt_final))

# Manually add these survey weights according to the enumeration area (according
# to the survey documentation, all households in an EA have the same weight): 
target_variables$wt_final[6613:6615] <- 1093.0268
target_variables$wt_final[10717:10719] <- 1636.4668
target_variables$wt_final[16986] <- 1332.726
target_variables$wt_final[17107] <- 1211.833

# Create tbl_svy object using target_variables dataframe: 
svy_targets <- target_variables %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final,
                          strata = ea)

#-------------------------------------------------------------------------------

# MICRONUTRIENT ADEQUACY

# Summary statistics of micronutrient adequacy: 

# Firstly need to melt data into long format:
mn_adequacy <- melt(target_variables, id.vars = c("hhid", "wt_final", "ea"),
                    measure.vars = c("vitamina_adequate", "thiamine_adequate", 
                                     "riboflavin_adequate","niacin_adequate", 
                                     "vitaminb6_adequate", "folate_adequate", 
                                     "vitaminb12_adequate", "fe_adequate", 
                                     "zn_adequate"))

# Create tbl_svy object using using mn_adequacy dataframe for further analyses: 

svy_mn_adequacy <- mn_adequacy %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final,
                          strata = ea)

# Compute summary statistics for proportion of households with adequate intake: 
adequacy <- svy_mn_adequacy %>% 
  group_by(variable, value) %>% 
  summarise(n = survey_total()) %>% 
  mutate(perc = round(n/sum(n)*100, digits = 1)) %>% 
  filter(value == "Adequate") %>% 
  dplyr::select(variable, perc) %>% 
  rename(adequate = perc)

inadequacy <- svy_mn_adequacy %>% 
  group_by(variable, value) %>% 
  summarise(n = survey_total()) %>% 
  mutate(perc = round(n/sum(n)*100, digits = 1)) %>% 
  filter(value == "Inadequate") %>% 
  dplyr::select(variable, perc) %>% 
  rename(inadequate = perc)

missing_data <- svy_mn_adequacy  %>% 
  group_by(variable, value) %>% 
  summarise(n = survey_total()) %>% 
  mutate(perc = round(n/sum(n)*100, digits = 1)) %>% 
  filter(is.na(value)) %>% 
  dplyr::select(variable, perc) %>% 
  rename(missing = perc)

# Merge adequacy, inadequacy and missing tables:
mn_table <- adequacy %>% 
  left_join(inadequacy, by = "variable") %>% 
  left_join(missing_data, by = "variable")

rm(list = c("adequacy", "inadequacy", "missing_data"))

mn_table$variable <- c("Vitamin A", "Thiamine", "Riboflavin", "Niacin", 
                       "Vitamin B6", "Folate", "Vitamin B12", "Iron", 
                       "Zinc")

knitr::kable(mn_table, col.names = c("Micronutrient", "Adequate (%)", 
                                     "Inadequate (%)", "Missing Data (%)")) %>% 
  kable_classic(html_font = "helvetica")

# Save table from Rstudio viewer.

# Create barplot for adequacy rates of each micronutrient in descending order: 

mn_color <- ghibli_palette("PonyoMedium", 5)

mn_table <- mn_table %>% 
  filter(variable == "Vitamin A" | variable == "Folate" | 
           variable == "Vitamin B12" | variable == "Iron" | variable == "Zinc")

adequacy_barplot <- ggplot(data = mn_table, aes(x = reorder(variable, -adequate), y = adequate)) +
  geom_bar(stat = "identity", fill = mn_color) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Micronutrient", y = "Adequacy (%)") + theme_pander()

# ggsave("figures/mn_adequacy.png", plot = adequacy_barplot,
#        width = 6, height = 5, dpi = 300)

# Remove objects no longer required: 
rm(list = c("adequacy_barplot", "mn_adequacy", "mn_table", "svy_mn_adequacy",
            "mn_color"))

#-------------------------------------------------------------------------------

# REACH: 

# Calculate survey weighted reach for each grain

# Rice (both locally produced and imported): 
ricecomb_reach <- svy_targets %>% 
  group_by(rice_combined) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(rice_combined, percentage)

# Wheat flour: 
wheatf_reach <- svy_targets %>% 
  group_by(wheat_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(wheat_flour, percentage)

# Maize flour:
maizef_reach <- svy_targets %>% 
  group_by(maize_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(maize_flour, percentage)

# Create df for reach: 
grain <- c("Rice", "Maize flour", "Wheat flour")

reach <- c(ricecomb_reach[2,2], maizef_reach[2,2], wheatf_reach[2,2])

reach <- as.double(reach)

grain_reach <- data.frame(grain, reach) %>% 
  arrange(desc(reach))

rm(list = c("ricecomb_reach", "wheatf_reach", "maizef_reach", "reach"))

# Visualise percentage of households with access to each fortification vehicle:

colour_palette <- ghibli_palette("PonyoMedium", n = 3)

reach_plot <- ggplot(grain_reach, aes(x = reorder(grain, -reach), 
                                      y = reach, fill = grain)) + 
  geom_col(show.legend = F) + labs(x = "Grain", y = "% of Households") +
  ggtitle("Reach of each fortification vehicle in Nigeria") +
  scale_fill_manual(values = colour_palette) +
  geom_text(aes(label = paste0(reach, "%")), vjust = 0, nudge_y = 0.5) + 
  theme_pander()

# Save the plot:
# ggsave(plot = reach_plot, "figures/reach.png", width = 8, height = 6, dpi = 800)

# Also create a table with summary statistics for reach:
knitr::kable(grain_reach, col.names = c("Fortification vehicle",
                                        "Reach (%)")) %>% 
  kable_classic(html_font = "helvetica")

# Save table from Rstudio viewer

rm(list = c("grain_reach", "reach_plot"))

#-------------------------------------------------------------------------------

# COVERAGE:

# Calculate survey weighted coverage for each grain: 

# Rice (both locally produced and imported): 
ricecomb_coverage <- svy_targets %>% 
  filter(risk_MND == "Yes") %>% 
  group_by(rice_combined) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(rice_combined, percentage)

# Wheat flour: 
wheatf_coverage <- svy_targets %>% 
  filter(risk_MND == "Yes") %>% 
  group_by(wheat_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(wheat_flour, percentage)

# Maize flour:
maizef_coverage <- svy_targets %>% 
  filter(risk_MND == "Yes") %>% 
  group_by(maize_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(maize_flour, percentage)

# Create df for coverage:

coverage <- c( ricecomb_coverage[2,2], maizef_coverage[2,2], wheatf_coverage[2,2])

coverage <- as.double(coverage)

grain_coverage <- data.frame(grain, coverage) %>% 
  arrange(desc(coverage))

# Remove objects that are no longer required: 
rm(list = c("ricecomb_coverage", "maizef_coverage", "wheatf_coverage"))

# Create barplot for coverage

coverage_plot <- ggplot(grain_coverage, aes(x = reorder(grain, -coverage), 
                           y = coverage, fill = grain)) + geom_col(show.legend = F) + 
  labs(x = "Grain", 
       y = "% of Households at risk of MND with access to fortification vehicle") +
  ggtitle("Coverage of each fortification vehicle in Nigeria") +
  geom_text(aes(label = paste0(coverage, "%")), vjust = 0, nudge_y = 0.5) +
  scale_fill_manual(values = colour_palette) +
  theme_pander()

# Save the plot:
# ggsave("figures/coverage.png", width = 8, height = 6, dpi = 800)

# Also create a table with summary statistics for coverage:
knitr::kable(grain_coverage, col.names = c("Fortification vehicle",
                                           "Coverage (%)")) %>% 
  kable_classic(html_font = "helvetica")

# Save table from Rstudio viewer

# Remove objects that are no longer required:
rm(list = c("grain_coverage", "coverage_plot", "colour_palette"))

#-------------------------------------------------------------------------------

# COVERAGE - further exploratory analyses: 

# Examine how coverage changes if we apply different thresholds for classifying 
# a household at risk of inadequate nutritional intake from the diet: 

# create alternative thresholds for risk of MND (1 to 5): 
target_variables$risk_MND1 <- ifelse(target_variables$n_inadequate >= 1, 
                                     "Yes", "No")

target_variables$risk_MND2 <- ifelse(target_variables$n_inadequate >= 2,
                                     "Yes", "No")

target_variables$risk_MND3 <- ifelse(target_variables$n_inadequate >= 3,
                                     "Yes", "No")

target_variables$risk_MND4 <- ifelse(target_variables$n_inadequate >= 4,
                                     "Yes", "No")

target_variables$risk_MND5 <- ifelse(target_variables$n_inadequate >= 5,
                                     "Yes", "No")

# Create table to view how many households are at risk of MND using each threshold:

label(target_variables$risk_MND1) <- "1 inadequate Micronutrient"
label(target_variables$risk_MND2) <- "2 inadequate Micronutrients"
label(target_variables$risk_MND3) <- "3 inadequate Micronutrients"
label(target_variables$risk_MND4) <- "4 inadequate Micronutrients"
label(target_variables$risk_MND5) <- "5 inadequate Micronutrients"

table1(~ risk_MND1 + risk_MND2 + risk_MND3 + risk_MND4 + risk_MND5,
       data = target_variables)

# Save from Rstudio viewer

# Integrate newly created thresholds into survey weighted data: 
svy_targets <- target_variables %>% 
  srvyr::as_survey_design(ids = 1, 
                          weights = wt_final,
                          strata = ea)

# Calculate coverage using alternative thresholds for diet inadequacy:

# 1 inadequate MN:
ricecomb_coverage_alt1 <- svy_targets %>% 
  filter(risk_MND1 == "Yes") %>% 
  group_by(rice_combined) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(rice_combined, percentage)

maizef_coverage_alt1 <- svy_targets %>% 
  filter(risk_MND1 == "Yes") %>% 
  group_by(maize_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(maize_flour, percentage)

wheatf_coverage_alt1 <- svy_targets %>% 
  filter(risk_MND1 == "Yes") %>% 
  group_by(wheat_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(wheat_flour, percentage)

coverage_alt1 <- c(ricecomb_coverage_alt1[2,2], maizef_coverage_alt1[2,2], 
                   wheatf_coverage_alt1[2,2])

coverage_alt1 <- as.double(coverage_alt1)

# >= 3 inadequate MN's:
ricecomb_coverage_alt3 <- svy_targets %>% 
  filter(risk_MND3 == "Yes") %>% 
  group_by(rice_combined) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(rice_combined, percentage)

maizef_coverage_alt3 <- svy_targets %>% 
  filter(risk_MND3 == "Yes") %>% 
  group_by(maize_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(maize_flour, percentage)

wheatf_coverage_alt3 <- svy_targets %>% 
  filter(risk_MND3 == "Yes") %>% 
  group_by(wheat_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(wheat_flour, percentage)

coverage_alt3 <- c(ricecomb_coverage_alt3[2,2], maizef_coverage_alt3[2,2], 
                   wheatf_coverage_alt3[2,2])

coverage_alt3 <- as.double(coverage_alt3)

# >= 4 inadequate MN's: 
ricecomb_coverage_alt4 <- svy_targets %>% 
  filter(risk_MND4 == "Yes") %>% 
  group_by(rice_combined) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(rice_combined, percentage)

maizef_coverage_alt4 <- svy_targets %>% 
  filter(risk_MND4 == "Yes") %>% 
  group_by(maize_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(maize_flour, percentage)

wheatf_coverage_alt4 <- svy_targets %>% 
  filter(risk_MND4 == "Yes") %>% 
  group_by(wheat_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(wheat_flour, percentage)

coverage_alt4 <- c(ricecomb_coverage_alt4[2,2], maizef_coverage_alt4[2,2], 
                   wheatf_coverage_alt4[2,2])

coverage_alt4 <- as.double(coverage_alt4)


# 5 inadequate MN's: 
ricecomb_coverage_alt5 <- svy_targets %>% 
  filter(risk_MND5 == "Yes") %>% 
  group_by(rice_combined) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(rice_combined, percentage)

maizef_coverage_alt5 <- svy_targets %>% 
  filter(risk_MND5 == "Yes") %>% 
  group_by(maize_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(maize_flour, percentage)

wheatf_coverage_alt5 <- svy_targets %>% 
  filter(risk_MND4 == "Yes") %>% 
  group_by(wheat_flour) %>% 
  summarise(n = survey_total()) %>% 
  mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
  dplyr::select(wheat_flour, percentage)

coverage_alt5 <- c(ricecomb_coverage_alt5[2,2], maizef_coverage_alt5[2,2], 
                   wheatf_coverage_alt5[2,2])

coverage_alt5 <- as.double(coverage_alt5)

# Create table: 
coverage_thresholds <- data.frame(grain, coverage_alt1, coverage, coverage_alt3, 
                                  coverage_alt4, coverage_alt5)

knitr::kable(coverage_thresholds, col.names = c("Fortification vehicle", ">=1 inadeqaute MN", 
                                                ">= 2 inadequate MN's", ">= 3 inadequate MN's", 
                                                ">= 4 inadequate MN's", "5 inadequate MN's"),
             caption = "Coverage (%) using alternative thresholds for classifying 
             households as having an apparently inadequate diet") %>% 
  kable_classic(html_font = "helvetica")

################################################################################
################################ END OF SCRIPT #################################
################################################################################
