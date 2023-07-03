################################################################################
######################## EXPLORATORY ANALYSIS OF DATA ##########################
################################################################################

# Install and load required packages:
rq_packages <- c("tidyverse", "ggplot2", "table1")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Source script to read in the analysis dataframe: 
source("code/analysis_df.R")

#-------------------------------------------------------------------------------

# PREPARE VARIABLES:
analysis_df$rice_combined <- factor(analysis_df$rice_combined, 
                                    levels = c("No", "Yes"),
                                    labels = c("No access to Rice",
                                               "Access to Rice"))


analysis_df$dwelling_tenure <- factor(analysis_df$dwelling_tenure,
                                      levels = c(1, 2, 3, 4), 
                                      labels = c ("Free (unauthorised)", 
                                                  "Free (authorised)", 
                                                  "Rented", 
                                                  "Owned"))

label(analysis_df$dwelling_tenure) <- "Dwelling tenure"

analysis_df$material_floor <- factor(analysis_df$material_floor,
                                     levels = c(0, 1), 
                                     labels = c("Natural", "Finished"))

label(analysis_df$material_floor) <- "Floor material"

analysis_df$electricity <- factor(analysis_df$electricity, 
                                  levels = c(0, 1), 
                                  labels = c("No", "Yes"))

label(analysis_df$electricity) <- "Access to electricity"

analysis_df$water_source <- factor(analysis_df$water_source, 
                                   levels = c(0, 1), 
                                   labels = c("Unimproved", "Improved"))

label(analysis_df$water_source) <- "Water source"

analysis_df$toilet_facility <- factor(analysis_df$toilet_facility, 
                                      levels = c(0, 1, 2), 
                                      labels = c("Open defecaetion", 
                                                 "Unimproved facility",
                                                 "Improved facility"))

label(analysis_df$toilet_facility) <- "Toilet facility"

label(analysis_df$n_per_room) <- "Residents sleeping per room"

analysis_df$agricultural_land <- factor(analysis_df$agricultural_land, 
                                        levels = c(0, 1), 
                                        labels = c("No", "Yes"))

label(analysis_df$agricultural_land) <- "Access or ownership of agricultural land"

analysis_df$radio <- factor(analysis_df$radio, 
                            levels = c(0, 1), 
                            labels = c("No", "Yes"))

label(analysis_df$radio) <- "Ownership of radio"

analysis_df$tv <- factor(analysis_df$tv, 
                         levels = c(0, 1), 
                         labels = c("No", "Yes"))

label(analysis_df$tv) <- "Ownership of television"

analysis_df$smart_phones <- factor(analysis_df$smart_phones, 
                                   levels = c(0, 1), 
                                   labels = c("No", "Yes"))

label(analysis_df$smart_phones) <- "Ownership of smart phone"

analysis_df$reg_mobile_phone <- factor(analysis_df$reg_mobile_phone, 
                                       levels = c(0, 1), 
                                       labels = c("No", "Yes"))

label(analysis_df$reg_mobile_phone) <- "Ownership of regular mobile phone"

analysis_df$fridge <- factor(analysis_df$fridge, 
                             levels = c(0, 1), 
                             labels = c("No", "Yes"))

label(analysis_df$fridge) <- "Ownership of fridge"

analysis_df$cars_vehicles <- factor(analysis_df$cars_vehicles,
                                    levels = c(0, 1), 
                                    labels = c("No", "Yes"))

label(analysis_df$cars_vehicles) <- "Ownership of vehicle"

label(analysis_df$proportion_male) <- "Proportion of male residents"

label(analysis_df$proportion_christian) <- "Proportion of Christian residents"

label(analysis_df$proportion_muslim) <- "Proportion of Muslim residents"

label(analysis_df$proportion_traditional) <- "Proportion of residents practicing \n a traditional religion"

label(analysis_df$proportion_primary) <- "Primary education"

label(analysis_df$proportion_secondary) <- "Secondary education"

label(analysis_df$proportion_higher) <- "Higher education"

label(analysis_df$proportion_wage_salary) <- "Wage/salary job"

label(analysis_df$proportion_own_agriculture) <- "Own agricultural work"

label(analysis_df$proportion_own_NFE) <- "Own non-farm enterprise work"

label(analysis_df$proportion_trainee_apprentice) <- "Trainee/apprentice"

analysis_df$geography <- factor(analysis_df$geography, 
                                levels = c("rural", "urban"), 
                                labels = c("Rural", "Urban"))

label(analysis_df$geography) <- "Geography"

analysis_df$consumption_quintile <- factor(analysis_df$consumption_quintile, 
                                           levels = c(1, 2, 3, 4, 5),
                                           labels = c("1 - Lowest", 
                                                      "2", "3", "4", 
                                                      "5 - Highest"))

label(analysis_df$consumption_quintile) <- "Socioeconomic position quintile"

#-------------------------------------------------------------------------------

# CUSTOMISE SETTINGS FOR PRODUCING TABLES: 

# The settings below are for creating summary tables using the "table1" package.

# How to display missing data:
my.render.missing <- function(x, ..., newlabel="Missing data") {
  setNames(render.missing.default(x, ...), newlabel)
}

# How to display statistics for continuous variables:
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
       "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

# Compute P-values (for both continuous and categorical variables): 
my.pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

#-------------------------------------------------------------------------------

table1(~ dwelling_tenure + material_floor + electricity + water_source + toilet_facility +
         n_per_room + agricultural_land + radio + tv + smart_phones + reg_mobile_phone + 
         fridge + cars_vehicles + proportion_male + proportion_christian + proportion_muslim +
         proportion_traditional + proportion_primary + proportion_secondary + proportion_higher +
         proportion_wage_salary + proportion_own_agriculture + proportion_own_NFE + 
         proportion_trainee_apprentice + geography + consumption_quintile | rice_combined, 
       data = analysis_df, 
       render.missing = my.render.missing,
       render.continuous = my.render.cont, 
       overall = FALSE,
       extra.col = list(`P-value` = my.pvalue))

#-------------------------------------------------------------------------------

# GEOGRAPHY AND SOCIOECONOMIC POSITION: 

table1(~ geography + consumption_quintile | rice_combined, 
       data = analysis_df, 
       render.missing = my.render.missing,
       render.continuous = my.render.cont, 
       overall = FALSE,
       extra.col = list(`P-value` = my.pvalue))

#-------------------------------------------------------------------------------

# GENDER AND RELIGION OF HOUSEHOLD RESIDENTS: 

table1(~ proportion_male + proportion_christian + proportion_muslim +
         proportion_traditional | rice_combined, 
       data = analysis_df, 
       render.missing = my.render.missing,
       render.continuous = my.render.cont, 
       overall = FALSE,
       extra.col = list(`P-value` = my.pvalue))

#-------------------------------------------------------------------------------

# HOUSEHOLD AND FACILITIES: 

table1(~ dwelling_tenure + material_floor + electricity + water_source + 
         toilet_facility + n_per_room + agricultural_land | rice_combined, 
       data = analysis_df, 
       render.missing = my.render.missing,
       render.continuous = my.render.cont, 
       overall = FALSE,
       extra.col = list(`P-value` = my.pvalue))

#-------------------------------------------------------------------------------

# HOUSEHOLD ASSETS: 

table1(~ radio + tv + smart_phones + reg_mobile_phone + fridge + 
         cars_vehicles | rice_combined, 
       data = analysis_df, 
       render.missing = my.render.missing,
       render.continuous = my.render.cont, 
       overall = FALSE,
       extra.col = list(`P-value` = my.pvalue))

#-------------------------------------------------------------------------------

# EDUCATION AND LABOUR: 

table1(~ proportion_primary + proportion_secondary + proportion_higher +
         proportion_wage_salary + proportion_own_agriculture + proportion_own_NFE + 
         proportion_trainee_apprentice | rice_combined, 
       data = analysis_df, 
       render.missing = my.render.missing,
       render.continuous = my.render.cont, 
       overall = FALSE,
       extra.col = list(`P-value` = my.pvalue))
