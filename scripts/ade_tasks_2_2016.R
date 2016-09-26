rm(list = ls())


#install.packages("pacman")

require(pacman)
p_load(
    readr, stringr, 
    purrr, tidyr, dplyr, 
    ggplot2, tmap, rgeos,
    xlsx)


# Task 1: Diversity by UR class -------------------------------------------

ur_class <- read_csv("data/derived/dz_2001_by_ur_6fold_class.csv")
#diversity_H <- read_csv("data/derived/p_all_H.csv")
all_diversities <- read_csv("data/derived/all_diversities.csv")
div_h_ur <- all_diversities %>%
    left_join(ur_class, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, year, category, ur_class, simpson)

div_ur_summaries <- div_h_ur  %>% 
    group_by(year, ur_class, category)   %>% 
    summarise(
        n_missing = length(is.na(simpson)), 
        min_H = min(simpson, na.rm =T),
        med_H = median(simpson, na.rm = T), 
        max_H = max(simpson, na.rm = T), 
        mean_H = mean(simpson, na.rm = T),
        sd_H = sd(simpson, na.rm = T), 
        lower_025 = quantile(simpson, 0.025, na.rm = T),
        upper_975 = quantile(simpson, 0.975, na.rm = T), 
        lower_250 = quantile(simpson, 0.250, na.rm = T),
        upper_750 = quantile(simpson, 0.750, na.rm = T)
        )


# Now to start exploring this

# Exploration of UR class effects -----------------------------------------


div_ur_summaries %>%
    ungroup() %>% 
    filter(year == 2001) %>% 
    mutate(ur_label = factor(
        ur_class, 
        levels = c(1, 2, 3, 5, 6), 
        labels = c("large_urban", "other_urban", "accessible small towns", "accessible_rural", "remote_rural")
        )
    ) %>% 
    select(ur_label, category, value = med_H) %>% 
    mutate(value = round(value, 3)) %>% 
    spread(ur_label, value)


dta1 <- div_ur_summaries %>%
    ungroup() %>% 
    mutate(ur_label = factor(
        ur_class, 
        levels = c(1, 2, 3, 5, 6), 
        labels = c("Large Urban", "Other Urban", "Accessible Small Towns", "Accessible Rural", "Remote Rural")
    )
    ) %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nBuilding Use type", 
            "Land Use:\nVacant/occupied land",
            "Tenure"
            )
    )) %>% 
    filter(category != "Land Use:\nBuilding Use type") %>%  # dropping as only for t2
    select(year, ur_label, category, value = med_H) %>% 
    filter(year %in% c(2001, 2011)) %>% spread(year, value)

dta1 %>% 
    ggplot(., aes(y = ur_label)) +
    facet_grid(category  ~ .) + 
    geom_segment(aes(x = `2001`, xend = `2011`, yend = ur_label), arrow = arrow(length = unit(0.1, "npc"), type = "closed")) + 
    labs(
        x = "Median diversity score", 
        y = "Urban/Rural Class", 
        title = "Diversity Change by Urban/Rural Class"
    ) +
    theme(strip.text.y = element_text(angle = 0))

ggsave("figures/change_in_diversity.png", height = 30, width = 15, units = "cm", dpi = 300)


    
#     
# - Look at the geography of diversity (at each of the two time points?) in terms of:
# o   SG urban-rural 6-fold classification.
# o   Distance of datazone to city centre.
# o   Density (spatial area and population of datazone).


# Task 2: relationship between diversity and distance to city cent --------


distance_dz <- read_csv("data/derived/dz_2001_by_distance_from_gg_centre.csv")

div_h_dist <- all_diversities %>% 
    left_join(distance_dz, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, year, distance_to_centre, category, simpson) 

div_h_dist <- div_h_dist %>% filter(category !="land_bus") %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nVacant/occupied land",
            "Tenure"
        )
    ))



# Now to visualise this 

div_h_dist %>% 
    filter(year %in% c(2001, 2011)) %>% 
    mutate(year = factor(year)) %>% 
    filter(distance_to_centre < 40000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    ggplot(., aes(x = distance_to_centre, y = simpson, group = year, linetype = year)) +
    geom_point(aes(colour = year, shape = year), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category) + 
    labs(x = "Distance to city centre in km", y = "Diversity score")

ggsave("figures/diversity_distance_upto40km.png", width = 20, height = 20, units = "cm", dpi = 300)

div_h_dist %>% 
    filter(year %in% c(2001, 2011)) %>% 
    mutate(year = factor(year)) %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    ggplot(., aes(x = distance_to_centre, y = simpson, group = year, linetype = year)) +
    geom_point(aes(colour = year, shape = year), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category) + 
    labs(x = "Distance to city centre in km", y = "Diversity score")
ggsave("figures/diversity_distance_upto15km.png", width = 20, height = 20, units = "cm", dpi = 300)



div_h_dist %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(category %in% c("Built: Type", "Demographic:\nCountry of Origin", "Tenure")) %>% 
    ggplot(., aes(x = distance_to_centre, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category, scale = "free_y") + 
    labs(x = "Distance to city centre in km", y = "Diversity score") + 
    scale_x_continuous(breaks = seq(0, 15, by = 1)) + 
    geom_vline(aes(xintercept = 8), linetype ="dashed") + 
    geom_vline(aes(xintercept = 10), linetype = "solid")
ggsave("figures/diversity_distance_tenurecoohousingstock.png", width =30, height = 30, units = "cm", dpi = 300)


# What about the three built environment indicators?
div_h_dist %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(str_detect(category, "Built")) %>% 
    ggplot(., aes(x = distance_to_centre, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category, scale = "free_y") + 
    labs(x = "Distance to city centre in km", y = "Diversity score") + 
    scale_x_continuous(breaks = seq(0, 15, by = 1)) + 
    geom_vline(aes(xintercept = 8), linetype ="dashed") + 
    geom_vline(aes(xintercept = 10), linetype = "solid")
ggsave("figures/diversity_distance_builtonly.png", width =30, height = 30, units = "cm", dpi = 300)

# What about the demographic variables?
div_h_dist %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(str_detect(category, "Demographic")) %>% 
    ggplot(., aes(x = distance_to_centre, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category, scale = "free_y") + 
    labs(x = "Distance to city centre in km", y = "Diversity score") + 
    scale_x_continuous(breaks = seq(0, 15, by = 1)) + 
    geom_vline(aes(xintercept = 4), linetype ="dashed") + 
    geom_vline(aes(xintercept = 6), linetype = "solid")
ggsave("figures/diversity_distance_demogrpahiconly.png", width =30, height = 30, units = "cm", dpi = 300)

# And now economic
div_h_dist %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(str_detect(category, "Economic")) %>% 
    ggplot(., aes(x = distance_to_centre, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category, scale = "free_y") + 
    labs(x = "Distance to city centre in km", y = "Diversity score") + 
    scale_x_continuous(breaks = seq(0, 15, by = 1)) 
ggsave("figures/diversity_distance_economiconly.png", width =30, height = 30, units = "cm", dpi = 300)



# Task 3: diversity and density -------------------------------------------


# Perhaps the most interesting now.. diversity against density

density_dz <- read_csv("data/derived/population_density_by_2001_dz.csv")


dens_h_dist <- all_diversities %>% 
    left_join(density_dz, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, year, population_density, category, simpson)    


dens_h_dist <- dens_h_dist %>% filter(category !="land_bus") %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nVacant/occupied land",
            "Tenure"
        )
    ))

dens_h_dist %>% 
    filter(year %in% c(2001, 2011)) %>% 
    filter(population_density < quantile(population_density, 0.99, na.rm =T)) %>% 
    mutate(year = factor(year)) %>% 
    ggplot(., aes(x = population_density, y = simpson, group = year, colour = year)) +
    geom_point(alpha = 0.1) +
    stat_smooth() + 
    facet_wrap(~category, scale = "free_y") 


dens_h_dist %>% 
    filter(year %in% c(2001, 2011)) %>% 
    mutate(year = factor(year)) %>% 
    ggplot(., aes(x = population_density, y = simpson, group = year, linetype = year)) +
    geom_point(aes(colour = year, shape = year), alpha = 0.1) +
    stat_smooth(colour = "black") +
    facet_wrap(~category, scale = "free_y") + 
    scale_x_log10(
        breaks = c(50, 100, 500, 1000, 2000, 5000, 10000, 20000),
        labels = c("50", "100", "500", "1k", "2k", "5k", "10k", "20k")
        ) + 
    geom_vline(aes(xintercept = 3390), linetype = "dashed") + # Indicative average density for Glasgow overall
    labs(x = "Persons per square km", y = "Diversity score")
    

ggsave("figures/diversity_by_population_density.png", height = 20, width = 30, units = "cm")

# Now to look at places with at least 1k/km
dens_h_dist %>% 
    ggplot(., aes(x = population_density, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") +
    facet_wrap(~category, scale = "free_y") + 
    scale_x_log10(
        limits = c(1000, 15000),
        breaks = c(1000, 2000, 5000, 10000, 15000),
        labels = c("1", "2", "5", "10", "15")
    ) + 
    geom_vline(aes(xintercept = 3390), linetype = "dashed") + # Indicative average density for Glasgow overall
    geom_vline(aes(xintercept = 6000)) + 
    labs(x = "Thousand of persons per square km", y = "Diversity score")
ggsave("figures/diversity_by_population_density_higherdensonly.png", height = 20, width = 20, units = "cm")
# Now to look at places with at least 1k/km
dens_h_dist %>% 
    mutate(population_density = population_density / 1000) %>% 
    ggplot(., aes(x = population_density, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") +
    facet_wrap(~category, scale = "free_y") + 
    scale_x_continuous(
        limits = c(1, 15),
        breaks = 1:15
    ) + 
    geom_vline(aes(xintercept = 3.390), linetype = "dashed") + # Indicative average density for Glasgow overall
    geom_vline(aes(xintercept = 6)) + 
    labs(x = "Thousand of persons per square km", y = "Diversity score")
ggsave("figures/diversity_by_population_density_higherdensonly_linearscale.png", height = 20, width = 20, units = "cm")

# And now to focus just on ethnicity, built type, and tenure
dens_h_dist %>% 
    mutate(population_density = population_density / 1000) %>%
    filter(category %in% c("Demographic:\nEthnicity", "Built: Type", "Tenure")) %>% 
    ggplot(., aes(x = population_density, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") +
    facet_wrap(~category) + 
    scale_x_continuous(
        limits = c(1, 15),
        breaks = 1:15
    ) + 
    geom_vline(aes(xintercept = 3.390), linetype = "dashed") + # Indicative average density for Glasgow overall
    geom_vline(aes(xintercept = 6)) + 
    labs(x = "Thousand of persons per square km", y = "Diversity score")
ggsave("figures/diversity_by_population_density_builttenureethnicity_linearscale.png", height = 20, width = 20, units = "cm")



# task 4: deprivation and diversity - quintile, change in -----------------


simd <- read_csv("data/simd/00410767.csv")
diversity_H <- read_csv("data/derived/p_all_H.csv")

#View(simd)
simd_simple <- simd %>% 
    select(dz_2001 = `Data Zone`, simd_score = `Overall SIMD 2012 Score`)

simd_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(simd_simple, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, simd_score, category, H)  %>% 
    filter(category != "land_bus") %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nVacant/occupied land",
            "Tenure"
        )
    ))



simd_h_dist %>%
    ggplot(., aes(x = simd_score, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "SIMD Score (lowest = least deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_deprivation.png", height = 20, width = 20, units = "cm")


# Now the same but for the income domain

simd_simple <- simd %>% 
    select(dz_2001 = `Data Zone`, simd_score = `Education, Skills and Training domain 2012 score`)

simd_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(simd_simple, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, simd_score, category, H)  %>% 
    filter(category != "land_bus") %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nVacant/occupied land",
            "Tenure"
        )
    ))



simd_h_dist %>%
    ggplot(., aes(x = simd_score, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Income Deprivation Score (lowest = lest deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_income_deprivation.png", height = 20, width = 20, units = "cm")

# And how housing deprivation

simd_simple <- simd %>% 
    select(dz_2001 = `Data Zone`, simd_score = `Housing domain score 2004, 2006, 2009 & 2012`)

simd_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(simd_simple, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, simd_score, category, H)  %>% 
    filter(category != "land_bus") %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nVacant/occupied land",
            "Tenure"
        )
    ))



simd_h_dist %>%
    ggplot(., aes(x = simd_score, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Housing Deprivation Score (lowest = lest deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_housing_deprivation.png", height = 20, width = 20, units = "cm")


# And now skills deprivation

simd_simple <- simd %>% 
    select(dz_2001 = `Data Zone`, simd_score = `Education, Skills and Training domain 2012 score`)

simd_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(simd_simple, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, simd_score, category, H)  %>% 
    filter(category != "land_bus") %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nVacant/occupied land",
            "Tenure"
        )
    ))



simd_h_dist %>%
    ggplot(., aes(x = simd_score, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Skills Deprivation Score (lowest = lest deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_skills_deprivation.png", height = 20, width = 20, units = "cm")

# And now health deprivation

simd_simple <- simd %>% 
    select(dz_2001 = `Data Zone`, simd_score = `Health domain 2012 score`)

simd_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(simd_simple, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, simd_score, category, H)  %>% 
    filter(category != "land_bus") %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nVacant/occupied land",
            "Tenure"
        )
    ))



simd_h_dist %>%
    ggplot(., aes(x = simd_score, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Health Deprivation Score (lowest = lest deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_health_deprivation.png", height = 20, width = 20, units = "cm")


# And now geographic access  deprivation

simd_simple <- simd %>% 
    select(dz_2001 = `Data Zone`, simd_score = `Geographic Access domain 2012 score`)

simd_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(simd_simple, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, simd_score, category, H)  %>% 
    filter(category != "land_bus") %>% 
    mutate(category = factor(
        category, 
        labels = c(
            "Built: Band",
            "Built: Size",
            "Built: Type",
            "Demographic:\nAge Structure",
            "Demographic:\nCountry of Origin",
            "Demographic:\nEthnicity",
            "Demographic:\nReligion",
            "Economic:\nEconomic Activity",
            "Economic:\nIndustry of Employment",
            "Economic:\nHighest Qualification",
            "Economic:\nSocioeconomic\nClassification",
            "Land Use:\nVacant/occupied land",
            "Tenure"
        )
    ))



simd_h_dist %>%
    ggplot(., aes(x = simd_score, y = H, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Geographic Access Deprivation Score (lowest = lest deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_geographicaccess_deprivation.png", height = 20, width = 20, units = "cm")



# Some comments/ideas on this: 

# Largest % change is in tenure, around 60% in most deprived quintile. 
# I assume this is to do with private rented sector taking up the strain
# from social rented sector 

# The next largest % change is in ethnicity, which has risen by around 20% 
# in all SIMD areas, but around three times that level in the least deprived 
# areas. This may be both because the least deprived areas were previously 
# the least diverse, but also because as Glasgow's economy has improved wealthier 
# migrants have been attracted to the city, and with more wealth have elected to 
# live in the least deprived areas. 

# There has been very little change in the diversity of industries that employ people.
# (need to check: I think this is broad categories of SIC)

# Largest decline is in vacant land, falling most in least deprived areas
# This suggests more land is being put to use, and so a smaller proportion 
# is vacant, pointing to general economic improvement from 2001 to 2011, meaning
# land has become more valuable. 
# 

# 
# -  Look at relative change over time in each measure of diversity 
# for each quintile of deprivation (to produce a table such as sketched 
#                                   on the second page attached).
# 


# -  Recalculate the diversity measures for 2011 using 
# three categories for each diversity dimension, to make the scores comparable. 
# Producing median scores for each SIMD quintile and for all datazones together.

# still to do  - go back a stage

# 
# - Include household type as a diversity dimension 
# (possibly using three categories? 
# Adult hhds; 
# family hhds; 
# older person households – 



# depends what the census already provides and how you can combine existing 
# categories).
# 
# We thought we would leave the question of how different dimensions of 
# diversity are associated with each other across datazones for later.
# 
# Once we have this lot to hand, we’ll meet to go over the results and 
# plan the writing of the first paper.
# 
# You also thought of having a look at what health measures you can access 
# for datazones with a view to the next paper on health outcomes 
# associated with diversity.
# 
# Yours,
# 

