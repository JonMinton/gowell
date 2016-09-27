rm(list = ls())


#install.packages("pacman")

require(pacman)
p_load(
    readr, stringr, 
    purrr, tidyr, dplyr, 
    ggplot2, tmap, rgeos,
    xlsx)


# All diversities data 
all_diversities <- read_csv("data/derived/all_diversities.csv")

all_diversities %>% 
    filter(!is.na(year)) %>% 
    group_by(category) %>% 
    mutate(
        nr_2001 = abs(year - 2001),
        nr_2011 = abs(year - 2011),
        
        mn_2001 = nr_2001 == min(nr_2001),
        mn_2011 = nr_2011 == min(nr_2011),
        
        period = ifelse(mn_2001, "t1", ifelse(mn_2011, "t2", NA))
        ) %>% 
    select(-nr_2001, -nr_2011, -mn_2001, -mn_2011) %>% 
    ungroup() -> tidied_diversities 



# Task 1: Diversity by UR class -------------------------------------------

ur_class <- read_csv("data/derived/dz_2001_by_ur_6fold_class.csv")
#diversity_H <- read_csv("data/derived/p_all_H.csv")
div_ur <- tidied_diversities %>%
    left_join(ur_class, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, category, ur_class, simpson)


# Table of diversities by urban-rural class
div_ur_summaries <- div_ur  %>% 
    group_by(period, ur_class, category)   %>% 
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
    filter(!is.na(ur_label)) %>% 
    select(period, ur_label, category, value = med_H) %>% 
    filter(period %in% c("t1", "t2")) %>% spread(period, value)

dta1 %>% 
    ggplot(., aes(y = ur_label)) +
    facet_grid(category  ~ .) + 
    geom_segment(aes(x = t1, xend = t2, yend = ur_label), arrow = arrow(length = unit(0.1, "npc"), type = "closed")) + 
    labs(
        x = "Median diversity score", 
        y = "Urban/Rural Class", 
        title = "Diversity Change by Urban/Rural Class"
    ) +
    theme(strip.text.y = element_text(angle = 0))

ggsave("figures/change_in_diversity_by_urclass.png", height = 30, width = 15, units = "cm", dpi = 300)


    
#     
# - Look at the geography of diversity (at each of the two time points?) in terms of:
# o   SG urban-rural 6-fold classification.
# o   Distance of datazone to city centre.
# o   Density (spatial area and population of datazone).


# Task 2: relationship between diversity and distance to city cent --------


distance_dz <- read_csv("data/derived/dz_2001_by_distance_from_gg_centre.csv")

div_dist <- tidied_diversities %>% 
    left_join(distance_dz, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, distance_to_centre, category, simpson) %>% 
    filter(category !="land_bus") %>% 
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

# diversity distance relationship, up to 15km
div_dist %>% 
    filter(!is.na(period)) %>% 
    filter(distance_to_centre < 40000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    ggplot(., aes(x = distance_to_centre, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category) + 
    labs(x = "Distance to city centre in km", y = "Diversity score")

ggsave("figures/diversity_distance_upto40km.png", width = 20, height = 20, units = "cm", dpi = 300)


# Diversity distance relationship, up to 15km
div_dist %>% 
    filter(!is.na(period)) %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    ggplot(., aes(x = distance_to_centre, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category) + 
    labs(x = "Distance to city centre in km", y = "Diversity score")
ggsave("figures/diversity_distance_upto15km.png", width = 20, height = 20, units = "cm", dpi = 300)


# Diversity-distance, up to 15km, for ethnicity, country of origin, built type and tenure
div_dist %>% 
    filter(!is.na(period)) %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(category %in% c("Built: Type", "Demographic:\nCountry of Origin", "Demographic:\nEthnicity","Tenure")) %>% 
    ggplot(., aes(x = distance_to_centre, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category, scale = "free_y") + 
    labs(x = "Distance to city centre in km", y = "Diversity score") + 
    scale_x_continuous(breaks = seq(0, 15, by = 1)) + 
    geom_vline(aes(xintercept = 5), linetype ="dashed") + 
    geom_vline(aes(xintercept = 10), linetype = "solid")
ggsave("figures/diversity_distance_tenurecoohousingstock.png", width =30, height = 30, units = "cm", dpi = 300)


# I want to know the correlations between these variables 

# div_dist  %>% filter(period == "t1")  %>% mutate(category = str_replace(category, "\\n", ""))  %>% spread(category, simpson)  %>% arrange(distance_to_centre)  %>% .[,4:16]  %>% cor(., method = "spearman", use = "pairwise.complete.obs")   %>%  write.csv("clipboard")
# div_dist  %>% filter(period == "t2")  %>% mutate(category = str_replace(category, "\\n", ""))  %>% spread(category, simpson)  %>% arrange(distance_to_centre)  %>% .[,4:16]  %>% cor(., method = "spearman", use = "pairwise.complete.obs")  %>%  write.csv("clipboard")

# # Temporal correlations within variables 
# div_dist %>%  filter(!is.na(period)) %>% 
#     spread(period, simpson) %>% select(category, t1, t2) %>% 
#     group_by(category) %>% nest() %>% 
#     mutate(cr = map_dbl(data, ~cor(., use = "pairwise.complete.obs", method ="spearman")[2,1]))
# 

# What about the three built environment indicators?
div_dist %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(str_detect(category, "Built")) %>% 
    ggplot(., aes(x = distance_to_centre, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category, scale = "free_y") + 
    labs(x = "Distance to city centre in km", y = "Diversity score") + 
    scale_x_continuous(breaks = seq(0, 15, by = 1)) + 
    geom_vline(aes(xintercept = 8), linetype ="dashed") + 
    geom_vline(aes(xintercept = 10), linetype = "solid")
ggsave("figures/diversity_distance_builtonly.png", width =30, height = 30, units = "cm", dpi = 300)

# What about the demographic variables?
div_dist %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(str_detect(category, "Demographic")) %>% 
    ggplot(., aes(x = distance_to_centre, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category, scale = "free_y") + 
    labs(x = "Distance to city centre in km", y = "Diversity score") + 
    scale_x_continuous(breaks = seq(0, 15, by = 1)) + 
    geom_vline(aes(xintercept = 4), linetype ="dashed") + 
    geom_vline(aes(xintercept = 6), linetype = "solid")
ggsave("figures/diversity_distance_demogrpahiconly.png", width =30, height = 30, units = "cm", dpi = 300)

# And now economic
div_dist %>% 
    filter(distance_to_centre < 15000) %>% 
    mutate(distance_to_centre = distance_to_centre / 1000) %>% 
    filter(str_detect(category, "Economic")) %>% 
    ggplot(., aes(x = distance_to_centre, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    stat_smooth(colour = "black") + 
    facet_wrap(~category, scale = "free_y") + 
    labs(x = "Distance to city centre in km", y = "Diversity score") + 
    scale_x_continuous(breaks = seq(0, 15, by = 1)) 
ggsave("figures/diversity_distance_economiconly.png", width =30, height = 30, units = "cm", dpi = 300)



# Task 3: diversity and density -------------------------------------------


# Perhaps the most interesting now.. diversity against density

density_dz <- read_csv("data/derived/population_density_by_2001_dz.csv")


dens_dist <- tidied_diversities %>% 
    left_join(density_dz, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, population_density, category, simpson)    


dens_dist <- dens_dist %>% 
    filter(category !="land_bus") %>%
    filter(!is.na(period)) %>% 
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

dens_dist %>% 
    filter(population_density >=50) %>% 
    ggplot(., aes(x = population_density, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
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
dens_dist %>% 
    ggplot(., aes(x = population_density, y = simpson, group = period, linetype = period)) +
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
dens_dist %>% 
    mutate(population_density = population_density / 1000) %>% 
    ggplot(., aes(x = population_density, y = simpson, group = period, linetype = period)) +
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
dens_dist %>% 
    mutate(population_density = population_density / 1000) %>%
    filter(category %in% c("Demographic:\nEthnicity", "Built: Type", "Tenure")) %>% 
    ggplot(., aes(x = population_density, y = simpson, group = period, linetype = period)) +
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

#View(simd)
simd_simple <- simd %>% 
    select(datazone = `Data Zone`, simd_score = `Overall SIMD 2012 Score`)

simd_diversity <- tidied_diversities %>% 
    inner_join(simd_simple) %>% 
    select(datazone, period, simd_score, category, simpson)  %>% 
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



simd_diversity %>%
    ggplot(., aes(x = simd_score, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category) + 
    stat_smooth(colour = "black") + 
    labs(x = "SIMD Score (lowest = least deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_deprivation.png", height = 20, width = 20, units = "cm")


# Now the same but for the income domain

simd_income <- simd %>% 
    select(datazone = `Data Zone`, simd_score = `Education, Skills and Training domain 2012 score`)

simd_income_diversity <- tidied_diversities %>% 
    inner_join(simd_income) %>% 
    select(datazone, period, simd_score, category, simpson)  %>% 
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



simd_income_diversity %>%
    ggplot(., aes(x = simd_score, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category) + 
    stat_smooth(colour = "black") + 
    labs(x = "Income Deprivation Score (lowest = least deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_income_deprivation.png", height = 20, width = 20, units = "cm")

# And how housing deprivation

simd_housing <- simd %>% 
    select(datazone = `Data Zone`, simd_score = `Housing domain score 2004, 2006, 2009 & 2012`)

simd_housing_diversity <- tidied_diversities %>% 
    inner_join(simd_housing) %>% 
    select(datazone, period, simd_score, category, simpson)  %>% 
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

simd_housing_diversity %>%
    ggplot(., aes(x = simd_score, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Housing Deprivation Score (lowest = least deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_housing_deprivation.png", height = 20, width = 20, units = "cm")


# And now skills deprivation

simd_skills <- simd %>% 
    select(datazone = `Data Zone`, simd_score = `Education, Skills and Training domain 2012 score`)

simd_skills_diversity <- tidied_diversities %>% 
    inner_join(simd_skills) %>% 
    select(datazone, period, simd_score, category, simpson)  %>% 
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


simd_skills_diversity %>%
    ggplot(., aes(x = simd_score, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Skills Deprivation Score (lowest = least deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_skills_deprivation.png", height = 20, width = 20, units = "cm")

# NOTE: SKILLS DEPRIVATION SEEMS IMPORTANT VARIABLE
# Note the flattening out in SEC and rise in diversity of highest qual in most deprived areas


# And now health deprivation

simd_health <- simd %>% 
    select(datazone = `Data Zone`, simd_score = `Health domain 2012 score`)

simd_health_diversity <- tidied_diversities %>% 
    inner_join(simd_health) %>% 
    select(datazone, period, simd_score, category, simpson)  %>% 
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


simd_health_diversity %>%
    ggplot(., aes(x = simd_score, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Health Deprivation Score (lowest = least deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_health_deprivation.png", height = 20, width = 20, units = "cm")


# And now geographic access  deprivation

simd_geog <- simd %>% 
    select(datazone = `Data Zone`, simd_score = `Geographic Access domain 2012 score`)

simd_geog_diversity <- tidied_diversities %>% 
    inner_join(simd_geog) %>% 
    select(datazone, period, simd_score, category, simpson)  %>% 
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



simd_geog_diversity %>%
    ggplot(., aes(x = simd_score, y = simpson, group = period, linetype = period)) +
    geom_point(aes(colour = period, shape = period), alpha = 0.1) +
    facet_wrap(~ category, scale = "free_y") + 
    stat_smooth(colour = "black") + 
    labs(x = "Geographic Access Deprivation Score (lowest = least deprived)", y = "Diversity scores")
ggsave("figures/diversity_by_geographicaccess_deprivation.png", height = 20, width = 20, units = "cm")





# decile tables  --------------------------------------------------------

# distance to city centre
div_dist  %>% 
    mutate(dist_decile = factor(ntile(distance_to_centre, 10)))  %>% 
    filter(!is.na(dist_decile)) %>% 
    group_by(period, category, dist_decile)  %>% 
    summarise(mean_s = mean(simpson))  %>%  
    ggplot(., 
           aes(x = dist_decile, y= mean_s, shape = period, group = period, linetype = period)
           ) + 
    facet_wrap(~category) + 
    geom_point() + 
    geom_line()


# density 

dens_dist %>% 
    mutate(dens_decile = factor(ntile(population_density, 10))) %>% 
    filter(!is.na(dens_decile)) %>% 
    group_by(period, category, dens_decile) %>% 
    summarise(mean_s = mean(simpson)) %>% 
    ggplot(., 
           aes(x = dens_decile, y= mean_s, shape = period, group = period, linetype = period)
    ) + 
    facet_wrap(~category) + 
    geom_point() + 
    geom_line()

# Simd diversity 

simd_diversity %>% 
    mutate(simd_decile = factor(ntile(simd_score, 10))) %>% 
    filter(!is.na(simd_decile)) %>% 
    group_by(period, category, simd_decile) %>% 
    summarise(mean_s = mean(simpson, na.rm = T)) %>% 
    ggplot(., 
           aes(x = simd_decile, y= mean_s, shape = period, group = period, linetype = period)
    ) + 
    facet_wrap(~category) + 
    geom_point() + 
    geom_line()






