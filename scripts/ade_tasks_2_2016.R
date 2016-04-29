rm(list = ls())



require(readr)
require(stringr)

require(plyr)
require(tidyr)
require(dplyr)

require(ggplot2)
require(tmap)
require(rgeos)


# Task 1: Diversity by UR class -------------------------------------------

ur_class <- read_csv("data/derived/dz_2001_by_ur_6fold_class.csv")
    diversity_H <- read_csv("data/derived/p_all_H.csv")

div_h_ur <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(ur_class, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, ur_class, category, H) 
    
div_ur_summaries <- div_h_ur  %>% 
    group_by(period, ur_class, category)   %>% 
    summarise(
        n_missing = length(is.na(H)), 
        min_H = min(H, na.rm =T),
        med_H = median(H, na.rm = T), 
        max_H = max(H, na.rm = T), 
        mean_H = mean(H, na.rm = T),
        sd_H = sd(H, na.rm = T), 
        lower_025 = quantile(H, 0.025, na.rm = T),
        upper_975 = quantile(H, 0.975, na.rm = T), 
        lower_250 = quantile(H, 0.250, na.rm = T),
        upper_750 = quantile(H, 0.750, na.rm = T)
        )


# Now to start exploring this

# Exploration of UR class effects -----------------------------------------


div_ur_summaries %>%
    ungroup() %>% 
    filter(period == "t1") %>% 
    mutate(ur_label = factor(
        ur_class, 
        levels = c(1, 2, 3, 5, 6), 
        labels = c("large_urban", "other_urban", "accessible small towns", "accessible_rural", "remote_rural")
        )
    ) %>% 
    select(ur_label, category, value = med_H) %>% 
    mutate(value = round(value, 2)) %>% 
    spread(ur_label, value)

div_ur_summaries %>%
    ungroup() %>% 
    filter(period == "t1") %>% 
    mutate(ur_label = factor(
        ur_class, 
        levels = c(1, 2, 3, 5, 6), 
        labels = c("large_urban", "other_urban", "accessible small towns", "accessible_rural", "remote_rural")
    )
    ) %>% 
    select(ur_label, category, value = med_H) %>% 
    ggplot(., aes(y = ur_label, x = value)) +
    facet_wrap(~ category, scales = "free_x") +
    geom_point() + 
    labs(x = "Median diversity score", y = "Urban/Rural Class", title = "Diversity by UR class in 2001")
    

# Now for both years on the same plot

div_ur_summaries %>%
    ungroup() %>% 
    mutate(ur_label = factor(
        ur_class, 
        levels = c(1, 2, 3, 5, 6), 
        labels = c("large_urban", "other_urban", "accessible small towns", "accessible_rural", "remote_rural")
    )
    ) %>% 
    select(ur_label, period, category, value = med_H) %>% 
    ggplot(., aes(y = ur_label, x = value, group = period, shape = period, colour = period)) +
    facet_wrap(~ category, scales = "free_x") +
    geom_point() + 
    labs(x = "Median diversity score", y = "Urban/Rural Class", title = "Diversity by UR class in 2001 and 2011")


ggsave(filename = "figures/median_diversity_by_urclass.png", width = 20, height = 20, units = "cm")
#     
# - Look at the geography of diversity (at each of the two time points?) in terms of:
# o   SG urban-rural 6-fold classification.
# o   Distance of datazone to city centre.
# o   Density (spatial area and population of datazone).


# Task 2: relationship between diversity and distance to city cent --------


distance_dz <- read_csv("data/derived/dz_2001_by_distance_from_gg_centre.csv")
diversity_H <- read_csv("data/derived/p_all_H.csv")

div_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(distance_dz, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, distance_to_centre, category, H) 



# Now to visualise this 

div_h_dist %>% 
    ggplot(., aes(x = distance_to_centre, y = H, group = period, colour = period)) +
    geom_point(alpha = 0.1) +
    stat_smooth() + 
    facet_wrap(~category, scale = "free_y") 

# What's improtant here is that most of the change in diversity only occures within the 
# first 5-10 km or so of the distance from city centre 

# This suggests using a log X scale

div_h_dist %>% 
    ggplot(., aes(x = distance_to_centre, y = H, group = period, colour = period)) +
    geom_point(alpha = 0.1) +
    stat_smooth() + 
    facet_wrap(~category, scale = "free_y") +
    scale_x_log10()
ggsave("figures/diversity_against_log_distance_from_centre.png", width = 20, height = 20, units = "cm")



# Task 3: diversity and density -------------------------------------------


# Perhaps the most interesting now.. diversity against density

density_dz <- read_csv("data/derived/population_density_by_2001_dz.csv")
diversity_H <- read_csv("data/derived/p_all_H.csv")

dens_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(density_dz, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, population_density, category, H)    

dens_h_dist %>% 
    ggplot(., aes(x = population_density, y = H, group = period, colour = period)) +
    geom_point(alpha = 0.1) +
    stat_smooth() + 
    facet_wrap(~category, scale = "free_y") 


dens_h_dist %>% 
    ggplot(., aes(x = population_density, y = H, group = period, colour = period)) +
    geom_point(alpha = 0.1) +
    stat_smooth() + 
    facet_wrap(~category, scale = "free_y") + 
    scale_x_log10()

ggsave("figures/diveristy_by_population_density.png", height = 20, width = 20, units = "cm")




# task 4: deprivation and diversity - quintile, change in -----------------


simd <- read_csv("data/simd/00410767.csv")
diversity_H <- read_csv("data/derived/p_all_H.csv")

#View(simd)
simd_simple <- simd %>% 
    select(dz_2001 = `Data Zone`, simd_score = `Overall SIMD 2012 Score`)

simd_h_dist <- diversity_H %>% 
    gather(key = "category", value = "H", tenure:land_bus) %>% 
    left_join(simd_simple, by = c("datazone" = "dz_2001")) %>% 
    select(datazone, period, simd_score, category, H)   

# % change from 2001 to 2011 by simd quintile

# As a table
simd_h_dist %>% 
    filter(category != "land_bus") %>% 
    mutate(simd_quintile = ntile(simd_score, 5)) %>% 
    select(simd_quintile, period, category, H) %>% 
    group_by(simd_quintile, period, category) %>% 
    summarise(H = median(H, na.rm = T)) %>% 
    ungroup %>% 
    spread(period, H) %>% 
    mutate(per_change = (t2 - t1) / t1) %>% 
    select(-t1, -t2) %>%
    mutate(per_change = round(per_change, 3)) %>% 
    spread(simd_quintile, per_change) 


# As figures 
simd_h_dist %>%
    filter(category != "land_bus") %>% 
    mutate(simd_quintile = ntile(simd_score, 5)) %>% 
    select(simd_quintile, period, category, H) %>% 
    group_by(simd_quintile, period, category) %>% 
    summarise(H = median(H, na.rm = T)) %>% 
    ungroup %>% 
    spread(period, H) %>% 
    mutate(per_change = (t2 - t1) / t1) %>% 
    select(-t1, -t2) %>%
    ggplot(.) + 
    geom_line(aes(x = simd_quintile, y = per_change, group = category, linetype = category, colour = category))

simd_h_dist %>%
    filter(category != "land_bus") %>% 
    mutate(simd_quintile = ntile(simd_score, 5)) %>% 
    select(simd_quintile, period, category, H) %>% 
    group_by(simd_quintile, period, category) %>% 
    summarise(H = median(H, na.rm = T)) %>% 
    ungroup %>% 
    spread(period, H) %>% 
    mutate(per_change = 100 * (t2 - t1) / t1) %>% 
    select(-t1, -t2) %>%
    ggplot(., 
           mapping = aes(
               x = simd_quintile, y = per_change)
           ) + 
    geom_line() + geom_point() +
    geom_hline(yintercept = 0) + 
    facet_wrap(~category, scales = "free_y") + 
    labs(y = "percentage change in diversity from 2001 to 2011", 
         x = "SIMD Quintile (1 = most deprived)"
         )
ggsave(
    "figures/percent_change_in_diversity_by_simd_quintile.png", 
    height = 20, width = 20, units = "cm")
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

