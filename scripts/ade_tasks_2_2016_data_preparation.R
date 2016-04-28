rm(list = ls())



require(readr)
require(stringr)

require(plyr)
require(tidyr)
require(dplyr)

require(ggplot2)
require(tmap)
require(rgeos)


source("scripts/helper_functions.R") # Contains join_attribute_table function
# Initial tasks: 

# 1) Calculate density for each DZ in GG
# 2) Calculate distance to city centre for each DZ in GG
# 3) Extract and attach urban-rural classification to each DZ in GG 


# Task 1: calculate density -----------------------------------------------

# Using tmap::read_shape

scotland_dz <- read_shape("data/shp/scotland_2001_datazones/scotland_dz_2001.shp")

# This works now.
# First to try to plot this 

#scotland_dz %>% qtm()
# This works, bit takes a little while

# Now to attach population counts to each datazone 

# To do this, first load a census 2001 dataset containing population counts

# Using using country of birth table from scotland dissim tables repo


cob_2001 <- read_csv("data/prepared_census/cob_2001.csv")

pop_counts <- cob_2001 %>%
    select(dz_2001, total)
# 
rm(cob_2001)

scotland_dz <- join_attribute_table(scotland_dz, pop_counts, "zonecode", "dz_2001")

# This works. Now to extract density estimates 

tmp <- calc_densities(scotland_dz, var = "total")
dim(pop_counts)

dzs <- scotland_dz$zonecode

zonecode_density <- data_frame(
    dz_2001 = as.character(dzs), 
    population_density = tmp
    )

# not all datazones comprise of just one polygon (islands etc), so for those dzs comprising more
# than one density estimate, calculate a geometric mean (as density is inherently geometric)

zonecode_density %>% 
    group_by(dz_2001) %>% 
    summarise(population_density = prod(population_density)^(1 / length(population_density)))

# There are now 6500 distinct dzs as expected. 

# now to save this table for future use 

write_csv(zonecode_density, path = "data/derived/population_density_by_2001_dz.csv")



# Task 2: calculate distance to city centre for each DZ in GG -------------

# For consistency, use the same central point as in scotland_dissim_tables

# For Glasgow this was 
# Glasgow
#  - West End of George Square
# G1 3BU
# S01003358

glasgow_central_dz <- "S01003358"

# using rgeos::gCentroid
centroids <- gCentroid(scotland_dz, byid = T)
centroids <- as(centroids, "data.frame")
centroids <- data_frame(dz_2001 = as.character(scotland_dz@data$zonecode), x =centroids$x, y = centroids$y)

centroids <- centroids %>% 
    mutate(centre = dz_2001 == glasgow_central_dz)

centroids <- centroids %>% 
    mutate(distance_to_centre = ((x - x[centre])^2 + (y - y[centre])^2)^0.5)

# Explore the data to sense check
qplot(data = centroids, x =x, y = y, colour = distance_to_centre) + scale_colour_gradient2()
qplot(data = centroids, x = distance_to_centre, binwidth = 200)

# again, for areas with more than one polygon, calculate a geometric average

centroids <- centroids %>% 
    group_by(dz_2001) %>% 
    summarise(
        distance_to_centre = prod(distance_to_centre) ^ (1 / length(distance_to_centre))
        ) 

# Now to save this for further use 

write_csv(centroids, path = "data/derived/dz_2001_by_distance_from_gg_centre.csv")



# Task 3: find urban rural class for each DZ ----- (The easy task) -------------------------

# First, let's see if it's already available 

bigfile <- read_csv("data/geographies/latestpcinfowithlinkpc.csv")

small_table <- bigfile %>% 
    select(dz_2001 = Datazone, ur_class = UrbRur6_2011_2012)


# quick exploration of number of non-unique codes for dzs
small_table  %>% 
    group_by(dz_2001)  %>% 
    summarise(
        n = length(ur_class), 
        n_distinct = length(unique(ur_class)), 
        ur_class = median(ur_class, na.rm =T) # later using mode as (say) UR class 3.5 not helpful for cat vars
        )  %>%   
    filter(!is.na(dz_2001))  %>%  
    ggplot(.) + 
    geom_point(
        aes(x = n, y = jitter(n_distinct)), 
        alpha = 0.2
        )


small_table  %>% 
    group_by(dz_2001)  %>% 
    summarise(
        n = length(ur_class), 
        n_distinct = length(unique(ur_class)), 
        ur_class = median(ur_class, na.rm =T)
    )  %>%   
    filter(!is.na(dz_2001))  %>% ungroup %>% 
    group_by(n_distinct) %>% 
    summarise(areas = sum(n)) %>% 
    mutate(
        cumulative_areas = cumsum(areas), 
        cprop = round(cumulative_areas / max(cumulative_areas), 2)
        )

# 82% of areas have one unique urban rural category, 98% of areas have one or two categories

# Because each dz comprises tens or hundreds of smaller areas, and in almost all cases 
# there are just two categories to choose between, the modal approach should be OK 

dz_ur <- small_table  %>% 
    group_by(dz_2001)  %>% 
    summarise(
        ur_class = calc_mode(ur_class, na.rm =T)
    ) %>% 
    filter(!is.na(dz_2001))


# Now, to write this out like the others 

write_csv(dz_ur, path = "data/derived/dz_2001_by_ur_6fold_class.csv")

# End of data preparation tasks


