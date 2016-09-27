# A script to calculate diversity scores according to a range of dimensions 
# using SNS and 2011 census data


rm(list=ls())

pacman::p_load(
    readr,
    tidyr, stringr,
    dplyr, 
    purrr,
    ggplot2
)


# Entropy function --------------------------------------------------------

# Shannon's Diversity Score (0 to - infinity)
calc_H <- function(df, correction = 0){
    vec <- as.vector(df) 
    vec <- vec + correction
    - sum(vec * log(vec))
}
#

# Simpson's diversity index
# from 
# http://geographyfieldwork.com/SimpsonsDiversityIndex.htm
calc_S <- function(df, correction = 0){
    vec <- as.vector(df) 
    vec <- vec + correction
    N <-  sum(vec)
    v2 <- (vec * (vec - 1)) / (N * (N - 1))
    1 - sum(v2)
}

# Now to reformulate the above 


# Data  -------------------------------------------------------------------


# Main Analysis -----------------------------------------------------------
greater_glasgow_dzs <- read_csv("data/geographies/dzs_in_greater_glasgow.csv") 


# Primary
tenure <- read_csv("data/derived/tenure_by_dz.csv") 


# Building type 

bld_band <- read_csv("data/derived/dwellings_by_band.csv") 
bld_size <- read_csv("data/derived/dwellings_by_size.csv") 
bld_type <- read_csv("data/derived/dwellings_by_type.csv") 


# Demographic -------------------------------------------------------------

demo_as <- read_csv("data/derived/demographic_groupings.csv") 
demo_eth <- read_csv("data/derived/ethnicity.csv") 
demo_rel <- read_csv("data/derived/rel.csv") 
demo_coo <- read_csv("data/derived/coo.csv") 


# Economy -------------------------------------------------------------

econ_qual <- read_csv("data/derived/highest_qual.csv") 
econ_act <- read_csv("data/derived/economic_activity.csv") 
econ_sec <- read_csv("data/derived/sec_by_dz.csv") 
econ_ind <- read_csv("data/derived/industry.csv") 


# Land use ----------------------------------------------------------------

land_vacant <- read_csv("data/derived/household_space_use.csv") 
land_bus <- read_csv("data/derived/building_use.csv") 



# standardise the above  --------------------------------------------------


tenure_tidy <- tenure %>% 
    rename(datazone = dz_2001) %>% 
    select(-total) %>% 
    mutate(category = "tenure") %>% 
    select(datazone, year, category, everything())

bld_band %>% mutate(category = "bld_band") %>% 
    select(datazone, year, category, everything()) -> bld_band_tidy



bld_size %>% 
    mutate(
        num_of_rooms = ifelse(
            num_of_rooms == 1, 
            paste(num_of_rooms, "room"), paste(num_of_rooms, "rooms"))
        ) %>% mutate(category = "bld_size") %>% 
    spread(num_of_rooms, count) %>% 
    select(datazone, year, category, everything()) -> bld_size_tidy


bld_type %>% 
    mutate(category = "bld_type") %>% 
    spread(type, count) %>% 
    select(datazone, year, category, everything()) -> bld_type_tidy


demo_rel %>% 
    mutate(category = "demo_rel") %>% 
    select(datazone, year, category, everything()) -> demo_rel_tidy

demo_eth %>% 
    mutate(category = "demo_eth") %>% 
    select(datazone, year, category, everything()) -> demo_eth_tidy


demo_as %>% 
    rename(datazone = dz_2001) %>% 
    mutate(category = "demo_as") %>% 
    select(datazone, year, category, everything()) -> demo_as_tidy


demo_coo %>% .[,-1] %>% 
    mutate(category = "demo_coo") %>% 
    select(datazone, year, category, everything()) -> demo_coo_tidy


econ_qual %>% 
    mutate(category = "econ_qual") %>% 
    select(datazone, year, category, everything()) -> econ_qual_tidy

econ_act %>% 
    select(-matches("unemployed_")) %>% 
    select(-all_people_16_74) %>% 
    mutate(category = "econ_act") %>% 
    select(datazone, year, category, everything()) -> econ_act_tidy

econ_sec %>% 
    .[,-1] %>% 
    mutate(category = "econ_sec") %>% 
    select(datazone, year, category, everything()) -> econ_sec_tidy


econ_ind %>% 
    mutate(category = "econ_ind") %>% 
    select(datazone, year, category, everything()) -> econ_ind_tidy

land_vacant %>% 
    mutate(category = "land_vacant") %>% 
    select(datazone, year, category, everything()) -> land_vacant_tidy

 
land_bus %>% 
    mutate(category = "land_bus") %>% 
    select(datazone, year, category, everything()) -> land_bus_tidy




# NOTE: Not all are in the same format. Some additional prep will be required

calc_diversities <- function(dta){
    dta %>% 
        group_by(datazone, year, category) %>% 
        nest %>% 
        mutate(
            shannon = map_dbl(data, calc_H),
            simpson = map_dbl(data, calc_S),
            inv_simpson = 1 / simpson
            ) %>% 
        select(
            datazone, year, category, shannon, simpson, inv_simpson
        ) 
}

tenure_tidy %>% calc_diversities() -> diversities_tenure
bld_band_tidy %>% calc_diversities() -> diversities_bld_band
bld_size_tidy %>% calc_diversities() -> diversities_bld_size
bld_type_tidy %>% calc_diversities() -> diversities_bld_type

demo_rel_tidy %>% calc_diversities() -> diversities_demo_rel
demo_eth_tidy %>% calc_diversities() -> diversities_demo_eth
demo_as_tidy %>% calc_diversities() -> diversities_demo_as
demo_coo_tidy %>% calc_diversities() -> diversities_demo_coo

econ_qual_tidy %>% calc_diversities() -> diversities_econ_qual
econ_act_tidy %>% calc_diversities() -> diversities_econ_act
econ_sec_tidy %>% calc_diversities() -> diversities_econ_sec
econ_ind_tidy %>% calc_diversities() -> diversities_econ_ind

land_vacant_tidy %>% calc_diversities() -> diversities_land_vacant
land_bus_tidy %>% calc_diversities() -> diversities_land_bus


diversities_all <- reduce(
    .x = list(
        diversities_tenure,
        diversities_bld_band,
        diversities_bld_size,
        diversities_bld_type,
        diversities_demo_rel,
        diversities_demo_eth,
        diversities_demo_as,
        diversities_demo_coo,
        diversities_econ_qual,
        diversities_econ_act,
        diversities_econ_sec,
        diversities_econ_ind,
        diversities_land_vacant,
        diversities_land_bus
    ),
    
    .f = bind_rows
                          )

greater_glasgow_dzs <- read_csv("data/geographies/dzs_in_greater_glasgow.csv") %>% 
    rename(datazone=dz_2001)

diversities_all  %>% 
    inner_join(greater_glasgow_dzs)  %>% 
    select(-chp) -> diversities_all 

write_csv(diversities_all, path = "data/derived/all_diversities.csv")
