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


# NOTE: Not all are in the same format. Some additional prep will be required

calc_diversities <- function(dta, grp_name){
    dta %>% 
        group_by(dz_2001, year) %>% nest %>% 
        mutate(
            shannon = map_dbl(data, calc_H),
            simpson = map_dbl(data, calc_S),
            inv_simpson = 1 / simpson,
            group = grp_name
        ) %>% 
        select(
            dz_2001, year, group, shannon, simpson, inv_simpson
        ) 
}

tenure %>% calc_diversities("tenure") -> diversities_tenure

map2(
    list(
        tenure %>% select_(~-total), 
        bld_band, bld_size, bld_type, 
        demo_as, demo_eth, demo_rel, demo_coo, 
        econ_qual, econ_act, econ_sec, econ_ind, 
        land_vacant, land_bus), 
    list("tenure", 
         "building band", "bulding size", "building type", 
         "demo age structure", "demo ethnicity", "demo religion", "demo country of origin", 
         "econ qualification", "econ activity ", "econ socioeconomic", "econ industrial", 
         "land vacant", "land business"), 
    calc_diversities
    ) -> diversities_list

# Diversity using H -------------------------------------------------------


tenure$H <- H(as.matrix(tenure[,c("social", "rented", "owned")]))
tenure_H <- tenure %>%
    select(datazone=dz_2001, year=year, H=H)


bld_band$H <- H(as.matrix(bld_band[,c("A", "B", "C", "D", "E" ,"F", "G", "H")]))
bld_band_H <- bld_band %>%
    select(datazone, year, H)

bld_size <- bld_size %>%
    mutate(num_of_rooms = paste0("n_", num_of_rooms)) %>%
    spread(key=num_of_rooms, value=count)
bld_size$H <- H(as.matrix(bld_size[,-c(1,2)]))
bld_size_H <- bld_size %>%
    select(datazone, year, H)

bld_type <- bld_type  %>%
    spread(key=type, value=count) 
bld_type$H <- H(as.matrix(bld_type[,-c(1,2)]))
bld_type_H <- bld_type %>%
    select(datazone, year, H)

demo_as$H <- H(as.matrix(demo_as[,-c(1,2)]))
demo_as_H <- demo_as %>%
    select(datazone=dz_2001, year, H)


demo_eth$H <- H(as.matrix(demo_eth[,-c(1,2)]))
demo_eth_H <- demo_eth %>%
    select(datazone, year, H)

demo_rel$H <- H(as.matrix(demo_rel[,-c(1,2)]))
demo_rel_H <- demo_rel %>%
    select(datazone, year, H)

demo_coo$H <- H(as.matrix(demo_coo[,-c(1,2)]))
demo_coo_H <- demo_coo %>%
    select(datazone, year, H)


econ_qual$H <- H(as.matrix(econ_qual[,-c(1,2)]))
econ_qual_H <- econ_qual %>%
    select(datazone, year, H)

econ_act$H <- H(as.matrix(econ_act[,-c(1,2)]))
econ_act_H <- econ_act %>%
    select(datazone, year, H)

econ_sec$H <- H(as.matrix(econ_sec[,-c(1,2)]))
econ_sec_H <- econ_sec %>%
    select(datazone, year, H)

econ_ind$H <- H(as.matrix(econ_ind[,-c(1,2)])) 
econ_ind_H <- econ_ind %>%
    select(datazone, year, H)

land_vacant$H <- H(as.matrix(land_vacant[,-c(1,2)]))
land_vacant_H <- land_vacant %>%
    select(datazone, year, H)


land_bus$H <- H(as.matrix(land_bus[,-c(1,2)])) 
land_bus_H <- land_bus %>%
    select(datazone, year, H)

# Now to combine where there are common years

all_H <- tenure_H %>%
    rename(tenure=H) %>%
    full_join(bld_band_H) %>%
    rename(bld_band=H) %>%
    full_join(bld_size_H) %>%
    rename(bld_size=H) %>%
    full_join(bld_type_H) %>%
    rename(bld_type=H) %>%
    full_join(demo_as_H) %>%
    rename(demo_as=H) %>%
    full_join(demo_eth_H) %>%
    rename(demo_eth=H) %>%
    full_join(demo_rel_H)  %>% 
    rename(demo_rel=H) %>%
    full_join(demo_coo_H) %>%
    rename(demo_coo=H) %>%
    full_join(econ_qual_H) %>%
    rename(econ_qual=H) %>%
    full_join(econ_act_H) %>%
    rename(econ_act=H) %>%
    full_join(econ_sec_H) %>%
    rename(econ_sec=H) %>%
    full_join(econ_ind_H) %>%
    rename(econ_ind=H) %>%
    full_join(land_vacant_H) %>%
    rename(land_vacant=H) %>%
    full_join(land_bus_H) %>%
    rename(land_bus=H)



greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% 
    tbl_df() %>%
    rename(datazone=dz_2001)

all_H <- all_H %>%
    gather(key=type, value=H, -datazone, -year) %>%
    filter(!is.na(H)) %>%
    group_by(type) %>%
    mutate(t1 = min(year), t2=max(year)) %>%
    mutate(d1 = abs(year - t1), d2 = abs(year - t2)) %>%
    mutate(period = ifelse(d1==min(d1), "t1", ifelse(d2==min(d2), "t2", NA))) %>%
    filter(!is.na(period)) %>%
    select(datazone, year, period, type, H) %>%
    spread(key=type, value=H)

write.csv(all_H, file="data/derived/all_H.csv", row.names=F)

# H, Correlations in both periods --------------------------------------------


p_all_H <- all_H %>% 
    gather(key=type, value=H, -datazone, -year, -period)  %>% 
    filter(!is.na(H) & is.finite(H))  %>% 
    filter(datazone %in% greater_glasgow_dzs$datazone) %>% 
    select(-year)  %>% 
    spread(key=type, value=H)

write.csv(p_all_H, file="data/derived/p_all_H.csv", row.names=F)



# Do as above, but Shannon's diversity index not H ----------------------------------------

tenure$S <- diversity(as.matrix(tenure[,c("social", "rented", "owned")]))
tenure_S <- tenure %>%
    select(datazone=dz_2001, year=year, S=S)


bld_band$S <- bld_band  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

bld_band_S <- bld_band %>%
    select(datazone, year, S)

bld_size$S <- bld_size  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
    
bld_size_S <- bld_size %>%
    select(datazone, year, S)

bld_type$S <- bld_size  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

bld_type_S <- bld_type %>%
    select(datazone, year, S)

demo_as$S <- demo_as  %>% 
    select(-dz_2001, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
demo_as_S <- demo_as %>%
    select(datazone=dz_2001, year, S)


demo_eth$S <- demo_eth  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
demo_eth_S <- demo_eth %>%
    select(datazone, year, S)

demo_rel$S <- demo_rel  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

demo_rel_S <- demo_rel %>%
    select(datazone, year, S)

demo_coo$S <- demo_coo  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

demo_coo_S <- demo_coo %>%
    select(datazone, year, S)


econ_qual$S <- econ_qual  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
    
econ_qual_S <- econ_qual %>%
    select(datazone, year, S)

econ_act$S <- econ_act  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

econ_act_S <- econ_act %>%
    select(datazone, year, S)

econ_sec$S <- econ_sec  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

econ_sec_S <- econ_sec %>%
    select(datazone, year, S)

econ_ind$S <- econ_ind  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

econ_ind_S <- econ_ind %>%
    select(datazone, year, S)

land_vacant$S <- land_vacant  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
    
land_vacant_S <- land_vacant %>%
    select(datazone, year, S)


land_bus$S <- land_bus  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

land_bus_S <- land_bus %>%
    select(datazone, year, S)

# Now to combine where there are common years

all_S <- tenure_S %>%
    rename(tenure=S) %>%
    full_join(bld_band_S) %>%
    rename(bld_band=S) %>%
    full_join(bld_size_S) %>%
    rename(bld_size=S) %>%
    full_join(bld_type_S) %>%
    rename(bld_type=S) %>%
    full_join(demo_as_S) %>%
    rename(demo_as=S) %>%
    full_join(demo_eth_S) %>%
    rename(demo_eth=S) %>%
    full_join(demo_rel_S)  %>% 
    rename(demo_rel=S) %>%
    full_join(demo_coo_S) %>%
    rename(demo_coo=S) %>%
    full_join(econ_qual_S) %>%
    rename(econ_qual=S) %>%
    full_join(econ_act_S) %>%
    rename(econ_act=S) %>%
    full_join(econ_sec_S) %>%
    rename(econ_sec=S) %>%
    full_join(econ_ind_S) %>%
    rename(econ_ind=S) %>%
    full_join(land_vacant_S) %>%
    rename(land_vacant=S) %>%
    full_join(land_bus_S) %>%
    rename(land_bus=S)



greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% 
    tbl_df() %>%
    rename(datazone=dz_2001)

all_S <- all_S %>%
    gather(key=type, value=S, -datazone, -year) %>%
    filter(!is.na(S) & !is.na(year)) %>%
    group_by(type) %>%
    mutate(t1 = min(year), t2=max(year)) %>%
    mutate(d1 = abs(year - t1), d2 = abs(year - t2)) %>%
    mutate(period = ifelse(d1==min(d1), "t1", ifelse(d2==min(d2), "t2", NA))) %>%
    filter(!is.na(period)) %>%
    select(datazone, year, period, type, S) %>%
    spread(key=type, value=S)

write.csv(all_S, file="data/derived/all_S.csv", row.names=F)


# S, Correlations in both periods --------------------------------------------


p_all_S <- all_S %>% 
    gather(key=type, value=S, -datazone, -year, -period)  %>% 
    filter(!is.na(S) & is.finite(S))  %>% 
    filter(datazone %in% greater_glasgow_dzs$datazone) %>% 
    select(-year)  %>% 
    spread(key=type, value=S)

write.csv(p_all_S, file="data/derived/p_all_S.csv", row.names=F)


