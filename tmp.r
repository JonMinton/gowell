rm(list=ls())
#pre-reqs
require(stringr)
require(tidyr)
require(dplyr)
require(repmis)
require(vegan)
require(ggplot2)

# d

dwell_sizes <- read.csv("data/derived/dwellings_by_size.csv") %>%  tbl_df
greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv")  %>% tbl_df 
tenure <- read.csv("data/derived/tenure_by_dz.csv")  %>% tbl_df# note this is glasgow only 

tenure <- tenure %>%
    select(datazone=dz_2001, all_households, social, rented, owned)

tenure <- tenure %>%
    mutate(
        social_tertile=ntile(social, 3),
        social=social * all_households, 
        rented=rented * all_households, 
        owned=owned * all_households
    ) %>%
    select(-all_households)

tenure$diversity <- diversity(
    as.matrix(tenure[,2:4])
)

tenure <- tenure %>%
    mutate(diversity_tertile=ntile(diversity, 3))

dwell_sizes <- dwell_sizes %>%
    inner_join(greater_glasgow_dzs, by=c("datazone"="dz_2001")) %>%
    select(-chp) %>%
    filter(year==2006)

combined <- tenure %>%
    inner_join(dwell_sizes)


ggplot(data=combined) + 
    facet_grid(social_tertile ~ diversity_tertile) + 
    geom_boxplot(aes(x=factor(num_of_rooms), y=count)) + 
    labs(x="number of rooms", y="count")
