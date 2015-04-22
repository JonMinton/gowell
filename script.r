# First script
# 11/2/2015


# To do : 
# 1) Send Poverty & Suburbs work to Ade
# 2) reply to emails
# 3) extract list of dzs as per Ellie's suggestion [ done]
# 4) add packrat to see if this solves markdown issue [done - seems to ]
# 5) add existing script and adapt where relevant

rm(list=ls())

require(rmarkdown)
require(repmis)
require(plyr)
require(dplyr)
require(tidyr)

require(ggplot2)
require(ggtern)
require(corrplot)

require(vegan)




# Main Analysis -----------------------------------------------------------

greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% tbl_df()






################################################################################################################


# Now to look at other kinds of mix

# mix types:

# Dwelling type mix

# dwellings, in by_year

require(plyr)
require(stringr)
require(tidyr)
require(dplyr)
require(ggplot2)
require(repmis)


# Diversity by housing sizes?

d_dwelling_types <- dwellings_types  %>% 
    spread(type, count)  %>% 
    select(-datazone, -year)  %>% 
    as.matrix %>%
    diversity

dwelling_types_wide <- dwellings_types %>%
    spread(type, count) %>%
    mutate(diversity=d_dwelling_types)
rm(d_dwelling_types)

# What has been the change in diversity from year to year?

div_by_year <- dwelling_types_wide %>%
    group_by(year) %>%
    summarise(
        q_025=quantile(diversity, 0.025),
        q_050=quantile(diversity, 0.050),
        q_100=quantile(diversity, 0.100),
        q_250=quantile(diversity, 0.250),
        q_500=quantile(diversity, 0.500),
        q_750=quantile(diversity, 0.750),
        q_900=quantile(diversity, 0.900),
        q_950=quantile(diversity, 0.950),
        q_975=quantile(diversity, 0.975)
    )


div_by_year %>% ggplot(aes(x=year, y=q_500)) +
    geom_line(size=1.1) +
    geom_ribbon(aes(ymin=q_025, ymax=q_975), alpha=0.2) +
    geom_ribbon(aes(ymin=q_050, ymax=q_950), alpha=0.2) +
    geom_ribbon(aes(ymin=q_100, ymax=q_900), alpha=0.2) +
    geom_ribbon(aes(ymin=q_250, ymax=q_750), alpha=0.2) +
    labs(x="Year", y="Diversity Level")


# Number of rooms by tenure diversity and proportion social_rented --------


rm(list=ls())
#pre-reqs
require(stringr)
require(tidyr)
require(dplyr)
require(repmis)
require(vegan)


# input_data

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


# This will produce a 3x3 tiled graph. Each row represents a different tertile of the 
# proportion social rented
# each column represents a different tertile of the tenure diversity score for that area
ggplot(data=combined) + 
    facet_grid(social_tertile ~ diversity_tertile) + 
    geom_boxplot(aes(x=factor(num_of_rooms), y=count)) + 
    labs(x="number of rooms", y="count")



