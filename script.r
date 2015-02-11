# First script
# 11/2/2015


# To do : 
# 1) Send Poverty & Suburbs work to Ade
# 2) reply to emails
# 3) extract list of dzs as per Ellie's suggestion
# 4) add packrat to see if this solves markdown issue

rm(list=ls())


require(repmis)
#require(plyr)
require(tidyr)
require(dplyr)
require(rmarkdown)


# find dzs referred to

chps_of_interest <- read.csv("data/geographies/greater_glasgow_definitions_simplified.csv") %>% tbl_df()
chps_of_interest <- chps_of_interest %>% slice(1:11)

chps_to_dzs <- read.csv("data/geographies/2011_dz_to_other_lookup.csv") %>% tbl_df()
chps_to_dzs <- chps_to_dzs %>% select(datazone=DataZone, chp=CHP) 

chps_of_interest <- chps_of_interest %>% rename(chp=chcp_code) 

dzs_in_greater_glasgow <- chps_to_dzs %>% 
    inner_join(chps_of_interest) %>%
    select(-chcp_name)

write.csv(dzs_in_greater_glasgow, file="data/geographies/dzs_in_greater_glasgow.csv", row.names=F)
Hi both,