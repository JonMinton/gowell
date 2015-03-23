rm(list=ls())

require(stringr)
require(plyr)
require(repmis)
require(tidyr)
require(dplyr)


# Linking chps to dzs -----------------------------------------------------


# # find dzs referred to
# 
chps_of_interest <- read.csv("data/geographies/greater_glasgow_definitions_simplified.csv") %>% tbl_df()
chps_of_interest <- chps_of_interest %>% slice(1:12)

chps_to_dzs <- read.csv("data/geographies/latestpcinfowithlinkpc.csv") %>% tbl_df()
chps_to_dzs <- chps_to_dzs %>% 
    select(dz_2001=Datazone, chp=CHP) %>% 
    distinct(dz_2001)

# n.b. need the 2001 not 2011 dz codes 
chps_of_interest <- chps_of_interest %>% rename(chp=chcp_code) 

dzs_in_greater_glasgow <- chps_to_dzs %>% 
    inner_join(chps_of_interest) %>%
    select(-chcp_name)

write.csv(dzs_in_greater_glasgow, file="data/geographies/dzs_in_greater_glasgow.csv", row.names=F)
# # 



# Want to do the same for igs

chps_of_interest <- read.csv("data/geographies/greater_glasgow_definitions_simplified.csv") %>% 
    tbl_df() %>%
    rename(chp=chcp_code)

chps_to_igs <- read.csv("data/geographies/latestpcinfowithlinkpc.csv") %>% tbl_df()
chps_to_igs <- chps_to_igs %>% 
    select(ig_2001=Intermed, chp=CHP) %>% 
    distinct(ig_2001) %>%
    filter(str_detect(ig_2001, "^S02"))


igs_in_gg <- chps_of_interest  %>% 
    inner_join(chps_to_igs)

write.csv(igs_in_gg, file="data/geographies/igs_in_greater_glasgow.csv", row.names=FALSE)