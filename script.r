# First script
# 11/2/2015


# To do : 
# 1) Send Poverty & Suburbs work to Ade
# 2) reply to emails
# 3) extract list of dzs as per Ellie's suggestion [ done]
# 4) add packrat to see if this solves markdown issue [done - seems to ]
# 5) add existing script and adapt where relevant



rm(list=ls())


require(repmis)
require(plyr)
require(tidyr)
require(dplyr)
require(rmarkdown)
require(ggplot2)



# Linking chps to dzs -----------------------------------------------------


# find dzs referred to

# chps_of_interest <- read.csv("data/geographies/greater_glasgow_definitions_simplified.csv") %>% tbl_df()
# chps_of_interest <- chps_of_interest %>% slice(1:11)
# 
# chps_to_dzs <- read.csv("data/geographies/2011_dz_to_other_lookup.csv") %>% tbl_df()
# chps_to_dzs <- chps_to_dzs %>% select(datazone=DataZone, chp=CHP) 
# 
# chps_of_interest <- chps_of_interest %>% rename(chp=chcp_code) 
# 
# dzs_in_greater_glasgow <- chps_to_dzs %>% 
#     inner_join(chps_of_interest) %>%
#     select(-chcp_name)
# 
# write.csv(dzs_in_greater_glasgow, file="data/geographies/dzs_in_greater_glasgow.csv", row.names=F)


# Main Analysis -----------------------------------------------------------

greater_glasgow_dzs <- read.csv("data/geographies//dzs_in_greater_glasgow.csv") %>% tbl_df()

#Tenure

tenure_households <- source_DropboxData(
    file="tenure_households.csv",
    key="kng5wc40le9kapj"    
) %>% tbl_df() %>% select(
    datazone, year, 
    all_households=HO.allhouseholds,
    council_houses=HO.council,
    rented_from_employer=HO.employ,
    owned_with_mortgage=HO.ownmortloan,
    owned_outright=HO.ownoutright,
    private_rented=HO.privlet,
    rented_from_relative=HO.relative,
    shared_ownership=HO.sharedown,
    other_social_rented=HO.social
) %>% 
    mutate(
        social=council_houses + other_social_rented,
        rented=rented_from_employer + private_rented+ rented_from_relative,
        owned=owned_with_mortgage + owned_outright + shared_ownership
    ) %>%
    mutate(
        council_houses=council_houses/all_households,
        rented_from_employer=rented_from_employer/all_households,
        owned_with_mortgage=owned_with_mortgage/all_households,
        owned_outright=owned_outright/all_households,
        private_rented=private_rented/all_households,
        rented_from_relative=rented_from_relative/all_households,
        shared_ownership=shared_ownership/all_households,
        other_social_rented=other_social_rented/all_households,
        social = social/all_households,
        rented = rented/all_households,
        owned=owned/all_households
    )


# then to pollution


# If pop weighted we should also include population counts

populations <- source_DropboxData(
    file="persons.csv",
    key="vcz7qngb44vbynq"
) %>% tbl_df() %>% select(
    datazone, year,
    GR.hspeop,
    GR.sapepeop
)

populations <- populations %>% mutate(
    population_count=ifelse(is.na(GR.sapepeop), GR.hspeop, GR.sapepeop)
) %>% select(datazone, year, population_count)



##############################################################################################################
# To do now:


mod_01 <- lm(
    pm10 ~ inc_deprivation,
    data=joined)
summary(mod_01)

mod_02 <- mod_01 %>% update(
    . ~ . + social
)
summary(mod_02)

mod_03 <- lm(pm10 ~ inc_deprivation * social, 
             data=joined)
summary(mod_03)

mod_social <- lm(
    pm10 ~ inc_deprivation * social, 
    data=joined
)
summary(mod_social)

mod_rental <- lm(
    pm10 ~ inc_deprivation * rented,
    data=joined
)
summary(mod_rental)

mod_owner <- lm(
    pm10 ~ inc_deprivation * owned,
    data=joined
)
summary(mod_owner)

joined$social_quartile <- joined$social %>% ntile(4)
qplot(
    y=pm10,
    x=inc_deprivation,
    colour=social,
    data=joined
) + scale_colour_gradient(limits=c(0,1)) + 
    geom_smooth(data=subset(joined, subset=social_quartile==4), method="lm")


# now to normalise deprivation levels on a 0-1 scale

j2 <- joined %>% mutate(inc_deprivation=(inc_deprivation - min(inc_deprivation))/(max(inc_deprivation)-min(inc_deprivation)))

# This *might* be helpful for understanding the relative importance of the interaction
# between household type proportions and income deprivation

norm_social <- lm(
    pm10 ~ inc_deprivation * social, 
    data=j2
)
summary(norm_social)

norm_rental <- lm(
    pm10 ~ inc_deprivation * rented,
    data=j2
)
summary(norm_rental)

norm_owner <- lm(
    pm10 ~ inc_deprivation * owned,
    data=j2
)
summary(norm_owner)
###################################################################################

# As a crude measure of mix, let's use the produce of the three proportions 
# of household type, then normalised to a 0-1 scale

j3 <- joined %>% mutate(mix=owned *rented * social) %>% mutate(mix =mix/max(mix))

qplot(x=mix, data=j3)

qplot(x=inc_deprivation, y=mix, data=j3) + stat_smooth()
qplot(y=inc_deprivation, x=mix, data=j3) + stat_smooth()


mix_social <- lm(
    pm10 ~ inc_deprivation * social *mix, 
    data=j3
)
summary(mix_social)

mix_rental <- lm(
    pm10 ~ inc_deprivation * rented * mix,
    data=j3
)
summary(mix_rental)

mix_owner <- lm(
    pm10 ~ inc_deprivation * owned * mix,
    data=j3
)
summary(mix_owner)

# Renove 3 way interaction to make interpretation simpler
summary(lm(pm10 ~ inc_deprivation*social + mix, data=j3))
summary(lm(pm10 ~ inc_deprivation*owned + mix, data=j3))
summary(lm(pm10 ~ inc_deprivation*rented + mix, data=j3))


### One additional thing: what if we encode proportion social housing as a colour with 
# y: pm10 
# x: income deprivation

joined <- income_deprivation %>% inner_join(tenure_households) %>% inner_join(pollution) 
