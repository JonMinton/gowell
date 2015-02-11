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
require(ggtern)



# Linking chps to dzs -----------------------------------------------------


# # find dzs referred to
# 
# chps_of_interest <- read.csv("data/geographies/greater_glasgow_definitions_simplified.csv") %>% tbl_df()
# chps_of_interest <- chps_of_interest %>% slice(1:12)
# 
# chps_to_dzs <- read.csv("data/geographies/latestpcinfowithlinkpc.csv") %>% tbl_df()
# chps_to_dzs <- chps_to_dzs %>% 
#     select(dz_2001=Datazone, chp=CHP) %>% 
#     distinct(dz_2001)
# 
# # n.b. need the 2001 not 2011 dz codes 
# chps_of_interest <- chps_of_interest %>% rename(chp=chcp_code) 
# 
# dzs_in_greater_glasgow <- chps_to_dzs %>% 
#     inner_join(chps_of_interest) %>%
#     select(-chcp_name)
# 
# write.csv(dzs_in_greater_glasgow, file="data/geographies/dzs_in_greater_glasgow.csv", row.names=F)
# # 

# Main Analysis -----------------------------------------------------------

greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% tbl_df()

#Tenure

tenure_households <- read.csv(
    "data/sns/tenure_households.csv"
    ) %>% tbl_df() %>% select(
    dz_2001=datazone, year, 
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

tenure_households <- greater_glasgow_dzs %>% left_join(tenure_households)
tenure_households <- tenure_households %>% mutate(
    mix=(social * rented * owned) / (1/3)^3)
    # this should be the maximum possible mix value

tenure_households %>% group_by(year) %>% summarise()
# unfortunately this is only available for 2001

# 
qplot(
    x=mix, data=tenure_households
    )

# a lot of excess 0s - no mix

# arrange households by mix, then plot proportions of each tenure type along this linke

tenure_households <- tenure_households %>% arrange(mix)

ggplot(tenure_households) + geom_line(aes(x=mix, y=social))
ggplot(tenure_households) + geom_line(aes(x=mix, y=owned))
ggplot(tenure_households) + geom_line(aes(x=mix, y=rented))

# want social, owned, and rented to be gathered 

tmp <- tenure_households %>% 
    select(dz_2001, mix, social, owned, rented) %>%
    gather(key = tenure_type, value=tenure_proportion, -dz_2001, -mix) 

ggplot(tmp, aes(x=mix, y=tenure_proportion)) + 
    geom_line() + facet_grid(tenure_type ~ .)

# ggplot(tmp, aes(x=mix, y=tenure_proportion)) + 
#     geom_line(aes(group=tenure_type, colour=tenure_type)) # this crashes rstudio in the office -try at home?




ggplot(tmp, aes(x=mix, y=tenure_proportion)) + 
    geom_line() + facet_grid(. ~ tenure_type)


ggtern(data=tenure_households, mapping=aes(x=social, y=rented, z=owned)) +geom_point()

# mix and dependancy ratios

populations <- read.csv(
    "data/sns/persons_by_gender_year_and_age.csv"
    ) %>% tbl_df() %>% rename(
    dz_2001=datazone
    )

# let's estimate dependency ratios

prop_working_age <- populations %>% 
    mutate(working_age = ifelse(upper_age >=16 & lower_age <=60, 1, 0)) %>%
    group_by(dz_2001, year, gender) %>% 
    summarise(proportion_working_age=sum(count[working_age==1])/sum(count))

# write.csv(prop_working_age, file="data/derived/prop_working_age.csv", row.names=F)

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
