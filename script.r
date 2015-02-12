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
require(corrplot)


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

tenure_households <- source_DropboxData(
    file="tenure_households.csv",
    key="kng5wc40le9kapj"
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

# 6505 observations - whole of Scotland
# left join to just Greater Glasgow
tenure_households <- greater_glasgow_dzs %>% left_join(tenure_households)
# now 2200 observations
# this is 34% of total, dzs are approx equal population
# if Scot population is 5.3 million this implies 
# Pop of Greater Glasgow is about 1.8 Million - 
# does this seem reasonable?


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

tenure_households %>% 
    select(dz_2001, mix, social, owned, rented) %>%
    gather(key = tenure_type, value=tenure_proportion, -dz_2001, -mix) %>%
    ggplot( aes(x=mix, y=tenure_proportion)) +
    geom_line() + 
    facet_grid(tenure_type ~ . ) + 
    labs(y="proportion", x="mix")



# mix deciles 

tenure_deciles <- tenure_households %>%
    select(dz_2001, mix, social, owned, rented, all_households) %>%
    gather(key = tenure_type, value=tenure_proportion, -dz_2001, -mix, -all_households) %>%
    mutate(m10=ntile(mix, 10)) %>% 
    group_by(m10, tenure_type) %>% 
    summarise(
        tenure_mean=mean(tenure_proportion),
        tenure_sd=sd(tenure_proportion),
        n=sum(all_households)
    ) %>%
    mutate(
        ci = tenure_sd / n^(1/2),
        lower=tenure_mean - 2 * ci,
        upper=tenure_mean + 2 * ci
    ) 

tenure_deciles %>% ggplot(aes(
        x=factor(m10), group=tenure_type, 
        colour=tenure_type, y=tenure_mean
        )) +
    geom_line(
        ) +
    geom_pointrange(
        aes(
            ymax=upper,
            ymin=lower
            )
        ) + labs(
            y="mean proportion",
            x="Decile of mix\n(1=lowest)"
            )

# what's the correlation between the tenure types over the deciles?

tenure_deciles %>% 
    select(m10, tenure_type, tenure_mean) %>%
    spread(key=tenure_type, value=tenure_mean) %>% 
    select(-m10) %>% 
    cor() %>% 
    corrplot.mixed()
    



# mix and dependancy ratios

populations <- read.csv("data/sns/populations_by_age_year_sex.csv") %>% 
    tbl_df()

# reduce to just Greater Glasgow

populations <- populations %>% right_join(greater_glasgow_dzs)

# now 1.98 million
populations %>% 
    group_by(year, age_group) %>%
    summarise(
        cells=n(),
        na_count=length(which(is.na(count)))
        ) %>% 
    View()


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
