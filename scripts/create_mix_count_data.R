# file containing script for producing 'cleaned' variables on mix

rm(list=ls())

require(rmarkdown)
require(repmis)
require(stringr)
require(plyr)
require(dplyr)
require(tidyr)

require(ggplot2)
require(ggtern)
require(corrplot)

require(vegan)




# Main Analysis -----------------------------------------------------------

greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% tbl_df()


# Tenure diversity 2001  --------------------------------------------------

#Tenure - 2001 census via SNS


tenure_households <- source_DropboxData(
    file="tenure_households.csv",
    key="6t6dss41g8fat1y"
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
) %>% mutate(
    social=council_houses + other_social_rented,
    rented=rented_from_employer + private_rented+ rented_from_relative,
    owned=owned_with_mortgage + owned_outright + shared_ownership
) 

# 6505 observations - whole of Scotland
# left join to just Greater Glasgow
tenure_households_2001 <- greater_glasgow_dzs %>% left_join(tenure_households)
# now 2200 observations
# this is 34% of total, dzs are approx equal population
# if Scot population is 5.3 million this implies 
# Pop of Greater Glasgow is about 1.8 Million - 
# does this seem reasonable?





# Tenure diversity - 2011 Census (directly) -------------------------------


tenure_households_2011 <- source_DropboxData(
    file="LC4427SC.csv",
    key="8ah8pn0wymz0to8"
) %>% tbl_df()


tenure_households_2011 <- tenure_households_2011 %>%
    rename(datazone=V1, building_type=V2)

# remove S92 areas
tenure_households_2011 <- tenure_households_2011 %>%
    filter(str_detect(datazone, pattern="^S01")) %>%
    filter(str_detect(building_type, "All households")) %>%
    select(-building_type)

# Gather and remove "Owned: Total" and "Rented or living rent free: Total"

tenure_households_2011 <- tenure_households_2011 %>%
    gather(key=hh_type, value=count, -datazone, -2) %>%
    filter(!str_detect(hh_type, "Total$"))

# now to check that the vals are all OK
tmp <- tenure_households_2011 %>%
    group_by(datazone) %>%
    summarise( all_households=mean(`All households`), all_2 = sum(count))

all(tmp$all_households ==tmp$all_2) # TRUE

# now to collapse to the categories used before, 
# social
# owned
# rented

tenure_households_2011 <-tenure_households_2011 %>%
    spread(key=hh_type, value=count) %>%
    transmute(
        dz_2001 = datazone,
        year = 2011L,
        total = `All households`,
        owned = `Owned: Owned outright` + `Owned: Owned with a mortgage or loan or shared ownership`,
        social = `Rented or living rent free: Social rented`,
        rented = `Rented or living rent free: Private rented or living rent free`
        )

tenure_households_2011 <- greater_glasgow_dzs %>% 
    left_join(tenure_households_2011) %>%
    select(-chp)

# join both groups

tenure_households_2001 <-tenure_households_2001 %>%
    select(dz_2001, year, total=all_households, social, rented, owned)

tenure_households <- bind_rows(tenure_households_2001, tenure_households_2011)

# want to compare 2001 with 2011 dzs
tenure_households %>%
    mutate(social = social/total, rented = rented/total, owned=owned/total) %>%
    gather(key=tentype, value=count, -dz_2001, -year, -total) %>%
    qplot(x=count, col=tentype, fill=tentype, group=tentype, data=., facets=. ~ year) 
    
# Looks similar enough, now to save...
write.csv(tenure_households, file="data/derived/tenure_by_dz.csv", row.names=F)



# Demography, 2001 and 2011 -----------------------------------------------

# mix and dependancy ratios

populations <- read.csv("data/derived/populations_by_age_year_sex.csv") %>% 
    tbl_df()


# reduce to just Greater Glasgow
populations <- populations %>% rename(dz_2001=datazone)
populations <- populations %>% right_join(greater_glasgow_dzs)

populations <- populations %>%
    select(-chp) %>%
    filter(year %in% c(2001, 2011))

# What age/sex groups do I want?

# Using the categories developed in another script

# year groupings are different for different years


    
populations_grouped <- populations  %>% 
    unite(sa, sex, age_range)  %>% 
    select(dz_2001, year, sa, count) %>%
    group_by(year) %>%
    spread(sa, count) %>%
    mutate_each(funs(ifelse(is.na(.), 0, .)), -dz_2001, -year) %>%
    transmute(
        dz_2001 =dz_2001, year=year,
        f1 = female_0_4 + female_5_9 + female_10_14 + female_10_15,
        f2 = female_15_19 + female_16_19 + female_20_24,
        f3 = female_25_29 + female_30_34 + female_35_39,
        f4 = female_40_44 + female_45_49 + female_50_54,
        f5 = female_55_59 + female_60_64,
        f6 = female_65_69 + female_70_74 + female_75_79,
        f7 = female_80_84 + female_85_89 + female_90_101,
        
        m1 = male_0_4 + male_5_9 + male_10_14 + male_10_15,
        m2 = male_15_19 + male_16_19 + male_20_24,
        m3 = male_25_29 + male_30_34 + male_35_39,
        m4 = male_40_44 + male_45_49 + male_50_54,
        m5 = male_55_59 + male_60_64,
        m6 = male_65_69 + male_70_74 + male_75_79,
        m7 = male_80_84 + male_85_89 + male_90_101
    )
        
  # Populations : grouped
write.csv(populations_grouped, file="data/derived/demographic_groupings.csv", row.names=F)



# Dwelling type -----------------------------------------------------------

# from SNS - seems to be 2001


dwellings <- source_DropboxData(
    file="dwellings.csv", 
    key="1oqfsgpfzotxji4"
) %>% tbl_df


dwellings_bands <- dwellings %>%
    select(datazone, year, matches("_[A-Z]_")) %>%
    inner_join(greater_glasgow_dzs) %>%
    select(-chp)

names(dwellings_bands) <- names(dwellings_bands) %>% 
    str_replace_all("HO.Band_", "") %>% 
    str_replace_all("_dwellings", "")

dwelling_bands <- dwellings_bands %>%
    gather("band", "count", -datazone, -year)


dwellings_sizes <- dwellings %>%
    select(datazone, year, matches("HO.Rooms[0123456789]{1,2}"), HO.Roomsge10)

names(dwellings_sizes) <- names(dwellings_sizes) %>% 
    str_replace_all("HO.Rooms", "") %>% 
    str_replace_all("ge", "")

dwellings_sizes <- dwellings_sizes  %>% 
    gather(num_of_rooms, count, -datazone, -year) %>%    
    filter(!is.na(count))


# Dwellings_type
dwellings_types <- dwellings %>%
    select(datazone, year, HO.Flat, HO.Terraced, HO.Semidetached, HO.Detached)

names(dwellings_types) <- names(dwellings_types)  %>% str_replace_all("HO.", "") 

dwellings_types <- dwellings_types %>% 
    gather(type, count, -datazone, -year) %>%
    filter(!is.na(count))

dwellings_types$type <- dwellings_types$type %>% tolower


write.csv(dwellings_bands, file="data/derived/dwellings_by_band.csv", row.names=FALSE)
write.csv(dwellings_sizes, file="data/derived/dwellings_by_size.csv", row.names=FALSE)
write.csv(dwellings_types, file="data/derived/dwellings_by_type.csv", row.names=FALSE)


# dwelling - 2011 census --------------------------------------------------

dwellings_2011 <- source_DropboxData(
    file="QS401SC.csv",
    key="gj8m6w0ysrgxpzx"
    ) %>%
    tbl_df

dwellings_2011 <- dwellings_2011 %>%
    rename(datazone=V1)

dwellings_2011 <- dwellings_2011 %>%
    filter(str_detect(datazone, "^S01")) %>%
    mutate_each(funs(as.numeric(str_replace(., "-", "0")) ), -datazone) 

# [1] "datazone"                                                                    
# [2] "All people"                                                                  
# [3] "Unshared dwelling total"                                                     
# [4] "Unshared dwelling: Whole house or bungalow"                                  
# [5] "Unshared dwelling: Whole house or bungalow: Detached"                        
# [6] "Unshared dwelling: Whole house or bungalow: Semi-detached"                   
# [7] "Unshared dwelling: Whole house or bungalow: Terraced (including end terrace)"
# [8] "Unshared dwelling: Flat maisonette or apartment"                             
# [9] "Unshared dwelling: Purpose-built block of flats or tenement"                 
# [10] "Unshared dwelling: Part of a converted or shared house (including bed-sits)" 
# [11] "Unshared dwelling: In a commercial building"                                 
# [12] "Unshared dwelling: Caravan or other mobile or temporary structure"           
# [13] "Shared dwelling"   

numrooms_2011 <- source_DropboxData(
    file= "QS407SC.csv",
    key= "cbsuhjchyld2mtz"
    ) %>%
    tbl_df

numrooms_2011 <- numrooms_2011 %>%
    rename(datazone=V1)

numrooms_2011 <- numrooms_2011 %>%
    filter(str_detect(datazone, "^S01")) %>%
    mutate_each(funs(as.numeric(str_replace(., "-", "0")) ), -datazone) 
