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

greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% 
    tbl_df()

area_unit_links <- read.csv("data/geographies/latestpcinfowithlinkpc.csv") %>%
    tbl_df() %>%
    select(Datazone, OA2001) # For 2001 Census
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

# from SNS - 


dwellings <- source_DropboxData(
    file="dwellings.csv", 
    key="1oqfsgpfzotxji4"
) %>% tbl_df


dwellings_bands <- dwellings %>%
    select(datazone, year, matches("_[A-Z]_")) %>%
    inner_join(greater_glasgow_dzs, by=c("datazone"="dz_2001")) %>%
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



# Occupational Mix - 2001 -------------------------------------------------

dta_sec_2001 <- read.csv("data/2001_census/KS012a.csv") %>%
    tbl_df

# Need to do linking from zone.code to datazone

dta_sec_2001 <- dta_sec_2001 %>%
    inner_join(area_unit_links, by=c("Zone.Code"= "OA2001")) %>%
    distinct() %>%
    select(-Zone.Code) %>%
    rename(datazone=Datazone) %>%
    group_by(datazone) %>%
    summarise_each(funs(sum)) %>%
    filter(str_detect(datazone, "^S01"))

# To make comparable with 2011 vars also need estimtes of total population sizes
# these will form the 'X' groups below. 

populations <- read.csv("data/derived/populations_by_age_year_sex.csv") %>% 
    tbl_df()
dta_sec_2001 <- populations  %>% 
    filter(year==2001)  %>% 
    spread(key=sex, value=count)  %>% 
    mutate(total = male + female)  %>% 
    filter(lower_age >=16 & upper_age <= 74)  %>% 
    group_by(datazone)  %>% 
    summarise(total_working_age=sum(total)) %>%
    ungroup %>%
    right_join(dta_sec_2001)

with(dta_sec_2001, all(total_working_age >= all_16_74_employed))

with(dta_sec_2001, total_working_age[total_working_age < all_16_74_employed] <- 
         all_16_74_employed[total_working_age < all_16_74_employed]
         )
# this suggests there are some errors
# HOwever, only two errors out of 6505 values. So, setting to 0



dta_sec_2001 <- dta_sec_2001 %>%
    mutate(year=2001) %>%
    transmute(
        datazone = datazone,
        year=year,
        total=total_working_age,
        I = managers_senior_officials + prof_occupations,
        II = associate_prof_technical_occ +  skilled_trades_occ + 
        personal_service + admin_secretarial + sales_customer_service,
        III = process_plant_machine_operatives,
        IV = elementary,
        X= total - (I + II + III + IV)
        ) %>%
    select(-total)

# X is 0 
# Occupational Mix - 2011 -------------------------------------------------

# This section will use relevant variables from the 2011 Census to identify the 
# mix of occupational classes in each datazone

# 

dta_sec <- source_DropboxData(
    key = "h4l5f34ktg7lxl6",
    file="KS611SC.csv"
) %>% tbl_df


dta_sec <- apply(dta_sec, 2, function(x) str_replace_all(x, ",", ""))
dta_sec <- apply(dta_sec, 2, function(x) str_replace_all(x, "-", "0"))
dta_sec[,-1] <- apply(dta_sec[,-1], 2, function(x) as.numeric(as.character(x)))
dta_sec <- data.frame(dta_sec) %>%
    tbl_df

names(dta_sec) <- c(
    "datazone", 
    "total_working_age",
    "I_higher_managerial",
    "Ii_higher_managerial_upper",
    "Iii_higher_managerial_lower",
    "II_lower_managerial",
    "III_intermediate",
    "III_small_employers",
    "III_lower_supervisory",
    "IV_semi_routine",
    "V_routine",
    "X_nonstudent_total",
    "X_nonstudent_neverworked",
    "X_nonstudent_ltunemployed",
    "X_student"    
)


dta_sec <- dta_sec %>%
    slice(-1) %>%
    gather(key="sec", value="count", -datazone) 

dta_sec$count <- as.numeric(as.character(dta_sec$count))


greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv")  %>% tbl_df 

dta_sec <- dta_sec %>%
    inner_join(greater_glasgow_dzs, by=c("datazone"="dz_2001")) %>%
    select(datazone, sec, count)


# What are the mutually exclusive groups?


tmp <- dta_sec %>%
    spread(sec, count) %>%    
    group_by(datazone) %>%
    mutate(
        total=total_working_age,
        I=I_higher_managerial, 
        II=II_lower_managerial + III_small_employers + III_lower_supervisory,
        III=IV_semi_routine + III_intermediate,
        IV=V_routine, 
        X=X_nonstudent_total + X_student
    ) %>%
    select(datazone, total, I, II, III, IV, X) %>%
    select(datazone, I, II, III, IV, X)

dta_sec_2011 <- tmp %>%
    mutate(year=2011) %>%
    select(datazone, year, I, II, III, IV, X)

dta_sec <- bind_rows(dta_sec_2001, dta_sec_2011)

dta_sec <- greater_glasgow_dzs  %>% 
    select(datazone=dz_2001)  %>% 
    inner_join(dta_sec) %>%
    arrange(year, datazone)


write.csv(dta_sec, file="data/derived/sec_by_dz.csv")



# Highest qualification  - 2001 -------------------------------------------

hq_2001 <- read.csv("data/2001_census/KS013.csv") %>%
    tbl_df


hq_2001 <- area_unit_links  %>% 
    rename(Zone.Code = OA2001)  %>% 
    left_join(hq_2001)  %>% 
    distinct %>%
    select(-Zone.Code)  %>% 
    rename(datazone=Datazone)  %>% 
    group_by(datazone)  %>% 
    summarise_each(funs(sum))  %>% 
    filter(str_detect(datazone, "^S01")) %>%
    mutate(year=2001) %>%
    gather(key=category, value=count, -datazone, -year) %>%
    select(datazone, year, category, count) %>%
    spread(key=category, value=count)


hq_2001 <- hq_2001 %>%
    gather(key=key, value=count, -datazone, -year) %>%
    mutate(key=mapvalues(key,
            from=c(
                "all_people_aged_16_74",                 
                "no_quals_or_quals_outwith_these_groups", 
                "highest_qual_level_1",
                "highest_qual_level_2",                 
                "highest_qual_level_3",                  
                "highest_qual_level_4",                   
                "aged_16_17",                            
                "aged_18_74",                             
                "in_employment",                          
                "unemployed",                            
                "economically_inactive"
                ),
            to=c(
                NA,                 
                "none", 
                "lvl_1",
                "lvl_2",                 
                "lvl_3",                  
                "lvl_4",                   
                NA,                            
                NA,                             
                NA,                          
                NA,                            
                NA
                )
            )
        ) %>%
    filter(!is.na(key)) %>%
    spread(key=key, value=count)





# Highest qualification - 2011 --------------------------------------------



hq_2011 <- read.csv("data/2011_census/KS501SC.csv") %>%
    tbl_df

hq_2011 <- hq_2011 %>%
    slice(-1) %>%
    rename(datazone=X) %>%
    gather(key=key, value=count, -datazone) %>%
    mutate(count=as.numeric(str_replace_all(str_replace_all(count, ",", ""), "-", "0")))

hq_2011 %>%
    group_by(key) %>%
    tally

hq_2011 <- hq_2011 %>%
    mutate(
        key=mapvalues(
            key,
            from= c(
                "All.people.aged.16.and.over",
                "All.people.aged.16.and.over..No.qualifications",
                "All.people.aged.16.and.over..Highest.level.of.qualification..Level.1.qualifications",
                "All.people.aged.16.and.over..Highest.level.of.qualification..Level.2.qualifications",
                "All.people.aged.16.and.over..Highest.level.of.qualification..Level.3.qualifications",
                "All.people.aged.16.and.over..Highest.level.of.qualification..Level.4.qualifications.and.above",
                "Schoolchildren.and.full.time.students..Aged.16.to.17",
                "Schoolchildren.and.full.time.students..Aged.18.and.over",
                "Full.time.students.aged.18.to.74..Economically.active..In.employment",
                "Full.time.students.aged.18.to.74..Economically.active..Unemployed",
                "Full.time.students.aged.18.to.74..Economically.Inactive"
            ),
            to= c(
                NA,
                "none",
                "lvl_1",
                "lvl_2",
                "lvl_3",
                "lvl_4",
                NA,
                NA,
                NA,
                NA,
                NA
            )
            
        )
        
    ) %>%
    filter(!is.na(key)) %>%
    spread(key, count)


hq_2011 <- hq_2011 %>%
    mutate(year=2011) %>%
    select(datazone, year, none, lvl_1, lvl_2, lvl_3, lvl_4)

hq <- bind_rows(hq_2001, hq_2011)

hq <- greater_glasgow_dzs %>%
    select(datazone=dz_2001) %>%
    inner_join(hq)


write.csv(hq, file="data/derived/highest_qual.csv", row.names=FALSE)


# Industry, 2001 ----------------------------------------------------------

# to do

ind_2001 <- read.csv("data/2001_census/KS011a.csv") %>%
    tbl_df

ind_2001 <-  area_unit_links  %>% 
    rename(Zone.Code = OA2001)  %>% 
    left_join(ind_2001)  %>% 
    distinct %>%
    select(-Zone.Code)  %>% 
    rename(datazone=Datazone)  %>% 
    group_by(datazone)  %>% 
    summarise_each(funs(sum))  %>% 
    filter(str_detect(datazone, "^S01")) %>%
    mutate(year=2001) %>%
    gather(key=category, value=count, -datazone, -year) %>%
    select(datazone, year, category, count) %>%
    spread(key=category, value=count)

ind_2001 <- ind_2001  %>% 
    mutate(
        fish_agg_hunt_forestry = fishing + agriculture_hunting_forestry        
        ) %>%
    select(-all_aged_16_74, -fishing, -agriculture_hunting_forestry)

# Industry - 2011 ---------------------------------------------------------



# Industry

ind_2011 <- read.csv("data/2011_census/KS605SC.csv") %>%
    tbl_df

ind_2011 <- ind_2011 %>%
    slice(-1) %>%
    rename(datazone=X) %>%
    gather(key=key, value=count, -datazone) %>%
    mutate(count=as.numeric(str_replace_all(str_replace_all(count, ",", ""), "-", "0")))

ind_2011 %>%
    group_by(key) %>%
    tally

ind_2011 <- ind_2011 %>%
    mutate(
        key=str_replace_all(str_extract(key, "^[A-Z]{1}[\\.]{2}"), "\\.", "")) %>%
    filter(!is.na(key)) %>%
    spread(key, count)

ind_2011 <- ind_2011 %>%
    mutate(year = 2011) %>%
    select(datazone, year, A, B, C, D, E, F, G, H, I, I, J, K, L, M, N, O, P, Q, R)

ind_2011 <- ind_2011 %>%
    transmute(
        datazone=datazone, 
        year=year,
        mining_and_quarrying = B,
        manufacturing = C,
        electricity_gas_water_supply = D + E,
        construction = F,
        wholesale_retail_trade_repairs = G,
        hotels_restaurants = I,
        transport_storage_communications = H + J,
        financial_intermediaries = K,
        real_estate_renting_business_activities = L + N,
        public_admin_defence_social_security = O + M,
        education = P,
        health_and_social_work = Q,
        other = R,
        fish_agg_hunt_forestry = A
        )

ind <- bind_rows(ind_2001, ind_2011)

ind <- greater_glasgow_dzs %>%
    select(datazone=dz_2001) %>%
    inner_join(ind) %>%
    arrange(year, datazone) 

write.csv(ind, file="data/derived/industry.csv", row.names=FALSE)



# Ethnicity - 2001 --------------------------------------------------------

eth_2001 <- read.csv("data/prepared_census/eg_2001.csv") %>%
    tbl_df

# african = acb
# all_people =NA
# any_mixed_background = mixed
# bangladeshi =asian
# black_scottish_or_other_black =acb
# caribbean =acb
# chinese =asian
# gaelic_speaker_and_born_in_scotland =NA
# gaelic_speaker_and_not_born_in_scotland = NA 
# indian =asian
# other_ethnic_group = other 
# other_south_asian =asian
# other_white =wt_nonscot
# other_white_british =wt_nonscot
# pakistani =asian
# white_irish =wt_nonscot
# white_scottish =wt_scot
 
eth_2001 <- eth_2001 %>%
    select(-X) %>%
    spread(type, count, fill=0) %>%
    group_by(datazone, year) %>%
    summarise(
        wt_scot = sum(white_scottish),
        wt_nonscot = sum(other_white + other_white_british + white_irish),
        acb = sum(african + black_scottish_or_other_black + caribbean),
        asian = sum(bangladeshi + chinese + indian + other_south_asian + pakistani),
        mixed= sum(any_mixed_background),        
        other= sum(other_ethnic_group)
        ) 

# have checked - these figs are mutually exclusive and exhaustive.



# all_people =NA

# gaelic_speaker_and_born_in_scotland =NA
# gaelic_speaker_and_not_born_in_scotland = NA 


# Ethnicity - 2011 --------------------------------------------------------


# 5) ethnicity

eth_2011 <- read.csv("data/prepared_census/eg_2011.csv") %>%
    tbl_df

eth_2011 <- eth_2011 %>%
    select(datazone, type,  count)

eth_2011 <- eth_2011 %>%
    mutate(count = str_replace_all(str_replace_all(count, "-", "0"), ",", ""),
           count= as.numeric(as.character(count)))


# african 
# african..african..african.scottish.or.african.british 
# african..other.african 
# all.people 
# asian..asian.scottish.or.asian.british 
# asian..asian.scottish.or.asian.british..bangladeshi..bangladeshi.scottish.or.bangladeshi.british 
# asian..asian.scottish.or.asian.british..chinese..chinese.scottish.or.chinese.british 
# asian..asian.scottish.or.asian.british..indian..indian.scottish.or.indian.british 
# asian..asian.scottish.or.asian.british..other.asian 
# asian..asian.scottish.or.asian.british..pakistani..pakistani.scottish.or.pakistani.british 
# caribbean.or.black 
# caribbean.or.black..black..black.scottish.or.black.british  
# caribbean.or.black..caribbean..caribbean.scottish.or.caribbean.british 
# caribbean.or.black..other.caribbean.or.black 
# mixed.or.multiple.ethnic.groups 
# other.ethnic.groups 
# other.ethnic.groups..arab..arab.scottish.or.arab.british 
# other.ethnic.groups..other.ethnic.group 
# white 
# white..gypsy.traveller
# white..irish 
# white..other.british 
# white..other.white 
# white..polish 
# white..scottish 


eth_2011 <- eth_2011 %>%
    spread(type, count) %>%
    mutate(
        year=2011,
        wt_scot=white..scottish,
        wt_nonscot=white - wt_scot ,
        acb=african + caribbean.or.black,
        asian=asian..asian.scottish.or.asian.british,        
        mixed=mixed.or.multiple.ethnic.groups,
        other=other.ethnic.groups
    ) %>%
    select(datazone, year, wt_scot, wt_nonscot, acb, asian, mixed, other)

eth <- bind_rows(eth_2001, eth_2011)

eth <- greater_glasgow_dzs %>%
    select(datazone=dz_2001) %>%
    inner_join(eth) %>%
    arrange(year, datazone)


write.csv(eth, file="data/derived/ethnicity.csv", row.names=FALSE)



# country of origin 2001 -----------------------------------------------------------

coo_2001 <- read.csv("data/prepared_census/coo_2001.csv") %>%
    tbl_df

coo_2001 <- coo_2001 %>%
    select(-X, datazone, year, type, count) %>%
    spread(type, count) %>%
    select(datazone, year, scotland, england, northern_ireland, wales, rep_ireland, other_eu, elsewhere)

# country of origin 2011 -----------------------------------------------------------

coo_2011 <- read.csv("data/prepared_census/coo_2011.csv") %>%
    tbl_df

coo_2011 <- coo_2011 %>%
    select(-X, datazone, year, type, count) %>%
    spread(type, count) %>%
    mutate(
        all_people = all.people,
        england = england,
        northern_ireland = northern.ireland,
        other_eu = other.eu..accession.countries.april.2001.to.march.2011 + other.eu..member.countries.in.march.2001..1.,
        rep_ireland = republic.of.ireland,
        scotland=scotland, 
        wales=wales,
        elsewhere=other.countries
        ) %>%
    select(datazone, year, scotland, england, northern_ireland, wales, rep_ireland, other_eu, elsewhere)

coo <- bind_rows(coo_2001, coo_2011)

coo <- greater_glasgow_dzs %>%
    select(datazone=dz_2001) %>%
    inner_join(coo) %>%
    arrange(year, datazone)

write.csv(coo, file="data/derived/coo.csv", row.names=FALSE)


# Religion ----------------------------------------------------------------


# religion 2001 -----------------------------------------------------------

rel_2001 <- read.csv("data/prepared_census/rel_2001.csv") %>%
    tbl_df

rel_2001 <- rel_2001 %>%
    select(datazone, year, type, count) %>%
    spread(type, count) %>%
    mutate(
        cos = church_of_scotland,
        catholic = roman_catholic,
        other_christian = other_christian,
        none = none,
        other_religion = another_religion + buddhist + hindu, + jewish + muslim + sikh,
        not_announced = not_announced
        ) %>%
    select(datazone, year, cos, catholic, other_christian, none, other_religion, not_announced)

# religion 2011 -----------------------------------------------------------

rel_2011 <- read.csv("data/prepared_census/rel_2011.csv") %>%
    tbl_df

rel_2011 <- rel_2011 %>%
    select(datazone, year, type, count)

rel_2011 <- rel_2011 %>%
    mutate(count = as.numeric(str_replace(str_replace(count, "-", "0"), ",", ""))) 


rel_2011 <- rel_2011 %>%
    select(datazone, year, type, count) %>%
    spread(type, count) %>%
    mutate(
        cos= church.of.scotland,
        catholic = roman.catholic,
        other_christian = other.christian,
        none = no.religion,
        other_religion = buddhist + hindu + jewish + muslim + sikh + other.religion,
        not_announced = religion.not.stated
        ) %>%
    select(datazone, year, cos, catholic, other_christian, none, other_religion, not_announced)

rel <- bind_rows(rel_2001, rel_2011)

rel <- greater_glasgow_dzs %>%
    select(datazone = dz_2001) %>%
    inner_join(rel) %>%
    arrange(year, datazone)

write.csv(rel, file="data/derived/rel.csv", row.names=FALSE)
# Dwelling use ------------------------------------------------------------

# Dwelling use 2001 -------------------------------------------------------



# 4) land use # NOTE: 2001 only
household_spaces_01 <- source_DropboxData(
    file="household_spaces.csv",
    key="4eg2to2vx7rrfki")  %>% tbl_df


household_spaces_01 <- household_spaces_01 %>%
    rename(
        holiday=HO.holiday,
        occupied=HO.occupied, 
        vacant=HO.vacant
    ) %>% 
    select(datazone, year, occupied, holiday, vacant) 



#  Space diversity 2011 ---------------------------------------------------

household_spaces_11 <- source_DropboxData(
    file="KS401SC.csv",
    key="adqpk37jkscnni6")  %>% tbl_df

household_spaces_11 <- household_spaces_11 %>%
    rename(datazone=V1) %>%
    filter(str_detect(datazone, "^S01")) %>%
    mutate(year = 2011) %>%
    gather(key=type, value=count, -datazone, -year) %>%
    mutate(count = as.numeric(str_replace(str_replace(count, "-", "0"), ",", "")))


household_spaces_11 <- household_spaces_11 %>%
    filter(
        type %in% c(
            "All household spaces: Occupied", 
            "All household spaces: Unoccupied: Second residence/holiday accommodation",
            "All household spaces: Unoccupied: Vacant"
            )
        ) %>%
    spread(type, count)

names(household_spaces_11) <- c("datazone", "year", "occupied", "holiday", "vacant")

household_spaces <- bind_rows(household_spaces_01, household_spaces_11)

household_spaces <- greater_glasgow_dzs  %>% 
    select(datazone = dz_2001)  %>% 
    inner_join(household_spaces)

write.csv(household_spaces, file="data/derived/household_space_use.csv", row.names=FALSE)

# Economic activity - 2001 ------------------------------------------------

ecact_2001 <- read.csv("data/2001_census/KS009a.csv") %>%
    tbl_df

ecact_2001 <- area_unit_links  %>% 
    rename(Zone.Code = OA2001)  %>% 
    left_join(ecact_2001)  %>% 
    distinct %>%
    select(-Zone.Code)  %>% 
    rename(datazone=Datazone)  %>% 
    group_by(datazone)  %>% 
    summarise_each(funs(sum))  %>% 
    filter(str_detect(datazone, "^S01")) %>%
    mutate(year=2001) %>%
    gather(key=category, value=count, -datazone, -year) %>%
    select(datazone, year, category, count) %>%
    spread(key=category, value=count)



# Economic Activity - 2011 ------------------------------------------------

ecact_2011 <- source_DropboxData(
    file = "KS601SC.csv",
    key= "kejjriv7drqsub1"
    ) %>%
    tbl_df

ecact_2011 <- ecact_2011  %>% 
    rename(datazone=V1)  %>% 
    filter(str_detect(datazone, "^S01")) 

# Common categories?
## All categories are the same, and in the same order

# Need to convert from character to numeric
ecact_2011 <- ecact_2011  %>% 
    gather(key=category, value=count, -datazone)   %>% 
    mutate(year = 2011) %>%
    mutate(count = as.integer(as.character(str_replace(str_replace(count, ",", ""), "-", "0"))))  %>%
    select(datazone, year, category, count)  %>% 
    spread(key=category, value=count)


names(ecact_2011) <- names(ecact_2001)

ecact <- bind_rows(ecact_2001, ecact_2011)

ecact <- greater_glasgow_dzs  %>% 
    select(datazone = dz_2001)  %>% 
    inner_join(ecact)

write.csv(ecact, file="data/derived/economic_activity.csv", row.names=FALSE)


# Building use ------------------------------------------------------------


# See paf_building_type_count_extraction for bulk of data wrangling

use_2001 <- read.csv("data/derived/building_use_counts_2010_datazone.csv") %>%
    tbl_df

use_2010 <- read.csv("data/derived/building_use_counts_2010_datazone.csv") %>%
    tbl_df


use_2001 <- use_2001 %>%
    mutate(year = 2001) %>%
    select(datazone, year, address=address_count, deliverypoint=deliverypoint_count, smallbus=smallbus_count)

use_2010 <- use_2010 %>%
    mutate(year = 2010) %>%
    select(datazone, year, address=address_count, deliverypoint=deliverypoint_count, smallbus=smallbus_count)

use <- bind_rows(use_2001, use_2010)

use <- greater_glasgow_dzs  %>% 
    select(datazone = dz_2001)  %>% 
    inner_join(use)

write.csv(use, file="data/derived/building_use.csv", row.names=FALSE)
