# A script to calculate diversity scores according to a range of dimensions 
# using SNS and 2011 census data





# Occupational Class From Census --------------------------------------------------


rm(list=ls())

require(repmis)
require(tidyr)
require(stringr)
require(plyr)
require(dplyr)
require(vegan)
require(ggplot2)

# Functions 

# Entropy calculation
H <- function(xx){
    N <- ncol(xx)
    p <- apply(xx, 1, function(x) (x+0.5)/sum(x))
    out <- -1 * apply(p, 2, function(x) sum(x * log(x))/N)
    return(out)
}
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
        X=X_nonstudent_total,
        S=X_student
    ) %>%
    select(datazone, total, I, II, III, IV, X, S) %>%
    mutate(t2=I+II+III+IV+X+S) %>%
    select(datazone, I, II, III, IV, X, S)

tmp$diversity <- tmp[,-1] %>%
    as.matrix %>%
    diversity 

tmp$H <- tmp  %>% 
    ungroup  %>% 
    select(-datazone, -diversity)  %>%
    as.matrix %>%
    H

dz_sec_diversity <- tmp %>%
    select(datazone, sec_div=diversity, sec_h=H)
dz_sec_diversity

write.csv(dz_sec_diversity, file="data/derived/diversity_sec_by_dz_2011_census.csv")


################

#Other types of diversity
# 1) Housing tenure diversity 



# Main Analysis -----------------------------------------------------------

greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% tbl_df()

#Tenure

tenure_households <- source_DropboxData(
    file="tenure_households.csv",
    key="6t6dss41g8fat1y"
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
) %>% mutate(
    social=council_houses + other_social_rented,
    rented=rented_from_employer + private_rented+ rented_from_relative,
    owned=owned_with_mortgage + owned_outright + shared_ownership
) %>%
    select(datazone, social, rented, owned)

# just for 2001 
tenure_households$diversity <- tenure_households %>%
    select(-datazone) %>%
    as.matrix %>%
    diversity

tenure_households$H <- tenure_households  %>% 
    ungroup  %>% 
    select(-datazone, -diversity)  %>%
    as.matrix %>%
    H

tenure_households <- tenure_households %>%
    select(datazone, diversity, H)

write.csv(tenure_households, file="data/derived/diversity_tenure_2001.csv", row.names=FALSE)

# 6505 observations - whole of Scotland
# left join to just Greater Glasgow
# tenure_households <- greater_glasgow_dzs %>% left_join(tenure_households)
# now 2200 observations
# this is 34% of total, dzs are approx equal population
# if Scot population is 5.3 million this implies 
# Pop of Greater Glasgow is about 1.8 Million - 
# does this seem reasonable?

#write.csv(tenure_households, file="data/derived/tenure_by_dz.csv", row.names=F)



# 2) council tax band diversity

dwelling_bands <- read.csv("data/derived/dwellings_by_band.csv") %>%
    tbl_df

dwelling_bands
# going for 2011 for comparability with census
dwelling_bands_2011 <- dwelling_bands %>%
    filter(year==2011)

dwelling_bands_2011$diversity <- dwelling_bands_2011%>%
    select(-datazone, -year) %>%
    as.matrix %>%
    diversity

dwelling_bands_2011$H <- dwelling_bands_2011%>%
    select(-datazone, -year, -diversity) %>%
    as.matrix %>%
    H

write.csv(dwelling_diversity_2011, file="data/derived/diversity_dwelling_band_2011.csv", row.names=FALSE)

# 3) dwelling type diversity

dwelling_types <- read.csv("data/derived/dwellings_by_type.csv") %>%
    tbl_df

tmp <- dwelling_types %>%
    filter(year==2011) %>%
    select(-year) %>%
    spread(type, count)

tmp$diversity <- tmp %>%
    select(-1) %>%
    as.matrix %>%
    diversity 

tmp$H <- tmp %>%
    select(-datazone, diversity) %>%
    as.matrix %>%
    H 

dwelling_type_diversity_2011 <- tmp %>%
    select(datazone, diversity, H)

write.csv(dwelling_type_diversity_2011, file="data/derived/diversity_dwelling_type_2011.csv", row.names=FALSE)


# 4) land use # NOTE: 2001 only
household_spaces <- source_DropboxData(
    file="household_spaces.csv",
    key="4eg2to2vx7rrfki")  %>% tbl_df

tmp <- household_spaces %>%
    rename(
        total=HO.allspaces,
        holiday=HO.holiday,
        occupied=HO.occupied, 
        vacant=HO.vacant
    ) %>% 
    select(datazone, total, occupied, holiday, vacant) 

tmp$diversity <- tmp  %>% 
    select(-datazone, -total)  %>% 
    as.matrix  %>% 
    diversity  

tmp$H <- tmp  %>% 
    select(-datazone, -total, -diversity)  %>% 
    as.matrix  %>% 
    H  


space_diversity_2001 <- tmp  %>%
    select(datazone, diversity, H)
rm(tmp)
write.csv(space_diversity_2001, file="data/derived/diversity_space_2001.csv", row.names=FALSE)

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

eth_2011 %>%
    group_by(type) %>%
    tally %>%
    print(n=25)

tmp <- eth_2011 %>%
    spread(type, count) %>%
    mutate(
        acb=african + caribbean.or.black,
        wt_scot=white..scottish,
        wt_nonscot=white - wt_scot ,
        asian=asian..asian.scottish.or.asian.british,
        mixed=mixed.or.multiple.ethnic.groups,
        other=other.ethnic.groups,
        t2=acb+wt_scot+wt_nonscot+asian+mixed+other,
        dif=all.people - t2
    ) %>%
    select(datazone, acb, wt_scot, wt_nonscot, asian, mixed, other)

tmp$diversity <- tmp  %>% 
    select(-1) %>%
    as.matrix %>%
    diversity 

tmp$H <- tmp %>%
    select(-datazone, -diversity) %>%
    H

diversity_eth_2011 <- tmp %>%
    select(datazone, diversity, H)

write.csv(diversity_eth_2011, file="data/derived/diversity_ethnicity_2011.csv", row.names=FALSE)

# 6) demographic mix # NOTE: THIS EXCLUDES 2011 - 2010 is last year
pops <- read.csv("data/derived/populations_by_age_year_sex.csv") %>%
    tbl_df


tmp <- pops  %>% 
    filter(year==2010)  %>% 
    unite(sa, sex, age_range)  %>% 
    select(datazone, sa, count) %>%
    spread(sa, count) %>%
    mutate(
        f1=female_0_4 + female_5_9 + female_10_15,
        m1=male_0_4 + male_5_9 + male_10_15, 
        
        f2=female_16_19 + female_20_24 + female_25_29,
        m2=male_16_19 + male_20_24 + male_25_29,
        
        f3=female_30_34 + female_35_39 + female_40_44,
        m3=male_30_34 + male_35_39 + male_40_44,
        f4=female_45_49 + female_50_54 + female_55_59,
        m4=male_45_49 + male_50_54 + male_55_59,
        f5=  female_60_64 + female_65_69 + female_70_74,
        m5=  male_60_64 + male_65_69 + male_70_74,
        f6= female_75_79 +female_80_84 +female_85_89 +female_90_101,    
        m6= male_75_79 +male_80_84 +male_85_89 +male_90_101
    ) %>%
    select(datazone, m1, m2, m3, m4, m5, m6, f1, f2, f3, f4, f5, f6)

tmp$diversity <- tmp %>%
    select(-1) %>%
    as.matrix %>%
    diversity

tmp$H <- tmp %>%
    select(-datazone, diversity) %>%
    as.matrix %>%
    H

diversity_demo_2010 <- tmp %>%
    select(datazone, diversity, H)

write.csv(diversity_demo_2010, file="data/derived/demographic_diversity_2010.csv", row.names=FALSE)

# 7) occupational class

# DONE


#### Additional sources of diversity?

# Candidates from SNS


# 2011 Census Tables ------------------------------------------------------


# The following tables have been found in the 2011 Census

# KS 204 SC     Country of birth [already done]
# QS 203 SC     Country of birth [already done]
# KS 501 SC     Highest qualification
# KS 605 SC     Industry
# QS 605 SC     Industry
# KS 206 SC     Language
# QS 702 SC     Method of Travel to Work
# KS 202 SC     National Identity
# KS 608 SC     Occupation
# QS 606 SC     Occupation
# KS 209 SCb    Religion
# KS 209 SCa    Religion (UK Harmonised)
# QS 101 SC     Residence Type
# KS 402 SC     Tenure




rm(list=ls())

require(repmis)
require(tidyr)
require(stringr)
require(plyr)
require(dplyr)
require(vegan)
require(ggplot2)

hq <- read.csv("data/2011_census/KS501SC.csv") %>%
    tbl_df

hq <- hq %>%
    slice(-1) %>%
    rename(datazone=X) %>%
    gather(key=key, value=count, -datazone) %>%
    mutate(count=as.numeric(str_replace_all(str_replace_all(count, ",", ""), "-", "0")))

hq %>%
    group_by(key) %>%
    tally

hq <- hq %>%
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


hq$diversity <- hq  %>% 
    select(-1)  %>% 
    as.matrix %>%
    diversity 
    
hq <- hq %>%
    select(datazone, diversity)

write.csv(hq, file="data/derived/highest_qual_diversity_2011.csv", row.names=FALSE)

# Industry

in1 <- read.csv("data/2011_census/KS605SC.csv") %>%
    tbl_df

in1 <- in1 %>%
    slice(-1) %>%
    rename(datazone=X) %>%
    gather(key=key, value=count, -datazone) %>%
    mutate(count=as.numeric(str_replace_all(str_replace_all(count, ",", ""), "-", "0")))

in1 %>%
    group_by(key) %>%
    tally

in1 <- in1 %>%
    mutate(
        key=str_replace_all(str_extract(key, "^[A-Z]{1}[\\.]{2}"), "\\.", "")) %>%
    filter(!is.na(key)) %>%
    spread(key, count)

in1$diversity <- in1  %>% 
    select(-1)  %>% 
    as.matrix %>%
    diversity 

in1 <- in1 %>%
    select(datazone, diversity)

write.csv(in1, file="data/derived/industry_1_diversity_2011.csv", row.names=FALSE)


## Language


lan <- read.csv("data/2011_census/KS206SC.csv") %>%
    tbl_df

lan <- lan %>%
    slice(-1) %>%
    rename(datazone=X) %>%
    gather(key=key, value=count, -datazone) %>%
    mutate(count=as.numeric(str_replace_all(str_replace_all(count, ",", ""), "-", "0")))

lan %>%
    group_by(key) %>%
    tally

lan in1 <- in1 %>%
    mutate(
        key=str_replace_all(str_extract(key, "^[A-Z]{1}[\\.]{2}"), "\\.", "")) %>%
    filter(!is.na(key)) %>%
    spread(key, count)

in1$diversity <- in1  %>% 
    select(-1)  %>% 
    as.matrix %>%
    diversity 

in1 <- in1 %>%
    select(datazone, diversity)

write.csv(in1, file="data/derived/industry_1_diversity_2011.csv", row.names=FALSE)

