# First script
# 11/2/2015


# To do : 
# 1) Load previously calculated diversity scores
# 2) Load previous SIMD scores
# 3) Report diversity along each dimension by diversity quintile


rm(list=ls())

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)

require(ggplot2)
require(corrplot)



# source files  -----------------------------------------------------------

greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% tbl_df()

simd <- read.csv("data/simd/00410767.csv") %>% tbl_df

all_H <- read.csv("data/derived/all_H.csv")  %>% tbl_df




# Derived data  -------------------------------------------------

all_H_gg <- all_H %>% filter(datazone %in% greater_glasgow_dzs$dz_2001)

all_H_gg <- all_H_gg %>% select(-year) %>% group_by(datazone, period) %>% 
    summarise_each(funs(mean(., na.rm=T)))


simd_ss <- simd %>% 
    select(
        datazone=Data.Zone, 
        overall_rank = Overall.SIMD.2012.Rank, 
        income_rank = Income.domain.2012.rank, 
        health_rank = Health.domain.2012.rank, 
        crime_rank = SIMD.Crime.2012.rank
        )

simd_ss[,-1] <- lapply(simd_ss[,-1], 
                       function(x) as.numeric(str_replace(as.character(x),",", ""))
                       ) 
simd_ss <- greater_glasgow_dzs %>% select(datazone=dz_2001) %>% left_join(simd_ss)

simd_H_gg <- simd_ss %>% left_join(all_H_gg)

# This is the df to work with

simd_H_gg


# Analyses  ---------------------------------------------------------------


# mean diversity, of each type, by overall SIMD rank, in each time period

summary(lm(tenure ~ overall_rank + period, data=simd_H_gg))
# As deprivation increases, tenure diversity reduces,
# but the diversity has increased over time

simd_H_gg %>% ggplot(data=.) +
    geom_point(aes(x=overall_rank, y=tenure)) + 
    facet_wrap(~ period) +
    stat_smooth(mapping=aes(x=overall_rank, y=tenure))
# asymmetric u-shaped relationship betwene the two, but overall 
# diversity has increased 

# by quintile

simd_H_gg %>% 
    group_by(period) %>% 
    mutate(simd_quint = ntile(overall_rank, 5)) %>% 
    group_by(period, simd_quint) %>% 
    summarise_each(funs(median(., na.rm=T)), tenure:land_bus)

# Note - many NaNs and Infs produced. need to investigate

simd_H_gg %>% 
    group_by(period) %>% 
    mutate(simd_quint = ntile(overall_rank, 5)) %>% 
    group_by(period, simd_quint) %>% 
    summarise_each(funs(median(., na.rm=T)), tenure:land_bus) %>% 
    gather(key=div_type, value=div_value, -period, -simd_quint) %>% 
    ggplot(.) +
    geom_point(aes(x=simd_quint, y=div_value, group=period, colour=period)) +
    facet_wrap( ~ div_type) 

