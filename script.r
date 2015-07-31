# First script
# 11/2/2015


# To do : 
# 1) Load previously calculated diversity scores
# 2) Load previous SIMD scores
# 3) Report diversity along each dimension by diversity quintile


rm(list=ls())


require(readr)


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

this_labeller <- function(variable, value){
    lookup <-  list(
        "t1" = "Around 2001 census",
        "t2" = "Around 2011 census"
    )
    return(lookup[value])
}
# Borrowing from : http://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels

simd_H_gg %>% ggplot(data=.) +
    geom_point(aes(x=overall_rank, y=tenure), alpha = 0.1) + 
    facet_grid(. ~ period, labeller=this_labeller) +
    stat_smooth(mapping=aes(x=overall_rank, y=tenure)) + 
    labs(x = "Overall SIMD rank", y = "Tenure diversity within datazone")

# asymmetric u-shaped relationship betwene the two, but overall 
# diversity has increased 

# by quintile

simd_H_gg %>% 
    group_by(period) %>% 
    mutate(simd_quint = ntile(overall_rank, 5)) %>% 
    group_by(period, simd_quint) %>% 
    summarise_each(funs(median(., na.rm=T)), tenure:land_bus) 


simd_H_gg %>% 
    group_by(period) %>% 
    mutate(simd_quint = ntile(overall_rank, 5)) %>% 
    group_by(period, simd_quint) %>% 
    summarise_each(funs(median(., na.rm=T)), tenure:land_bus) %>% 
    gather(key=div_type, value=div_value, -period, -simd_quint) %>% 
    ggplot(.) +
    geom_bar(aes(x=simd_quint, y=div_value, group=period, fill = period, colour=period), stat = "identity", position = "dodge") +
    facet_wrap( ~ div_type, scales ="free_y") + 
    labs(x = "SIMD Quintile within Greater Glasgow", y = "median diversity scores for areas within quintile")

ggsave("figures/diversity_quintiles_gg_free_y.png",
       width = 25, height = 15, units = "cm", dpi = 300
)

simd_H_gg %>% 
    group_by(period) %>% 
    mutate(simd_quint = ntile(overall_rank, 5)) %>% 
    group_by(period, simd_quint) %>% 
    summarise_each(funs(median(., na.rm=T)), tenure:land_bus) %>% 
    gather(key=div_type, value=div_value, -period, -simd_quint) %>% 
    ggplot(., mapping = aes(x=simd_quint, y=div_value, group=period, colour=period, label = round(div_value, 4))) +
    geom_point() + geom_line() +
    geom_text(colour = "black") + 
    facet_wrap( ~ div_type, scales ="free_y") + 
    labs(x = "SIMD Quintile within Greater Glasgow", y = "median diversity scores for areas within quintile")

ggsave("figures/diversity_quintiles_gg_labelled_lines_free_y.png",
       width = 50, height = 30, units = "cm", dpi = 300
)

simd_H_gg %>% 
    group_by(period) %>% 
    mutate(simd_quint = ntile(overall_rank, 5)) %>% 
    group_by(period, simd_quint) %>% 
    summarise_each(funs(median(., na.rm=T)), tenure:land_bus) %>% 
    gather(key=div_type, value=div_value, -period, -simd_quint) %>% 
    ggplot(.) +
    geom_bar(aes(x=simd_quint, y=div_value, group=period, fill = period, colour=period), stat = "identity", position = "dodge") +
    facet_wrap( ~ div_type) + 
    labs(x = "SIMD Quintile within Greater Glasgow", y = "median diversity scores for areas within quintile")

ggsave("figures/diversity_quintiles_gg.png_common_y.png",
       width = 25, height = 15, units = "cm", dpi = 300
)



# Notes on meeting with Ade, 28 July 2015
# 

# 1) descriptive statistics of mean values by SIMD quintile for variables where this is appropriate. 
# i) council tax band
dta_by_band <- read_csv("data/derived/dwellings_by_band.csv")

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

dta_by_band <- greater_glasgow_dzs %>% 
    select(datazone = dz_2001) %>% 
    left_join(dta_by_band) %>% 
    gather(key = band, value = count, -datazone, -year) %>% 
    left_join(simd_ss) 

# turn letters into corresponding numbers 
fn <- function(x) which(LETTERS %in% x)
dta_by_band$bandnum <- sapply(dta_by_band$band, fn)

dta_by_band %>% 
    mutate(
        rank_decile = ntile(overall_rank, 10),
        count =ifelse(is.na(count), 0, count)
        ) %>% 
    group_by(year, rank_decile, bandnum) %>% 
    summarise(
        count = sum(count)        
        ) %>% 
    mutate(
        tmp = bandnum * count ,
        tmp2 = bandnum^2 * count
        ) %>% 
    group_by(year, rank_decile) %>% 
    summarise(
        mean_band = sum(tmp) / sum(count),
        var_band = (sum(tmp2) / sum(count)) - mean_band, 
        sd_bankd = var_band ^ 0.5
    )



mean_bandnum = sum(bandnum * count) / sum(count),
sd_bandnum = ((sum(bandnum^2 * count) / sum(count) ) - mean(bandnum)) ^ (1/2)

    summarise(mean_band = mean(bandnum, na.rm=T))

# ii) number of rooms


# iii) proportion non-white
# iv) proportiona non-Scottish



# 2) graph showing the proportionate change in diversity scores, by quintile, for each of the variables, from t1 to t2. 

# 3) correlations/graphs showing association between deciles (rather than quintiles) of tenure diversity and 
# i) economic variables like highest qualification, economic activity, sec and industry; 
# ii) demographic variables like ethnic group, religion, and country of origin. 

# 4) correlations/graphs showing association between vacant land use deciles and 
# i) economic variables (highest qualification, economic activity, sec and so), but with vacant land use deciles and building size as the potential 'driver' variables. 
# 
# 5) 
# a) Cluster analysis characterising areas into up to five or six clusters based on the composition of tenure observed within datazones. 
# b) The cluster analysis is run on data from both t1 and t2, and a contingency table/transition matrix will be produced to help identify areas whose cluster has changed over time 
# c) The diversity and other descriptive statistics of certain types of 'cluster jumpers' (5b) will also be explored. 
# 
# 6) If the appropriate crosstabs exist from the 2001 and 2011 censuses, the correspondence between between employment rates and tenure-based variables (e.g. proportion of households in social rented accommodation) will also be explored. 
# 
# Does the above seem accurate enough? Any additional corrections and suggestions please let me know.
# 
# Best wishes,
# Jon


