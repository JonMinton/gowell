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


