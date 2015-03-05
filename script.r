# First script
# 11/2/2015


# To do : 
# 1) Send Poverty & Suburbs work to Ade
# 2) reply to emails
# 3) extract list of dzs as per Ellie's suggestion [ done]
# 4) add packrat to see if this solves markdown issue [done - seems to ]
# 5) add existing script and adapt where relevant

rm(list=ls())

require(rmarkdown)
require(repmis)
require(plyr)
require(dplyr)
require(tidyr)

require(ggplot2)
require(ggtern)
require(corrplot)

require(vegan)



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

#write.csv(tenure_households, file="data/derived/tenure_by_dz.csv", row.names=F)



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

# ternery plot

ggtern(data=tenure_households, aes(x=social, y=owned, z=rented, colour=mix)) + geom_point()

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

populations <- read.csv("data/derived/populations_by_age_year_sex.csv") %>% 
    tbl_df()


# reduce to just Greater Glasgow
populations <- populations %>% rename(dz_2001=datazone)
populations <- populations %>% right_join(greater_glasgow_dzs)

# now 1.21 million
populations %>% 
    group_by(year, age_range, sex) %>%
    summarise(
        cells=n(),
        sum=sum(count)
        ) 

populations <- populations %>% arrange(year, sex, lower_age)

populations <- populations %>% 
    arrange(year,sex, lower_age) %>%
    mutate(
        working_age = ifelse(lower_age > 16 & ((upper_age < 60 & sex=="female") | (upper_age < 65 & sex=="male")), 1, 0)
    ) 

populations %>% 
    group_by(year, dz_2001, sex, working_age) %>%
    summarise(count=sum(count))

prop_working_age <- populations %>% 
    group_by(year, dz_2001, sex) %>%
    summarise(p_wage=sum(count[working_age==1])/sum(count))

prop_working_age <- prop_working_age[!is.nan(prop_working_age$p_wage),] 
prop_working_age <- prop_working_age %>% mutate(dep_r = (1 - p_wage) / p_wage)

prop_working_age %>% group_by(year, sex) %>%
    summarise( 
              q_025=quantile(p_wage, 0.025), 
              q_050=quantile(p_wage, 0.050),
              q_100=quantile(p_wage, 0.100),
              q_250=quantile(p_wage, 0.250),
              q_500=quantile(p_wage, 0.500),
              q_750=quantile(p_wage, 0.750),
              q_900=quantile(p_wage, 0.900),
              q_950=quantile(p_wage, 0.950),
              q_975=quantile(p_wage, 0.975)
              ) %>%
    ggplot(aes(x=year, y=q_500)) +
    facet_grid(. ~ sex) +
    geom_ribbon(aes(ymin=q_025, ymax=q_975), alpha=0.2) +
    geom_ribbon(aes(ymin=q_050, ymax=q_950), alpha=0.2) +
    geom_ribbon(aes(ymin=q_100, ymax=q_900), alpha=0.2) +
    geom_ribbon(aes(ymin=q_250, ymax=q_750), alpha=0.2) +
    geom_line(size=1.1) + 
    coord_cartesian(ylim=c(0,1)) + 
    labs(x="Year", y="Proportions of datazone over 16 and under 60 years old")

# equivalently, dependency ratio

prop_working_age %>% group_by(year, sex) %>%
    summarise( 
        q_025=quantile(dep_r, 0.025), 
        q_050=quantile(dep_r, 0.050),
        q_100=quantile(dep_r, 0.100),
        q_250=quantile(dep_r, 0.250),
        q_500=quantile(dep_r, 0.500),
        q_750=quantile(dep_r, 0.750),
        q_900=quantile(dep_r, 0.900),
        q_950=quantile(dep_r, 0.950),
        q_975=quantile(dep_r, 0.975)
    ) %>%
    ggplot(aes(x=year, y=q_500)) +
    facet_grid(. ~ sex) +
    geom_ribbon(aes(ymin=q_025, ymax=q_975), alpha=0.2) +
    geom_ribbon(aes(ymin=q_050, ymax=q_950), alpha=0.2) +
    geom_ribbon(aes(ymin=q_100, ymax=q_900), alpha=0.2) +
    geom_ribbon(aes(ymin=q_250, ymax=q_750), alpha=0.2) +
    geom_line(size=1.1) + geom_hline(yintercept=1, linetype="dashed") +
    labs(x="Year", y="Dependency Ratio")


# Correlation between mix and proportion_working_age at datazone level

tenure_wage <- prop_working_age %>% 
    select(-dep_r) %>%
    spread(key=sex, value=p_wage) %>%
    inner_join(tenure_households)

tenure_wage %>% 
    ggplot(aes(x=female, y=mix)) + 
    geom_point() + 
    stat_smooth(method="lm") + stat_smooth(colour="green")

    
tenure_wage %>% 
    ggplot(aes(x=male, y=mix)) + 
    geom_point() + 
    stat_smooth(method="lm") + stat_smooth(colour="green")


# Diversity Index ---------------------------------------------------------


#########################################################################################
# Instead of my own measure, how about using the diversity index?
# Available in the vegan package
# http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/diversity.html

# Trying out examples:
# data(BCI)
# H <- diversity(BCI)
# simp <- diversity(BCI, "simpson")
# invsimp <- diversity(BCI, "inv")
# r.2 <- rarefy(BCI, 2)
# alpha <- fisher.alpha(BCI)
# pairs(cbind(H, simp, invsimp, r.2, alpha), pch="+", col="blue")
# ## Species richness (S) and Pielou's evenness (J):
# S <- specnumber(BCI) ## rowSums(BCI > 0) does the same...
# J <- H/log(S)

# So, the counts are needed


tenure_diversity <- tenure_households %>% 
    filter(year==2001) %>% 
    select(all_households, social, rented, owned) %>% 
    mutate(
        social=social * all_households,
        rented=rented * all_households,
        owned=owned * all_households
           ) %>% 
    select(-all_households) %>% 
    diversity() %>% 
    as.data.frame() %>% tbl_df() 
names(tenure_diversity) = "diversity"

tenure_diversity <- tenure_diversity %>% 
    mutate(dz_2001 = tenure_households$dz_2001) %>% 
    tbl_df() %>% 
    select(dz_2001, diversity)

tenure_diversity <- tenure_households %>% 
    filter(year==2001) %>% 
    select(dz_2001, all_households, social, rented, owned) %>% 
    mutate(
        social=social ,
        rented=rented ,
        owned=owned 
        ) %>% 
    inner_join(tenure_diversity)

# Create 100 bins based on diversity 

tenure_diversity %>% 
    mutate(d_bin=ntile(diversity, 100)) %>%
    select(-all_households) %>% 
    gather(key=type, value=proportion, social, rented, owned) %>%
    arrange(d_bin) %>% 
    ggplot(aes(x=d_bin, y=proportion, fill=type, colour=type)) + geom_point(alpha=0.3) +
    facet_wrap(~ type) +
    labs(
        x="Diversity centile",
        y="Proportion of homes in datazones of this housing type")


# Other types of mix to look at:
# social class mix
# 

################################################################################################################


# Now to look at other kinds of mix

# mix types:

# Dwelling type mix

# dwellings, in by_year

require(plyr)
require(stringr)
require(tidyr)
require(dplyr)
require(ggplot2)
require(repmis)

greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% 
    tbl_df %>%
    rename(datazone=dz_2001)


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


# Diversity by housing sizes?

d_dwelling_types <- dwellings_types  %>% 
    spread(type, count)  %>% 
    select(-datazone, -year)  %>% 
    as.matrix %>%
    diversity

dwelling_types_wide <- dwellings_types %>%
    spread(type, count) %>%
    mutate(diversity=d_dwelling_types)
rm(d_dwelling_types)

# What has been the change in diversity from year to year?

div_by_year <- dwelling_types_wide %>%
    group_by(year) %>%
    summarise(
        q_025=quantile(diversity, 0.025),
        q_050=quantile(diversity, 0.050),
        q_100=quantile(diversity, 0.100),
        q_250=quantile(diversity, 0.250),
        q_500=quantile(diversity, 0.500),
        q_750=quantile(diversity, 0.750),
        q_900=quantile(diversity, 0.900),
        q_950=quantile(diversity, 0.950),
        q_975=quantile(diversity, 0.975)
    )


div_by_year %>% ggplot(aes(x=year, y=q_500)) +
    geom_line(size=1.1) +
    geom_ribbon(aes(ymin=q_025, ymax=q_975), alpha=0.2) +
    geom_ribbon(aes(ymin=q_050, ymax=q_950), alpha=0.2) +
    geom_ribbon(aes(ymin=q_100, ymax=q_900), alpha=0.2) +
    geom_ribbon(aes(ymin=q_250, ymax=q_750), alpha=0.2) +
    labs(x="Year", y="Diversity Level")


# Number of rooms by tenure diversity and proportion social_rented --------


rm(list=ls())
#pre-reqs
require(stringr)
require(tidyr)
require(dplyr)
require(repmis)
require(vegan)


# input_data

dwell_sizes <- read.csv("data/derived/dwellings_by_size.csv") %>%  tbl_df
greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv")  %>% tbl_df 
tenure <- read.csv("data/derived/tenure_by_dz.csv")  %>% tbl_df# note this is glasgow only 

tenure <- tenure %>%
    select(datazone=dz_2001, all_households, social, rented, owned)

tenure <- tenure %>%
    mutate(
        social_tertile=ntile(social, 3),
        social=social * all_households, 
        rented=rented * all_households, 
        owned=owned * all_households
           ) %>%
    select(-all_households)

tenure$diversity <- diversity(
    as.matrix(tenure[,2:4])
    )

tenure <- tenure %>%
    mutate(diversity_tertile=ntile(diversity, 3))

dwell_sizes <- dwell_sizes %>%
    inner_join(greater_glasgow_dzs, by=c("datazone"="dz_2001")) %>%
    select(-chp) %>%
    filter(year==2006)
    
combined <- tenure %>%
    inner_join(dwell_sizes)


# This will produce a 3x3 tiled graph. Each row represents a different tertile of the 
# proportion social rented
# each column represents a different tertile of the tenure diversity score for that area
ggplot(data=combined) + 
    facet_grid(social_tertile ~ diversity_tertile) + 
    geom_boxplot(aes(x=factor(num_of_rooms), y=count)) + 
    labs(x="number of rooms", y="count")



# Dimensions of Diversity -------------------------------------------------


# This section will look at the relationship between various forms of diversity 
# within Greater Glasgow. 

rm(list=ls())

require(repmis)
require(plyr)
require(tidyr)
require(stringr)
require(dplyr)

require(ggplot2)

require(vegan)


demo_div <- read.csv("data/derived/demographic_diversity_2010.csv") %>%
    tbl_df %>%
    rename(demographic=diversity)

dtype_div <- read.csv("data/derived/diversity_dwelling_type_2011.csv") %>%
    tbl_df %>%
    rename(dtype=diversity)

eth_div <- read.csv("data/derived/diversity_ethnicity_2011.csv") %>%
    tbl_df %>%
    rename(ethnicity=diversity)

sec_div <- read.csv("data/derived/diversity_sec_by_dz_2011_census.csv") %>%
    tbl_df %>%
    rename(sec=sec_div)

ten_div <- read.csv("data/derived/diversity_tenure_2001.csv" ) %>%
    tbl_df %>%
    rename(tenure=diversity)

spac_div <- read.csv("data/derived/diversity_space_2001.csv") %>%
    tbl_df %>%
    rename(space=diversity)

band_div <- read.csv("data/derived/diversity_dwelling_band_2011.csv") %>%
    tbl_df %>%
    rename(band=diversity)


# Inner join (will automatically cut to Greater Glasgow) while renaming vars

div_all <- demo_div %>%
    inner_join(dtype_div) %>%
    inner_join(eth_div) %>%
    inner_join(sec_div) %>%
    inner_join(ten_div) %>%
    inner_join(spac_div) %>%
    inner_join(band_div) %>%
    select(-X, -social, -rented, -owned)

d_rankdiv_all <- div_all %>%
    mutate_each(funs(min_rank), -datazone)

pairs(sample_n(d_rankdiv_all[,-1], 500), panel=panel.smooth)

cor(d_rankdiv_all[,-1], method="spearman") %>% 
    corrplot.mixed()


d_propdiv_all <- div_all %>%
    mutate_each(funs(percent_rank), -datazone)

pairs(d_propdiv_all[,-1], panel=panel.smooth)
cor(d_propdiv_all[,-1], method="spearman") %>% 
    corrplot.mixed()

# No difference

# factor analysis

m1 <- factanal(
    ~ demographic + dtype + ethnicity + sec + tenure + space + band, 
    data=d_propdiv_all, 
    factors=1
    )
m2 <- factanal(
    ~ demographic + dtype + ethnicity + sec + tenure + space + band, 
    data=d_propdiv_all, 
    factors=2
    )
m3 <- factanal(
    ~ demographic + dtype + ethnicity + sec + tenure + space + band, 
    data=d_propdiv_all, 
    factors=3
    )

load2 <- m2$loadings
plot(load2, type="n", xlim=c(-1, 1), ylim=c(-1,1), 
     xlab="Loading on first factor", ylab="Loading on second factor", 
     main="Diversity Correlations: Two factor model") 
text(load2, labels=names(d_propdiv_all)[-1], cex=0.7) 
abline(v=0, lty="dashed"); abline(h=0, lty="dashed")

load3_12 <- m3$loadings[,1:2]
plot(load3_12, type="n", xlim=c(-1, 1), ylim=c(-1,1), 
     xlab="Loading on first factor", ylab="Loading on second factor", 
     main="Diversity Correlations: Three factor model") 
text(load3_12, labels=names(d_propdiv_all)[-1], cex=0.7) 
abline(v=0, lty="dashed"); abline(h=0, lty="dashed")

load3_23 <- m3$loadings[,2:3]
plot(load3_23, type="n", xlim=c(-1, 1), ylim=c(-1,1), 
     xlab="Loading on second factor", ylab="Loading on third factor", 
     main="Diversity Correlations: Three factor model") 
text(load3_23, labels=names(d_propdiv_all)[-1], cex=0.7) 
abline(v=0, lty="dashed"); abline(h=0, lty="dashed")


load3_13 <- m3$loadings[,c(1,3)]
plot(load3_13, type="n", xlim=c(-1, 1), ylim=c(-1,1), 
     xlab="Loading on first factor", ylab="Loading on third factor", 
     main="Diversity Correlations: Three factor model") 
text(load3_13, labels=names(d_propdiv_all)[-1], cex=0.7) 
abline(v=0, lty="dashed"); abline(h=0, lty="dashed")


> plot(load, type="n"); text(load, labels=names(d_propdiv_all)[-1], cex=0.7)
> plot(load, type="n"); text(load, labels=names(d_propdiv_all)[-1], cex=0.7); abline(v=0, lty="dashed"); abline(h=0, lty="dashed")
