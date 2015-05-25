# A script to calculate diversity scores according to a range of dimensions 
# using SNS and 2011 census data


rm(list=ls())

require(repmis)
require(tidyr)
require(stringr)
require(plyr)
require(dplyr)
require(vegan)
require(ggplot2)
require(xtable)

# Entropy function --------------------------------------------------------


# Entropy calculation
H <- function(xx){
    N <- ncol(xx)
    p <- apply(xx, 1, function(x) (x+0.5)/sum(x))
    out <- -1 * apply(p, 2, function(x) sum(x * log(x))/N)
    return(out)
}




# Data  -------------------------------------------------------------------

# Primary
tenure <- read.csv("data/derived/tenure_by_dz.csv") %>%
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

#Tenure households - ONLY AVAILABLE FOR 2001!

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


# Building type 

bld_band <- read.csv("data/derived/dwellings_by_band.csv") %>%
    tbl_df

bld_size <- read.csv("data/derived/dwellings_by_size.csv") %>%
    tbl_df

bld_type <- read.csv("data/derived/dwellings_by_type.csv") %>%
    tbl_df


# Demographic -------------------------------------------------------------

demo_as <- read.csv("data/derived/demographic_groupings.csv") %>%
    tbl_df

demo_eth <- read.csv("data/derived/ethnicity.csv") %>%
    tbl_df

demo_rel <- read.csv("data/derived/rel.csv") %>%
    tbl_df

demo_coo <- read.csv("data/derived/coo.csv") %>%
    tbl_df


# Economy -------------------------------------------------------------

econ_qual <- read.csv("data/derived/highest_qual.csv") %>%
    tbl_df

econ_act <- read.csv("data/derived/economic_activity.csv") %>%
    tbl_df

econ_sec <- read.csv("data/derived/sec_by_dz.csv") %>%
    tbl_df

econ_ind <- read.csv("data/derived/industry.csv") %>%
    tbl_df


# Land use ----------------------------------------------------------------

land_vacant <- read.csv("data/derived/household_space_use.csv") %>%
    tbl_df

land_bus <- read.csv("data/derived/building_use.csv") %>%
    tbl_df


# NOTE: Not all are in the same format. Some additional prep will be required



# Diversity using H -------------------------------------------------------


tenure$H <- H(as.matrix(tenure[,c("social", "rented", "owned")]))
tenure_H <- tenure %>%
    select(datazone=dz_2001, year=year, H=H)


bld_band$H <- H(as.matrix(bld_band[,c("A", "B", "C", "D", "E" ,"F", "G", "H")]))
bld_band_H <- bld_band %>%
    select(datazone, year, H)

bld_size <- bld_size %>%
    mutate(num_of_rooms = paste0("n_", num_of_rooms)) %>%
    spread(key=num_of_rooms, value=count)
bld_size$H <- H(as.matrix(bld_size[,-c(1,2)]))
bld_size_H <- bld_size %>%
    select(datazone, year, H)

bld_type <- bld_type  %>%
    spread(key=type, value=count) 
bld_type$H <- H(as.matrix(bld_type[,-c(1,2)]))
bld_type_H <- bld_type %>%
    select(datazone, year, H)

demo_as$H <- H(as.matrix(demo_as[,-c(1,2)]))
demo_as_H <- demo_as %>%
    select(datazone=dz_2001, year, H)


demo_eth$H <- H(as.matrix(demo_eth[,-c(1,2)]))
demo_eth_H <- demo_eth %>%
    select(datazone, year, H)

demo_rel$H <- H(as.matrix(demo_rel[,-c(1,2)]))
demo_rel_H <- demo_rel %>%
    select(datazone, year, H)

demo_coo$H <- H(as.matrix(demo_coo[,-c(1,2)]))
demo_coo_H <- demo_coo %>%
    select(datazone, year, H)


econ_qual$H <- H(as.matrix(econ_qual[,-c(1,2)]))
econ_qual_H <- econ_qual %>%
    select(datazone, year, H)

econ_act$H <- H(as.matrix(econ_act[,-c(1,2)]))
econ_act_H <- econ_act %>%
    select(datazone, year, H)

econ_sec$H <- H(as.matrix(econ_sec[,-c(1,2)]))
econ_sec_H <- econ_sec %>%
    select(datazone, year, H)

econ_ind$H <- H(as.matrix(econ_ind[,-c(1,2)])) 
econ_ind_H <- econ_ind %>%
    select(datazone, year, H)

land_vacant$H <- H(as.matrix(land_vacant[,-c(1,2)]))
land_vacant_H <- land_vacant %>%
    select(datazone, year, H)


land_bus$H <- H(as.matrix(land_bus[,-c(1,2)])) 
land_bus_H <- land_bus %>%
    select(datazone, year, H)

# Now to combine where there are common years

all_H <- tenure_H %>%
    rename(tenure=H) %>%
    full_join(bld_band_H) %>%
    rename(bld_band=H) %>%
    full_join(bld_size_H) %>%
    rename(bld_size=H) %>%
    full_join(bld_type_H) %>%
    rename(bld_type=H) %>%
    full_join(demo_as_H) %>%
    rename(demo_as=H) %>%
    full_join(demo_eth_H) %>%
    rename(demo_eth=H) %>%
    full_join(demo_rel_H)  %>% 
    rename(demo_rel=H) %>%
    full_join(demo_coo_H) %>%
    rename(demo_coo=H) %>%
    full_join(econ_qual_H) %>%
    rename(econ_qual=H) %>%
    full_join(econ_act_H) %>%
    rename(econ_act=H) %>%
    full_join(econ_sec_H) %>%
    rename(econ_sec=H) %>%
    full_join(econ_ind_H) %>%
    rename(econ_ind=H) %>%
    full_join(land_vacant_H) %>%
    rename(land_vacant=H) %>%
    full_join(land_bus_H) %>%
    rename(land_bus=H)



greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% 
    tbl_df() %>%
    rename(datazone=dz_2001)

all_H <- all_H %>%
    gather(key=type, value=H, -datazone, -year) %>%
    filter(!is.na(H)) %>%
    group_by(type) %>%
    mutate(t1 = min(year), t2=max(year)) %>%
    mutate(d1 = abs(year - t1), d2 = abs(year - t2)) %>%
    mutate(period = ifelse(d1==min(d1), "t1", ifelse(d2==min(d2), "t2", NA))) %>%
    filter(!is.na(period)) %>%
    select(datazone, year, period, type, H) %>%
    spread(key=type, value=H)

write.csv(all_H, file="data/derived/all_H.csv", row.names=F)

# H, Correlations in both periods --------------------------------------------


p_all_H <- all_H %>% 
    gather(key=type, value=H, -datazone, -year, -period)  %>% 
    filter(!is.na(H) & is.finite(H))  %>% 
    filter(datazone %in% greater_glasgow_dzs$datazone) %>% 
    select(-year)  %>% 
    spread(key=type, value=H)

write.csv(p_all_H, file="data/derived/p_all_H.csv", row.names=F)

fn <- function(x){
    xx <- as.matrix(x[,-c(1,2)])
    out <- cor(xx, use="pairwise.complete.obs") %>%
        round(2)
    
    return(out)
}

H_corrs  <- dlply(p_all_H, .(period), fn)


# Table of correlations, t1 -----------------------------------------------

tab <- H_corrs[["t1"]]
print(xtable(tab), type="html", file="tables/Correlations_between_entropy_levels_t1.html")

# Table of correlations, t2 -----------------------------------------------
tab <- H_corrs[["t2"]]
print(xtable(tab), type="html", file="tables/Correlations_between_entropy_levels_t2.html")



# PCA, around 2001 --------------------------------------------------------

fit_t1 <- p_all_H %>%
    filter(period=="t1") %>%
    filter(complete.cases(.)) %>%
    select(-datazone, -period) %>%
    prcomp(., cor=T)



# table of H loadings around 2001 -----------------------------------------

fit_t1$rotation  %>% 
    round(2)  %>%
    xtable %>%
    print(type="html", file="tables/entropy_factor_loadings_t1.html")
     

# Graph of H loadings around 2001 -----------------------------------------

fit_t1$rotation  %>% 
    round(2)  %>% 
    as.data.frame  %>%
    mutate(type=rownames(.))  %>% 
    gather(key="pc", value="loading", -type)  %>% 
    ggplot(data=.) + 
    geom_point(aes(x=loading, y= type)) + 
    facet_wrap( ~ pc) + 
    geom_segment(
        aes(
            y=type, yend=type, 
            x=ifelse(loading < 0, loading, 0), 
            xend=ifelse(loading > 0, loading, 0)
            )
        ) + 
    labs(x="Factor loading on Principal Component", y="Entropy Type", title="Factor loadings around 2001") +
    theme(plot.title=element_text(face = "bold"))
ggsave("figures/h_factor_loadings_pca_around_2001.tiff", dpi=300, height=30, width=30, unit="cm")



# pca around 2011 ---------------------------------------------------------

fit_t2 <- p_all_H %>%
    filter(period=="t2") %>%
    filter(complete.cases(.)) %>%
    select(-datazone, -period) %>%
    prcomp(., cor=T)



# table of H loadings around 2011 -----------------------------------------


fit_t2$rotation  %>% 
    round(2)  %>%
    xtable %>%
    print(type="html", file="tables/entropy_factor_loadings_t2.html")

print(xtable(tab), type="html", file="tables/e0_mean_1750_onwards.html")



# Graph of H loadings around 2001 -----------------------------------------

fit_t2$rotation  %>% 
    round(2)  %>% 
    as.data.frame  %>%
    mutate(type=rownames(.))  %>% 
    gather(key="pc", value="loading", -type)  %>% 
    ggplot(data=.) + 
    geom_point(aes(x=loading, y= type)) + 
    facet_wrap( ~ pc) + 
    geom_segment(
        aes(
            y=type, yend=type, 
            x=ifelse(loading < 0, loading, 0), 
            xend=ifelse(loading > 0, loading, 0)
        )
    ) + 
    labs(x="Factor loading on Principal Component", y="Entropy Type", title="Factor loadings around 2011") +
    theme(plot.title=element_text(face = "bold"))
ggsave("figures/h_factor_loadings_pca_around_2011.tiff", dpi=300, height=30, width=30, unit="cm")



# Do as above, but Shannon's diversity index not H ----------------------------------------

tenure$S <- diversity(as.matrix(tenure[,c("social", "rented", "owned")]))
tenure_S <- tenure %>%
    select(datazone=dz_2001, year=year, S=S)


bld_band$S <- bld_band  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

bld_band_S <- bld_band %>%
    select(datazone, year, S)

bld_size$S <- bld_size  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
    
bld_size_S <- bld_size %>%
    select(datazone, year, S)

bld_type$S <- bld_size  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

bld_type_S <- bld_type %>%
    select(datazone, year, S)

demo_as$S <- demo_as  %>% 
    select(-dz_2001, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
demo_as_S <- demo_as %>%
    select(datazone=dz_2001, year, S)


demo_eth$S <- demo_eth  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
demo_eth_S <- demo_eth %>%
    select(datazone, year, S)

demo_rel$S <- demo_rel  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

demo_rel_S <- demo_rel %>%
    select(datazone, year, S)

demo_coo$S <- demo_coo  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

demo_coo_S <- demo_coo %>%
    select(datazone, year, S)


econ_qual$S <- econ_qual  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
    
econ_qual_S <- econ_qual %>%
    select(datazone, year, S)

econ_act$S <- econ_act  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

econ_act_S <- econ_act %>%
    select(datazone, year, S)

econ_sec$S <- econ_sec  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

econ_sec_S <- econ_sec %>%
    select(datazone, year, S)

econ_ind$S <- econ_ind  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

econ_ind_S <- econ_ind %>%
    select(datazone, year, S)

land_vacant$S <- land_vacant  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity
    
land_vacant_S <- land_vacant %>%
    select(datazone, year, S)


land_bus$S <- land_bus  %>% 
    select(-datazone, -year, -H)  %>% 
    as.matrix  %>% 
    diversity

land_bus_S <- land_bus %>%
    select(datazone, year, S)

# Now to combine where there are common years

all_S <- tenure_S %>%
    rename(tenure=S) %>%
    full_join(bld_band_S) %>%
    rename(bld_band=S) %>%
    full_join(bld_size_S) %>%
    rename(bld_size=S) %>%
    full_join(bld_type_S) %>%
    rename(bld_type=S) %>%
    full_join(demo_as_S) %>%
    rename(demo_as=S) %>%
    full_join(demo_eth_S) %>%
    rename(demo_eth=S) %>%
    full_join(demo_rel_S)  %>% 
    rename(demo_rel=S) %>%
    full_join(demo_coo_S) %>%
    rename(demo_coo=S) %>%
    full_join(econ_qual_S) %>%
    rename(econ_qual=S) %>%
    full_join(econ_act_S) %>%
    rename(econ_act=S) %>%
    full_join(econ_sec_S) %>%
    rename(econ_sec=S) %>%
    full_join(econ_ind_S) %>%
    rename(econ_ind=S) %>%
    full_join(land_vacant_S) %>%
    rename(land_vacant=S) %>%
    full_join(land_bus_S) %>%
    rename(land_bus=S)



greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv") %>% 
    tbl_df() %>%
    rename(datazone=dz_2001)

all_S <- all_S %>%
    gather(key=type, value=S, -datazone, -year) %>%
    filter(!is.na(S) & !is.na(year)) %>%
    group_by(type) %>%
    mutate(t1 = min(year), t2=max(year)) %>%
    mutate(d1 = abs(year - t1), d2 = abs(year - t2)) %>%
    mutate(period = ifelse(d1==min(d1), "t1", ifelse(d2==min(d2), "t2", NA))) %>%
    filter(!is.na(period)) %>%
    select(datazone, year, period, type, S) %>%
    spread(key=type, value=S)

write.csv(all_S, file="data/derived/all_S.csv", row.names=F)


# S, Correlations in both periods --------------------------------------------


p_all_S <- all_S %>% 
    gather(key=type, value=S, -datazone, -year, -period)  %>% 
    filter(!is.na(S) & is.finite(S))  %>% 
    filter(datazone %in% greater_glasgow_dzs$datazone) %>% 
    select(-year)  %>% 
    spread(key=type, value=S)

write.csv(p_all_S, file="data/derived/p_all_S.csv", row.names=F)


