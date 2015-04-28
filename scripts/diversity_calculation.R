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

# quick regression


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

p_all_H <- all_H %>% 
    gather(key=type, value=H, -datazone, -year, -period)  %>% 
    filter(!is.na(H) & is.finite(H))  %>% 
    filter(datazone %in% greater_glasgow_dzs$datazone) %>% 
    select(-year)  %>% 
    spread(key=type, value=H)

fn <- function(x){
    xx <- as.matrix(x[,-c(1,2)])
    out <- cor(xx, use="pairwise.complete.obs") %>%
        round(2)
    
    return(out)
}

H_corrs  <- dlply(p_all_H, .(period), fn)



# Factor analysis ---------------------------------------------------------

# prcomp

fit <- p_all_H %>%
    filter(period=="t1") %>%
    filter(complete.cases(.)) %>%
    select(-datazone, -period) %>%
    prcomp(., cor=T)

summary(fit)
plot(fit)

fit <- p_all_H %>%
    filter(period=="t1") %>%
    filter(complete.cases(.)) %>%
    select(-datazone, -period) %>%
    princomp(., cor=T)

summary(fit)
plot(fit)


pairs(fit$loadings[,1:5])

pairs(fit$loadings[,1:5], col=1:(ncol(p_all_H) - 2), upper.panel=NULL, main="Factor loadings")
par(xpd=TRUE) 
legend('topright', bty='n', pch='o', col=1:(ncol(p_all_H) - 2), attr(fit$loadings, 'dimnames')[[1]], title="Variables")

m1 <- p_all_H  %>% 
    filter(period=="t1")  %>% 
    filter(complete.cases(.)) %>%
    select(-datazone, -period)  %>% 
    factanal(
    x=. , 
    factors=1
    )

m2 <- p_all_H  %>% 
    filter(period=="t1")  %>% 
    filter(complete.cases(.)) %>%
    select(-datazone, -period)  %>% 
    factanal(
        x=. , 
        factors=2
    )

m3 <- p_all_H  %>% 
    filter(period=="t1")  %>% 
    filter(complete.cases(.)) %>%
    select(-datazone, -period)  %>% 
    factanal(
        x=. , 
        factors=3
    )

m4 <- p_all_H  %>% 
    filter(period=="t1")  %>% 
    filter(complete.cases(.)) %>%
    select(-datazone, -period)  %>% 
    factanal(
        x=. , 
        factors=4
    )



load2 <- m2$loadings
plot(load2, type="n", xlim=c(-1, 1), ylim=c(-1,1), 
    xlab="Loading on first factor", ylab="Loading on second factor", 
    main="Diversity Correlations: Two factor model") 
text(load2, labels=names(p_all_H), cex=0.7) 
abline(v=0, lty="dashed"); abline(h=0, lty="dashed")

load1_2 <- m3$loadings[,1:2]
plot(load1_2, type="n", xlim=c(-1, 1), ylim=c(-1,1), 
     xlab="Loading on first factor", ylab="Loading on second factor", 
     main="Diversity Correlations: Three factor model") 
text(load3_1, labels=names(p_all_H), cex=0.7) 
abline(v=0, lty="dashed"); abline(h=0, lty="dashed")

load2_3 <- m3$loadings[,2:3]
plot(load2_3, type="n", xlim=c(-1, 1), ylim=c(-1,1), 
     xlab="Loading on second factor", ylab="Loading on third factor", 
     main="Diversity Correlations: Three factor model") 
text(load3_2, labels=names(p_all_H), cex=0.7) 
abline(v=0, lty="dashed"); abline(h=0, lty="dashed")

load1_3 <- m3$loadings[,c(1, 3)]
plot(load1_3, type="n", xlim=c(-1, 1), ylim=c(-1,1), 
     xlab="Loading on first factor", ylab="Loading on third factor", 
     main="Diversity Correlations: Three factor model") 
text(load1_3, labels=names(p_all_H), cex=0.7) 
abline(v=0, lty="dashed"); abline(h=0, lty="dashed")

# 3d representation of factors
tmp <- m3$loadings[,1:3]

open3d()
decorate3d(
    xlim=c(-1, 1), ylim=c(-1, 1), zlim=c(-1, 1),
    xlab="F1", ylab="F2", zlab="F3", box=FALSE, axes=FALSE
)
text3d(
    x=tmp[,1], y=tmp[,2], z=tmp[,3],
    text=rownames(tmp)
)

planes3d(a=1, b=0, c=0, alpha=0.2, col="red")
planes3d(a=0, b=1, c=0, alpha=0.2, col="green")
planes3d(a=0, b=0, c=1, alpha=0.2, col="blue")
points3d(x=tmp[,1], y=tmp[,2], z=tmp[,3], size=3)
