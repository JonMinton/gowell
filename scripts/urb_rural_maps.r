# Analyses of urban and rural class


# Prereqs 

rm(list=ls())

require(spdep)
require(rgeos)
require(maptools)
require(rgdal)

require(RColorBrewer)


require(lattice)
require(ggplot2)
require(ggmap)

require(repmis)
require(plyr)
require(stringr)
require(tidyr)
require(dplyr)


# 2001 datazones and 2001 intermediate geographies are perfectly 
# matched. Therefore, I am looking for 2001 intermediate geography 
# shapefiles. 

# I have requested to download these from edina - done 
# putting these in data/shp/scotland_2001_intermed - done

# Tasks 
# i) load 2001 datazones
# ii ) display 2001 datazones

dz_shp <- readOGR("data/shp/scotland_2001_datazones", layer="scotland_dz_2001")
summary(dz_shp)
plot(dz_shp)

# iii) load 2001 igs
# iv ) display 2001 igs

ig_shp <- readOGR("data/shp/scotland_2001_intermed", layer="scotland_igeog_2001")
summary(ig_shp)
plot(ig_shp)


# Now to find our dzs comprising greater glasgow

greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv")  %>% tbl_df %>%
    rename(datazone=dz_2001)

greater_glasgow_igs <- read.csv("data/geographies/igs_in_greater_glasgow.csv") %>%
    tbl_df  %>% 
    rename(ig=ig_2001)


# Now to subset dzs  and subset igs

dz_gg_shp <- dz_shp[dz_shp$zonecode %in% greater_glasgow_dzs$datazone,]
plot(dz_gg_shp)

ig_gg_shp <- ig_shp[ig_shp$zonecode %in% greater_glasgow_igs$ig,]
plot(ig_gg_shp)

# Now, to load urban_rural cats

urb_rural <- source_DropboxData(
    file="urban_rural_classification.csv", key="tq4co47kclqkc39"
    ) %>%
    tbl_df

# using 2004

urb_rural <- urb_rural %>%
    filter(year==2004) %>%
    select(datazone, ur_class =CS.urclass6d04)

urb_rural$ur_class <- mapvalues(
    urb_rural$ur_class,
    from=c("1", "2","3","4","5", "6"),
    to=c(
        "1_large_urban",
        "2_other_urban",
        "3_accessible_town",
        "4_remote_town",
        "5_accessible_rural",
        "6_remote_rural"        
        )
)

dz_gg_shp@data <- merge(dz_gg_shp, urb_rural, by.x="zonecode", by.y="datazone")

tmp <- dz_gg_shp@data 
tmp  %>% group_by(ur_class)  %>% summarise(n=n())

# Base graphics approach:

plot(
    dz_gg_shp, border=FALSE
    )

plot(
    dz_gg_shp[dz_gg_shp$ur_class=="1_large_urban",],
    col="blue", border=FALSE
)

plot(
    dz_gg_shp[dz_gg_shp$ur_class=="2_other_urban",],
    col="green", add=TRUE, border=FALSE
)
plot(
    dz_gg_shp[dz_gg_shp$ur_class=="3_accessible_town",],
    col="red", add=TRUE, border=FALSE
)

plot(
    dz_gg_shp[dz_gg_shp$ur_class=="5_accessible_rural",],
    col="yellow", add=TRUE, border=FALSE
)
plot(ig_gg_shp, add=T)

legend("bottomleft", 
       legend=c(
           "1) Large Urban", "2) Other Urban", "3) Accessible Town", "5) Accessible Rural"
           ),
       fill=c("blue", "green", "red", "yellow"),
       title="Key", bty="n"
       )


# Trying with spplot

dz_gg_shp@data$col_no <- as.factor(dz_gg_shp$ur_class)

my_pal <- brewer.pal(4, "Set1")
require(latticeExtra)


g1 <- spplot(dz_gg_shp, zcol="col_no", 
       colorkey=TRUE, col.regions=my_pal,
       col="transparent"
       )
g2 <- spplot(
    ig_gg_shp, 
    zcol="zonecode", col.regions="transparent", 
    colorkey=FALSE
    )

g1 + g2


# Now to map diversity quintiles 

demo_div <- read.csv("data/derived/demographic_diversity_2010.csv") %>%
    tbl_df %>%
    rename(demographic=diversity) %>%
    select(datazone, demographic)


dtype_div <- read.csv("data/derived/diversity_dwelling_type_2011.csv") %>%
    tbl_df %>%
    rename(dtype=diversity) %>%
    select(datazone, dtype)

eth_div <- read.csv("data/derived/diversity_ethnicity_2011.csv") %>%
    tbl_df %>%
    rename(ethnicity=diversity) %>%
    select(datazone, ethnicity)

sec_div <- read.csv("data/derived/diversity_sec_by_dz_2011_census.csv") %>%
    tbl_df %>%
    rename(sec=sec_div) %>%
    select(datazone, sec)

ten_div <- read.csv("data/derived/diversity_tenure_2001.csv" ) %>%
    tbl_df %>%
    rename(tenure=diversity) %>%
    select(datazone, tenure)

spac_div <- read.csv("data/derived/diversity_space_2001.csv") %>%
    tbl_df %>%
    rename(space=diversity) %>%
    select(datazone, space)

band_div <- read.csv("data/derived/diversity_dwelling_band_2011.csv") %>%
    tbl_df %>%
    rename(band=diversity) %>%
    select(datazone, band)

joined <- Reduce(inner_join, list(demo_div, dtype_div, eth_div, sec_div, ten_div, spac_div, band_div))


# Now quintiles 

joined_quints <- 
    joined %>%
    mutate_each(funs(ntile(., 5)), -datazone)


# 
# # n.b. need the 2001 not 2011 dz codes 
# chps_of_interest <- chps_of_interest %>% rename(chp=chcp_code) 
# 
# dzs_in_greater_glasgow <- chps_to_dzs %>% 
#     inner_join(chps_of_interest) %>%
#     select(-chcp_name)
# 
# write.csv(dzs_in_greater_glasgow, file="data/geographies/dzs_in_greater_glasgow.csv", row.names=F)


# superimpose 2001 intermeds on 2001 datazones
dz_shp <- readOGR("data/shp/scotland_2001_datazones", layer="scotland_dz_2001")
greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv")  %>% tbl_df %>%
    rename(datazone=dz_2001)

dz_gg_shp <- dz_shp[dz_shp@data$zonecode %in% greater_glasgow_dzs$datazone,]
unique_dzs <- unique(dz_gg_shp@data$zonecode)
dz_gg_shp <- dz_gg_shp[dz_gg_shp@data$zonecode %in% unique_dzs,]

dz_gg_shp@data <- merge(dz_gg_shp@data, joined_quints, by.x="zonecode", by.y="datazone", all.x=TRUE)

dz_gg_shp_qnts <- dz_gg_shp

dz_gg_shp_qnts@data <- dz_gg_shp_qnts@data[
    c(
        "zonecode","demographic", "dtype", "ethnicity", "sec", "tenure", "space", "band"
        )
    ]

# Now to find our dzs comprising greater glasgow

dz_gg_shp_qnts@data$demographic <- as.factor(dz_gg_shp_qnts@data$demographic)
spplot(dz_gg_shp_qnts, zcol="demographic", 
             col="transparent"
)

dz_gg_shp_qnts@data$dtype <- as.factor(dz_gg_shp_qnts@data$dtype)
spplot(dz_gg_shp_qnts, zcol="dtype", 
       col="transparent"
)

dz_gg_shp_qnts@data$sec <- as.factor(dz_gg_shp_qnts@data$sec)
spplot(dz_gg_shp_qnts, zcol="sec", 
       col="transparent"
)

dz_gg_shp_qnts@data$tenure <- as.factor(dz_gg_shp_qnts@data$tenure)
spplot(dz_gg_shp_qnts, zcol="tenure", 
       col="transparent"
)

dz_gg_shp_qnts@data$space <- as.factor(dz_gg_shp_qnts@data$space)
spplot(dz_gg_shp_qnts, zcol="space", 
       col="transparent"
)

dz_gg_shp_qnts@data$band <- as.factor(dz_gg_shp_qnts@data$band)
spplot(dz_gg_shp_qnts, zcol="band", 
       col="transparent"
)



# Map changes in diversity ------------------------------------------------

p_all_H <- read.csv("data/derived/p_all_H.csv") %>%
    tbl_df

tenure_div_quint <- p_all_H  %>% 
    mutate_each(funs(ntile(., n=3)), -datazone, -period)  %>% 
    select(datazone, period, tenure)  %>% 
    spread(key=period, value=tenure)  %>% 
    mutate(change_tenure_quint= t2 - t1) 


dz_shp <- readOGR("data/shp/scotland_2001_datazones", layer="scotland_dz_2001")
greater_glasgow_dzs <- read.csv("data/geographies/dzs_in_greater_glasgow.csv")  %>% tbl_df %>%
    rename(datazone=dz_2001)

dz_gg_shp <- dz_shp[dz_shp@data$zonecode %in% greater_glasgow_dzs$datazone,]
unique_dzs <- unique(dz_gg_shp@data$zonecode)
dz_gg_shp <- dz_gg_shp[dz_gg_shp@data$zonecode %in% unique_dzs,]

dz_gg_shp@data <- merge(dz_gg_shp@data, tenure_div_quint, by.x="zonecode", by.y="datazone", all.x=TRUE)

dz_gg_shp_qnts <- dz_gg_shp

dz_gg_shp_qnts@data$change_tenure_quint <- as.factor(dz_gg_shp_qnts@data$change_tenure_quint)
spplot(dz_gg_shp_qnts, zcol="change_tenure_quint", 
       col="transparent"
)

# To do : 
# zoom out of 'Glasgow' slightly

tmp <- qmap("Glasgow", zoom=9)
# transform and fortify ig_gg_shp, do choropleth

dz_gg_fort <- dz_gg_shp
dz_gg_fort <- spTransform(dz_gg_fort, CRS("+proj=longlat +datum=WGS84"))
dz_gg_fort <- fortify(dz_gg_fort, region="zonecode")
dz_gg_fort <- dz_gg_fort %>%
    left_join(dz_gg_shp@data, by=c("id"="zonecode"))

tmp + geom_polygon(
    aes(x=long, y= lat, group=id, fill=as.factor(change_tenure_quint)),
    data=dz_gg_fort,
    alpha=0.7
)
