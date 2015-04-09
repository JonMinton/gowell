# Postcode address files - exploring how to use to create 
# measures of land use diversity at datazone and intermediate geography level

rm(list=ls())

require(foreign)

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)

require(rgeos)
require(maptools)
require(rgdal)
require(sp)
require(spdep)

require(vegan)

require(lattice)
require(latticeExtra)
require(ggplot2)
require(RColorBrewer)
require(ggmap)



# Aggregate count data to datazones and IGs for 2001 and 2010 -------------



# What now? 
# Need to look again at the NSPD to understand variables better

# Key point from Ellie's long email

#Postcode directories for Nov 2010 and earlier list 
# 1) the number of address counts 
#defined as numeric count taken from PSA (number of addresses EXCLUDING small 
# businesses/non-residential)
# AND
# 2) the number of delivery points 
# total number of delivery counts


paf_2001 <- read.csv(
    file="E:/Data/postcode_address_files/unzipped/2001/afg2001feb.csv",
    header=FALSE
    ) %>%
    tbl_df
# labels now extracted from the manual. Do they match

# My best guess is 

paf_2001 <- paf_2001 %>%
    rename(
        PCD = V1,
        PCD2 = V2,
        DOINTR = V3,
        DOTERM = V4,
        OSCTY = V5,
        OSLAUA = V6,
        OSWARD = V7,
        USERTYPE = V8,
        OSEAST100M = V10,
        OSNRTH100M = V11,
        OSEAST1M = V12,
        OSNRTH1M = V13,
        OSGRDIND = V14,
        OLDHA = V15,
        HRO = V16,
        CTRY = V17,
        GENIND = V18,
        PAFIND = V19,
        GOR = V20 ,
        STREG = V21,
        TTWA = V22,
        EER = V23,
        TECLEC = V24,
        NUTS = V27,
        PSED = V28,
        CENED = V29,
        ADDRCT = V31,
        DPCT = V32,
        SMLBUSCT = V34,
        ASHAPREV = V35,
        LEA = V36
        )



links <- read.spss(file="E:/Dropbox/Data/Links_between_Areal_Units/latestpcinfowithlinkpc.sav", 
                   use.value.labels = TRUE,
                   to.data.frame = TRUE 
                   ) %>%
    tbl_df

links <- links %>%
    select(PostcodeFull, Datazone, INTERMED)
links$pcd <- str_replace(links$PostcodeFull, "\\s+", "") %>% str_trim

paf_2001$pcd <- str_replace(paf_2001$PCD, "\\s+", "")  %>% str_trim 
paf_2001_linked <- paf_2001 %>%
    inner_join(links)
# this works. Now to save a simplified version with only the vars of interest

paf_2001_dz <- paf_2001_linked %>%
    select(postcode=pcd, datazone=Datazone, address_count = ADDRCT, 
           deliverypoint_count = DPCT, smallbus_count = SMLBUSCT
           ) %>%
    group_by(datazone) %>%
    summarise(
        address_count = sum(address_count),
        deliverypoint_count = sum(deliverypoint_count),

        smallbus_count = sum(smallbus_count)
        ) 

# remove blank dz

paf_2001_dz <- paf_2001_dz[str_detect(paf_2001_dz$datazone, "^S01"),]

# Same for igs

paf_2001_ig <- paf_2001_linked %>%
    select(postcode=pcd, intermed=INTERMED, address_count = ADDRCT, 
           deliverypoint_count = DPCT, smallbus_count = SMLBUSCT
    ) %>%
    group_by(intermed) %>%
    summarise(
        address_count = sum(address_count),
        deliverypoint_count = sum(deliverypoint_count),

        smallbus_count = sum(smallbus_count)
    ) 

# remove blank dz

paf_2001_ig <- paf_2001_ig[str_detect(paf_2001_ig$intermed, "^S02"),]

# Same for igs

write.csv(paf_2001_dz, file="data/derived/building_use_counts_2001_datazone.csv", row.names=F)

write.csv(paf_2001_ig, file="data/derived/building_use_counts_2001_intermed.csv", row.names=F)


# Now to do the same for 2010

paf_2010 <- read.csv(
    file="E:/Data/postcode_address_files/unzipped/2010/NSPDF_FEB_2010_UK_1M_FP.csv",
    header=FALSE
) %>%
    tbl_df
# Will need to rematch (may be easier as the doc may be the correct one this time)


labels <- read.csv(file="E:/Data/postcode_address_files/labels.csv") %>%
    tbl_df

names(paf_2010) <- labels$Field.name

# Great. this makes it very easy
# datazones are DZONE1, intermeds are DZONE2

# Vars of itnerest are 
#ADDRCT
#DPCT
#MOCT
#SMLBUSCT


paf_2010_dz <- paf_2010 %>%
    select(
        datazone=DZONE1, intermed=DZONE2, 
        address_count = ADDRCT, deliverypoint_count = DPCT, 
        smallbus_count = SMLBUSCT
           ) %>%
    group_by(datazone) %>%
    summarise(
        address_count=sum(address_count, na.rm=T),
        deliverypoint_count = sum(deliverypoint_count, na.rm=T),
        
        smallbus_count = sum(smallbus_count, na.rm=T)
              )

paf_2010_dz <- paf_2010_dz[str_detect(paf_2010_dz$datazone, "^S01"),]


paf_2010_ig <- paf_2010 %>%
    select(
        datazone=DZONE1, intermed=DZONE2, 
        address_count = ADDRCT, deliverypoint_count = DPCT, 
        smallbus_count = SMLBUSCT
    ) %>%
    group_by(intermed) %>%
    summarise(
        address_count=sum(address_count, na.rm=T),
        deliverypoint_count = sum(deliverypoint_count, na.rm=T),
        smallbus_count = sum(smallbus_count, na.rm=T)
    )

paf_2010_ig <- paf_2010_ig[str_detect(paf_2010_ig$intermed, "^S02"),]

# Now to write out 

write.csv(paf_2010_dz, file="data/derived/building_use_counts_2010_datazone.csv",
          row.names=FALSE)

write.csv(paf_2010_ig, file="data/derived/building_use_counts_2010_intermed.csv",
          row.names=FALSE)

          

# simple explorations/sense testing of aggregated data----------------------------------

rm(list=ls())

paf_2001_dz <- read.csv(file="data/derived/building_use_counts_2001_datazone.csv") %>%
    tbl_df
paf_2001_ig <- read.csv(file="data/derived/building_use_counts_2001_intermed.csv") %>%
    tbl_df

paf_2010_dz <- read.csv(file="data/derived/building_use_counts_2010_datazone.csv") %>%
    tbl_df
paf_2010_ig <- read.csv(file="data/derived/building_use_counts_2010_intermed.csv") %>%
    tbl_df


# Now to see if the counts are similar from 2001 to 2010

paf_2001_ig %>%
    select(intermed, dp_2001=deliverypoint_count) %>%
    inner_join(paf_2010_ig) %>%
    select(intermed, dp_2001, dp_2010=deliverypoint_count) %>%
    ggplot(data=., aes(x=dp_2001, y=dp_2010)) +
    geom_point(aes(alpha=0.1)) + 
    stat_smooth(method="lm")

paf_2001_ig %>%
    select(intermed, ac_2001=address_count) %>%
    inner_join(paf_2010_ig) %>%
    select(intermed, ac_2001, ac_2010=address_count) %>%
    ggplot(data=., aes(x=ac_2001, y=ac_2010)) +
    geom_point(aes(alpha=0.1)) + 
    stat_smooth(method="lm")

paf_2001_ig %>%
    select(intermed, sb_2001=smallbus_count) %>%
    inner_join(paf_2010_ig) %>%
    select(intermed, sb_2001, sb_2010=smallbus_count) %>%
    ggplot(data=., aes(x=sb_2001, y=sb_2010)) +
    geom_point(aes(alpha=0.1)) + 
    stat_smooth(method="lm")



# Plot patterns of building types in Greater Glasgow -----------------------

# Look at IGs to start with

# Analyses of urban and rural class


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

# Now, to look at how these counts add up

paf_2001_ig  %>% mutate(total = address_count + smallbus_count, dif =deliverypoint_count - total)  %>% sample_n(10)

# Diffs are mostly zero, sometimes negative
# This implies the negatives refer to shared use. 
# This could be important, but I'm not sure how to go about coding it.
# Instead I'll pretend the two mutually exclusive categories as 
# addresses and small businesses

# Want to categorise areas by proportion of total addresses that 
# are small businesses 

sb_props <- paf_2001_ig %>%
    mutate(
        prop_sb = smallbus_count / (smallbus_count + address_count),
        sb_dec = ntile(prop_sb, 10)
        ) %>%
    select(intermed, prop_sb, sb_dec)


ig_gg_shp@data <- merge(ig_gg_shp, sb_props, by.x="zonecode", by.y="intermed")

spplot(ig_gg_shp, zcol="prop_sb", 
       colorkey=TRUE, 
       col="transparent"
)

# How about datazones?

sb_props <- paf_2001_dz %>%
    mutate(
        prop_sb = smallbus_count / (smallbus_count + address_count),
        sb_dec = ntile(prop_sb, 10)
    ) %>%
    select(datazone, prop_sb, sb_dec)


dz_gg_shp@data <- merge(dz_gg_shp, sb_props, by.x="zonecode", by.y="datazone")

spplot(dz_gg_shp, zcol="prop_sb", 
       colorkey=TRUE, 
       col="transparent"
)

# Now what about diversity?


paf_2001_dz$diversity <- paf_2001_dz  %>% 
    select(address_count, smallbus_count)  %>% 
    as.matrix %>%
    diversity


dz_gg_shp@data <- merge(dz_gg_shp, paf_2001_dz, by.x="zonecode", by.y="datazone")

spplot(dz_gg_shp, zcol="diversity", 
       colorkey=TRUE, 
       col="transparent"
)

# Now IG 
paf_2001_ig$diversity <- paf_2001_ig  %>% 
    select(address_count, smallbus_count)  %>% 
    as.matrix %>%
    diversity


ig_gg_shp@data <- merge(ig_gg_shp, paf_2001_ig, by.x="zonecode", by.y="intermed")

spplot(ig_gg_shp, zcol="diversity", 
       colorkey=TRUE, 
       col="transparent"
)

# Working hypothesis: place diversity should be linked to density of the areal units

# to test this I need to extract the area size for each areal unit

dz_ardiv <- data.frame(
    datazone=dz_gg_shp@data$zonecode,
    diversity=dz_gg_shp@data$diversity,
    area= sapply(dz_gg_shp@polygons, function(x) x@area)
    )

qplot(x=area, y=diversity, data=dz_ardiv, log="x") + stat_smooth()

summary(lm(diversity ~ log(area), data=dz_ardiv)) 
# According to this, a slight stat sig POSITIVE association between area and diversity
# (i.e. opposite direction to what I expected)

# What about IG?

ig_ardiv <- data.frame(
    datazone=ig_gg_shp@data$zonecode,
    diversity=ig_gg_shp@data$diversity,
    area= sapply(ig_gg_shp@polygons, function(x) x@area)
)

qplot(x=area, y=diversity, data=ig_ardiv, log="x") + stat_smooth()
summary(lm(diversity ~ log(area), data=ig_ardiv))
# No association whatsoever at IG level.

# Conclusion: this doesn't seem to be a factor.



# Some attempts at using ggmap --------------------------------------------


# To do : 
# zoom out of 'Glasgow' slightly

tmp <- qmap("Glasgow", zoom=9)
# transform and fortify ig_gg_shp, do choropleth

ig_gg_fort <- ig_gg_shp
ig_gg_fort <- spTransform(ig_gg_fort, CRS("+proj=longlat +datum=WGS84"))
ig_gg_fort <- fortify(ig_gg_fort, region="zonecode")
ig_gg_fort <- ig_gg_fort %>%
    left_join(ig_gg_shp@data, by=c("id"="zonecode"))

png("figs/place_diversity.png", res=300, width=10, height=8, units="in")
gx <- tmp + geom_polygon(
    aes(x=long, y= lat, group=id, fill=diversity),
    data=ig_gg_fort,
    alpha=0.7
    )
print(gx)

dev.off()
