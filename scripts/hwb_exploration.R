# First look at NHS Health and Wellbeing research

rm(list=ls())

require(spdep)
require(rgeos)
require(maptools)
require(rgdal)

require(RColorBrewer)


require(lattice)
require(latticeExtra)


require(foreign)
require(tidyr)
require(stringr)
require(dplyr)



dta <- read.spss("E:/data/ak_hwb11_extract//HWB11 Master (AK March14).sav", 
                 to.data.frame = TRUE,
                 use.value.labels=TRUE,
                 trim.factor.names=TRUE,
                 trim_values=TRUE,
                 ) %>%
    tbl_df

dtazone_count <- dta %>%
    xtabs(~datazone, data=.) %>%
    as.data.frame


dtazone_count$datazone <- dtazone_count$datazone %>%
    as.character %>%
    str_trim

dtazone_count <- dtazone_count %>%
    rename(freq=Freq)

quantile(dtazone_count$freq, c(0.025, 0.5, 0.975))

# 95% of datazones contain between 5 and 27 observations, median is 10 obs

# Now to plot these on a map...

hist(dtazone_count$freq, 
     breaks=50, 
     main="histogram of number of observations\nper datazone", 
     xlab="Number of observations in datazone")

# so what are the datazones with massive oversampling?

dtazone_count$datazone[dtazone_count$freq > 27]
dtazone_count$datazone[dtazone_count$freq > 120]

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

dz_gg_shp@data <- merge(dz_gg_shp@data, dtazone_count, by.x="zonecode", by.y="datazone", all.x=TRUE)


spplot(
    dz_gg_shp, 
    zcol="freq", 
    col="lightgrey", colorkey=TRUE)


g1 <- spplot(dz_gg_shp, zcol="freq", 
             colorkey=TRUE, 
             col="transparent"
)
g2 <- spplot(
    ig_gg_shp, 
    zcol="zonecode", col.regions="transparent",
    col="lightgrey",
    colorkey=FALSE
)

g1 + g2

