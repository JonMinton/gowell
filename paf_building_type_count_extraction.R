# Postcode address files - exploring how to use to create 
# measures of land use diversity at datazone and intermediate geography level

rm(list=ls())

require(foreign)

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)

require(lattice)
require(sp)
require(ggplot2)


# Data 

# Postcode address best fit

postcode_links <- read.csv(
    file="E:/data/postcode_address_files/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv"
    ) %>%
    tbl_df
# NOTE: This just seems to contain English places. Need to look for a different file.

rm(postcode_links)
gc()


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
        MOCT = V33,
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
           deliverypoint_count = DPCT, multipleocc_count = MOCT, smallbus_count = SMLBUSCT
           ) %>%
    group_by(datazone) %>%
    summarise(
        address_count = sum(address_count),
        deliverypoint_count = sum(deliverypoint_count),
        multipleocc_count = sum(multipleocc_count),
        smallbus_count = sum(smallbus_count)
        ) 

# remove blank dz

paf_2001_dz <- paf_2001_dz[str_detect(paf_2001_dz$datazone, "^S01"),]

# Same for igs

paf_2001_ig <- paf_2001_linked %>%
    select(postcode=pcd, intermed=INTERMED, address_count = ADDRCT, 
           deliverypoint_count = DPCT, multipleocc_count = MOCT, smallbus_count = SMLBUSCT
    ) %>%
    group_by(intermed) %>%
    summarise(
        address_count = sum(address_count),
        deliverypoint_count = sum(deliverypoint_count),
        multipleocc_count = sum(multipleocc_count),
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
        multipleocc_count = MOCT, smallbus_count = SMLBUSCT
           ) %>%
    group_by(datazone) %>%
    summarise(
        address_count=sum(address_count, na.rm=T),
        deliverypoint_count = sum(deliverypoint_count, na.rm=T),
        multipleocc_count = sum(multipleocc_count, na.rm=T),
        smallbus_count = sum(smallbus_count, na.rm=T)
              )

paf_2010_dz <- paf_2010_dz[str_detect(paf_2010_dz$datazone, "^S01"),]


paf_2010_ig <- paf_2010 %>%
    select(
        datazone=DZONE1, intermed=DZONE2, 
        address_count = ADDRCT, deliverypoint_count = DPCT, 
        multipleocc_count = MOCT, smallbus_count = SMLBUSCT
    ) %>%
    group_by(intermed) %>%
    summarise(
        address_count=sum(address_count, na.rm=T),
        deliverypoint_count = sum(deliverypoint_count, na.rm=T),
        multipleocc_count = sum(multipleocc_count, na.rm=T),
        smallbus_count = sum(smallbus_count, na.rm=T)
    )

paf_2010_ig <- paf_2010_ig[str_detect(paf_2010_ig$intermed, "^S02"),]

# Now to write out 

write.csv(paf_2010_dz, file="data/derived/building_use_counts_2010_datazone.csv",
          row.names=FALSE)

write.csv(paf_2010_ig, file="data/derived/building_use_counts_2010_intermed.csv",
          row.names=FALSE)

          
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
        MOCT = V33,
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
           deliverypoint_count = DPCT, multipleocc_count = MOCT, smallbus_count = SMLBUSCT
    ) %>%
    group_by(datazone) %>%
    summarise(
        address_count = sum(address_count),
        deliverypoint_count = sum(deliverypoint_count),
        multipleocc_count = sum(multipleocc_count),
        smallbus_count = sum(smallbus_count)
    ) 

# remove blank dz

paf_2001_dz <- paf_2001_dz[str_detect(paf_2001_dz$datazone, "^S01"),]

# Same for igs

paf_2001_ig <- paf_2001_linked %>%
    select(postcode=pcd, intermed=INTERMED, address_count = ADDRCT, 
           deliverypoint_count = DPCT, multipleocc_count = MOCT, smallbus_count = SMLBUSCT
    ) %>%
    group_by(intermed) %>%
    summarise(
        address_count = sum(address_count),
        deliverypoint_count = sum(deliverypoint_count),
        multipleocc_count = sum(multipleocc_count),
        smallbus_count = sum(smallbus_count)
    ) 

# remove blank dz

paf_2001_ig <- paf_2001_ig[str_detect(paf_2001_ig$intermed, "^S02"),]

# Same for igs

write.csv(paf_2001_dz, file="data/derived/building_use_counts_2001_datazone.csv", row.names=F)

write.csv(paf_2001_ig, file="data/derived/building_use_counts_2001_intermed.csv", row.names=F)


# Now to do the same for 2010

