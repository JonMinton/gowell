# Postcode address files - exploring how to use to create 
# measures of land use diversity at datazone and intermediate geography level

rm(list=ls())

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

labels <- read.csv(
    file="E:/Data/postcode_address_files/labels.csv"
    ) %>%
    tbl_df

# dimensions are not the same: 53 and 36

