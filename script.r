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




