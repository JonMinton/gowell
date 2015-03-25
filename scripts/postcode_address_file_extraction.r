# the purpose of this script is to use data from the 2001 and 2010 postcode 
# address files to produce estimates of residential/non-residential mix at datazone 
# level. 

# The files themselves are far too large to include locally, or even on dropbox, 
# so instead will only be accessible locally, within the office. 


rm(list=ls())

require(tidyr)
require(stringr)
require(dplyr)


dta_2001 <- read.csv("E:/data/postcode_address_files/unzipped/2001/afg2001feb.csv", header=F) %>%
    tbl_df


PCD
PCD2
PCDS
DOINTR
DOTERM
OSCTY
OSLAUA
OSWARD
USERTYPE
OSEAST1M
OSEAST100M
OSNRTH1M
OSNRTH100M
OSGRDIND
OSHLTHAU
HRO
CTRY
GENIND
PAFIND
GOR
STREG
PCON
EER
TECLEC
TTWA
PCT
NUTS
PSED
CENED
EDIND
ADDRCT
DPCT
MOCT
SMLBUSCT
OSHAPREV
LEA
OLDHA
WARDC91
WARDO91
WARD98
STATSWARD
OACODE
OAIND
