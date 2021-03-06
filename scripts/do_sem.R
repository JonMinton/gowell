# SEM using lavaan package

rm(list=ls())
# Prereqs -----------------------------------------------------------------


require(plyr)
require(tidyr)
require(dplyr)

require(lavaan)

require(ggplot2)


# Data --------------------------------------------------------------------

all_H <- read.csv("data/derived/all_H.csv") %>%
    tbl_df

all_S <- read.csv("data/derived/all_S.csv") %>%
    tbl_df

p_all_H <- read.csv("data/derived/p_all_H.csv") %>%
    tbl_df

p_all_S <- read.csv("data/derived/p_all_S.csv") %>%
    tbl_df




# sem proper --------------------------------------------------------------

model <- '
    # latent variables
    bld =~ bld_band + bld_size + bld_type
    demo =~ demo_as + demo_eth + demo_rel + demo_coo
    econ =~ econ_act + econ_sec + econ_ind
    land =~ land_vacant + land_bus

'

p_all_H %>%
    filter(complete.cases(.)) %>%
    filter(period=="t1") %>%
    cfa(model, data=.) -> fit_t1

p_all_H %>%
    filter(complete.cases(.)) %>%
    filter(period=="t2") %>%
    sem(model, data=.) -> fit_t2


p_all_H %>%
    filter(complete.cases(.)) %>%
    filter(period=="t1") %>%
    select(econ_act, econ_sec, econ_ind) %>%
    pairs(., panel=panel.smooth)

p_all_H %>%
    filter(complete.cases(.)) %>%
    filter(period=="t2") %>%
    select(econ_act, econ_sec, econ_ind) %>%
    pairs(., panel=panel.smooth)


