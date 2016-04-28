rm(list = ls())



require(readr)
require(stringr)

require(plyr)
require(tidyr)
require(dplyr)

require(ggplot2)
require(tmap)

# Initial tasks: 

# 1) Calculate density for each DZ in GG
# 2) Calculate distance to city centre for each DZ in GG
# 3) Extract and attach urban-rural classification to each DZ in GG 


# Task 1: calculate density -----------------------------------------------

# Using tmap::read_shape

scotland_dz <- re("data/shp/scotland_2001_datazones/scotland_dz_2001.shp")
# 1) Calculate density for each DZ in GG
# 2) Calculate distance to city centre for each DZ in GG
# 3) Extract and attach urban-rural classification to each DZ in GG 

# Comments from Ade Feb 2016

# Hi Jon,
# 
# Good to see you today; let’s not leave it so long this time!
#     
#     Here’s my hand-written note – attached.
# 
# I think we agreed to do the following:
#     
# - Look at the geography of diversity (at each of the two time points?) in terms of:
# o   SG urban-rural 6-fold classification.
# o   Distance of datazone to city centre.
# o   Density (spatial area and population of datazone).





# 
# -  Look at relative change over time in each measure of diversity 
# for each quintile of deprivation (to produce a table such as sketched 
#                                   on the second page attached).
# 
# -  Recalculate the diversity measures for 2011 using 
# three categories for each diversity dimension, to make the scores comparable. 
# Producing median scores for each SIMD quintile and for all datazones together.
# 
# - Include household type as a diversity dimension 
# (possibly using three categories? 
# Adult hhds; 
# family hhds; 
# older person households – 

# depends what the census already provides and how you can combine existing categories).
# 
# We thought we would leave the question of how different dimensions of diversity are associated with each other across datazones for later.
# 
# Once we have this lot to hand, we’ll meet to go over the results and plan the writing of the first paper.
# 
# You also thought of having a look at what health measures you can access for datazones with a view to the next paper on health outcomes associated with diversity.
# 
# Yours,
# 
