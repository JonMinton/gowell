# A script to calculate diversity scores according to a range of dimensions 
# using SNS and 2011 census data





# Occupational Class From Census --------------------------------------------------


rm(list=ls())

require(repmis)
require(tidyr)
require(stringr)
require(plyr)
require(dplyr)
require(vegan)
require(ggplot2)

# Functions 

# Entropy calculation
H <- function(xx){
    N <- ncol(xx)
    p <- apply(xx, 1, function(x) (x+0.5)/sum(x))
    out <- -1 * apply(p, 2, function(x) sum(x * log(x))/N)
    return(out)
}



################

#Other types of diversity
# 1) Housing tenure diversity 


# DONE


#### Additional sources of diversity?

# Candidates from SNS


# 2011 Census Tables ------------------------------------------------------


# The following tables have been found in the 2011 Census

# KS 204 SC     Country of birth [already done]
# QS 203 SC     Country of birth [already done]
# KS 501 SC     Highest qualification
# KS 605 SC     Industry
# QS 605 SC     Industry
# KS 206 SC     Language
# QS 702 SC     Method of Travel to Work
# KS 202 SC     National Identity
# KS 608 SC     Occupation
# QS 606 SC     Occupation
# KS 209 SCb    Religion
# KS 209 SCa    Religion (UK Harmonised)
# QS 101 SC     Residence Type
# KS 402 SC     Tenure




rm(list=ls())

require(repmis)
require(tidyr)
require(stringr)
require(plyr)
require(dplyr)
require(vegan)
require(ggplot2)





