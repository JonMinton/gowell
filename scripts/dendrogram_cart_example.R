rm(list = ls())


#install.packages("pacman")

require(pacman)
p_load(
    readr, stringr, 
    purrr, tidyr, dplyr, 
    ggplot2, tmap, rgeos,
    xlsx,
    ape
    )




simd <- read_csv("data/simd/00410767.csv")

#View(simd)
rescale <- function(x) {
    (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}
simd_by_la <- simd %>% 
    select(datazone = `Data Zone`, 
           la_name = `Local Authority Name`, 
           simd_overall = `Overall SIMD 2012 Score`, 
           simd_income = `Income domain 2012 rate (%)`, 
           simd_employment = `Employment domain 2012 rate (%)`,
           simd_health = `Health domain 2012 score`,
           simd_skills = `Education, Skills and Training domain 2012 score`, 
           simd_housing = `Housing domain score 2004, 2006, 2009 & 2012`, 
           simd_geog = `Geographic Access domain 2012 score`,
           simd_crime = `SIMD Crime 2012 score`
           ) %>% 
    select(-datazone) %>% 
    group_by(la_name) %>% 
    mutate_each(funs(rescale)) %>% 
    summarise_each(funs(mean(., na.rm = T)))  




# https://rpubs.com/gaston/dendrograms
class(simd_by_la) <- "data.frame"
rownames(simd_by_la) <- simd_by_la$la_name
simd_by_la$la_name <- NULL


hc <- hclust(dist(simd_by_la))

plot(hc)


# Example of CART 

# http://www.statmethods.net/advstats/cart.html


simd_simple <- simd %>% 
    select(datazone = `Data Zone`, 
           la_name = `Local Authority Name`, 
           simd_overall = `Overall SIMD 2012 Score`, 
           simd_income = `Income domain 2012 rate (%)`, 
           simd_employment = `Employment domain 2012 rate (%)`,
           simd_health = `Health domain 2012 score`,
           simd_skills = `Education, Skills and Training domain 2012 score`, 
           simd_housing = `Housing domain score 2004, 2006, 2009 & 2012`, 
           simd_geog = `Geographic Access domain 2012 score`,
           simd_crime = `SIMD Crime 2012 score`
    )  


fit <- rpart(
    simd_overall ~ simd_income + simd_employment + simd_health + simd_skills,
    data = simd_simple, method = "anova"
    )

plot(fit, uniform=TRUE, 
     main="Regression Tree for SIMD ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

