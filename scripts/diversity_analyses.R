# Diversity analysis



rm(list=ls())

# Prereqs -----------------------------------------------------------------


require(repmis)
require(tidyr)
require(stringr)
require(plyr)
require(dplyr)
require(vegan)
require(ggplot2)
require(xtable)
require(corrplot)



# Data --------------------------------------------------------------------


p_all_H <- read.csv("data/derived/p_all_H.csv") %>%
    tbl_df


p_all_S <- read.csv("data/derived/p_all_S.csv") %>%
    tbl_df



# Analyses: H -------------------------------------------------------------


# Descriptive stats -------------------------------------------------------

p_all_H %>%
    group_by(period) %>%
    summarise_each(funs(round(mean(., na.rm=T), 2)), -datazone, -period) %>%
    write.csv("clipboard")

p_all_H %>%
    group_by(period) %>%
    summarise_each(funs(round(sd(., na.rm=T), 2)), -datazone, -period) %>%
    write.csv("clipboard")


fn <- function(x){
    xx <- as.matrix(x[,-c(1,2)])
    out <- cor(xx, use="pairwise.complete.obs") %>%
        round(2)
    
    return(out)
}

H_corrs  <- dlply(p_all_H, .(period), fn)

png("figures/corrplot_t1.png", width=20, height=20, res=300, unit="cm")
H_corrs[["t1"]]  %>% 
    corrplot(., type="lower", main="\n\nDiversity using H, Around 2001", diag=FALSE, method=c("number"))
dev.off()

png("figures/corrplot_t2.png", width=20, height=20, res=300, unit="cm")
H_corrs[["t2"]]  %>% 
    corrplot(., type="lower", main="\n\nDiversity using H, Around 2011", diag=FALSE, method=c("number"))
dev.off()
# Table of correlations, t1 -----------------------------------------------

tab <- H_corrs[["t1"]]
print(xtable(tab), type="html", file="tables/Correlations_between_entropy_levels_t1.html")

# Table of correlations, t2 -----------------------------------------------
tab <- H_corrs[["t2"]]
print(xtable(tab), type="html", file="tables/Correlations_between_entropy_levels_t2.html")



# PCA, around 2001 --------------------------------------------------------

fit_t1 <- p_all_H %>%
    filter(period=="t1") %>%
    filter(complete.cases(.)) %>%
    select(-datazone, -period) %>%
    prcomp(., cor=T)



# table of H loadings around 2001 -----------------------------------------

fit_t1$rotation  %>% 
    round(2)  %>%
    xtable %>%
    print(type="html", file="tables/entropy_factor_loadings_t1.html")


# Graph of H loadings around 2001 -----------------------------------------

fit_t1$rotation  %>% 
    round(2)  %>% 
    as.data.frame  %>%
    mutate(type=rownames(.))  %>% 
    gather(key="pc", value="loading", -type)  %>% 
    ggplot(data=.) + 
    geom_point(aes(x=loading, y= type)) + 
    facet_wrap( ~ pc) + 
    geom_segment(
        aes(
            y=type, yend=type, 
            x=ifelse(loading < 0, loading, 0), 
            xend=ifelse(loading > 0, loading, 0)
        )
    ) + 
    labs(x="Factor loading on Principal Component", y="Entropy Type", title="Factor loadings around 2001") +
    theme(plot.title=element_text(face = "bold"))
ggsave("figures/h_factor_loadings_pca_around_2001.png", dpi=300, height=30, width=30, unit="cm")



# pca around 2011 ---------------------------------------------------------

fit_t2 <- p_all_H %>%
    filter(period=="t2") %>%
    filter(complete.cases(.)) %>%
    select(-datazone, -period) %>%
    prcomp(., cor=T)



# table of H loadings around 2011 -----------------------------------------


fit_t2$rotation  %>% 
    round(2)  %>%
    xtable %>%
    print(type="html", file="tables/entropy_factor_loadings_t2.html")

print(xtable(tab), type="html", file="tables/e0_mean_1750_onwards.html")



# Graph of H loadings around 2011 -----------------------------------------

fit_t2$rotation  %>% 
    round(2)  %>% 
    as.data.frame  %>%
    mutate(type=rownames(.))  %>% 
    gather(key="pc", value="loading", -type)  %>% 
    ggplot(data=.) + 
    geom_point(aes(x=loading, y= type)) + 
    facet_wrap( ~ pc) + 
    geom_segment(
        aes(
            y=type, yend=type, 
            x=ifelse(loading < 0, loading, 0), 
            xend=ifelse(loading > 0, loading, 0)
        )
    ) + 
    labs(x="Factor loading on Principal Component", y="Entropy Type", title="Factor loadings around 2011") +
    theme(plot.title=element_text(face = "bold"))
ggsave("figures/h_factor_loadings_pca_around_2011.png", dpi=300, height=30, width=30, unit="cm")

# first three pcs from both periods

pc_t1 <- fit_t1$rotation %>%
    round(2) %>%
    as.data.frame %>%
    mutate(type=rownames(.)) %>%
    gather(key="pc", value="loading", -type) %>%
    tbl_df %>%
    mutate(period="t1") %>%
    select(period, type, pc, loading)
    
pc_t2 <- fit_t2$rotation %>%
    round(2) %>%
    as.data.frame %>%
    mutate(type=rownames(.)) %>%
    gather(key="pc", value="loading", -type) %>%
    tbl_df %>%
    mutate(period="t2") %>%
    select(period, type, pc, loading)

pc_both <- pc_t1  %>% bind_rows(pc_t2)

pc_both %>%
    filter(pc %in% c("PC1", "PC2", "PC3")) %>%
    ggplot(data=.) +
    geom_point(aes(x=loading, y= type)) + 
    geom_segment(
        aes(
            y=type, yend=type, 
            x=ifelse(loading < 0, loading, 0), 
            xend=ifelse(loading > 0, loading, 0),
            group=period
        )
    ) + 
    facet_grid(period ~ pc) +
    labs(x="Factor loading on Principal Component", y="Entropy Type", 
         title="First three factor loadings, around 2001 and 2011") +
    theme(plot.title=element_text(face = "bold"))
    
ggsave("figures/first_3_factors_t1t2.png", 
       width=15, height=15, unit="cm", dpi=300
       )



# Determinants of overall diversity ---------------------------------------


require(rpart)

p_all_H$h_mean <- apply(p_all_H[,c(-1, -2)], 1, function(x) mean(x, na.rm=T)) 

fit <- p_all_H[complete.cases(p_all_H),] %>%
    filter(period=="t1") %>%
    rpart(h_mean ~ tenure + bld_size + land_vacant,
          method="class", data=.)

plot(fit)
text(fit, use.n=T, all=T, cex=0.8)         
