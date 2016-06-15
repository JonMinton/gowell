# Mapping of diversity 

rm(list = ls())


#install.packages("pacman")

require(pacman)
p_load(
    readr, stringr, 
    purrr, tidyr, dplyr, 
    ggplot2, 
    tmap, rgeos, sp,
    xlsx
    )



# The aim of this is to be able to explore diversity scores for each datazone 

# Some specific tasks 

# 1) Show UR class on a map
# 1a) reduce to only greater glasgow
# 1b) Produce UR class labels 
# 1c) Produce map for GG only


# 2) Show standard map and cartogram

# 3) Amend colour schemes 

# 4) Place Title above bounding box

# 5) Produce t1 t2 side by side




# Tasks now  --------------------------------------------------------------


# 1. Show UR class on a map --------------------------------------------------

ur_class <- read_csv("data/derived/dz_2001_by_ur_6fold_class.csv")
ur_class %>% 
    mutate(ur_label = factor(
        ur_class, 
        levels = c(1, 2, 3, 5, 6), 
        labels = c("large_urban", "other_urban", "accessible small towns", "accessible_rural", "remote_rural")
    )
    ) -> ur_class

shp <- read_shape(file = "data/shp/scotland_2001_datazones/scotland_dz_2001.shp")

shp_ur_join <- append_data(shp = shp, data = ur_class, key.shp = "zonecode", key.data = "dz_2001")



tm_shape(shp_ur_join, borders = NULL) + 
    tm_fill("ur_label", title = "Urban Rural Class") + # Note - important to add title at this stage rather than later
    tm_legend(
        title.size = 2.0,
        text.size = 1.5
    ) -> this_map

save_tmap(this_map, 
          filename = "maps/Scotland_urbanrural_standard.png", 
          width = 20, height = 35, units = "cm", dpi=300
          )


# As above, but just for Glasgow

dzs_in_gg <- read_csv("data/geographies/dzs_in_greater_glasgow.csv")
dzs_in_gg %>% 
    left_join(ur_class) %>% 
    select(dz_2001, ur_class, ur_label) -> ur_gg_only


append_data(
    shp = shp, data = ur_gg_only, 
    key.shp = "zonecode", key.data = "dz_2001"
    ) -> tmp
shp_ur_gg <- tmp[!is.na(tmp@data$ur_class),]
rm(tmp)

tm_shape(shp_ur_gg, borders = NULL) + 
    tm_fill("ur_label", title = "Urban Rural Class") + # Note - important to add title at this stage rather than later
    tm_legend(
        title.size = 2.0,
        text.size = 1.5
    ) + 
    tm_scale_bar(position = c("right", "top")) -> this_map

save_tmap(this_map, 
          filename = "maps/gg_urbanrural_standard.png", 
          width = 30, height = 30, units = "cm", dpi=300
)

# Let's add some major road networks

roads <- read_shape(file = "data/shp/scotland-roads-shape/roads.shp")

# clip the roads to within shp_ur_gg

# From http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
gClip <- function(shp, bb){
    if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
    else b_poly <- as(extent(bb), "SpatialPolygons")
    gIntersection(shp, b_poly, byid = T)
}

# Now need to find the bounding box of shp_ur_gg and apply to roads

this_bb <- bbox(shp_ur_gg)

roads_clipped <- gClip(roads, this_bb)
roads_clipped()

this_map + 
    tm_shape(roads) + 
    tm_lines()

# To display this interactively using leaflet
# tmap_mode("view")
# this_map
# 
# # try the same with html
# save_tmap(this_map, filename = "Scotland_urbanrural_standard.html") 
# # This sort of works, though not if you want to save to a subdirectory



# .... --------------------------------------------------------------------



diversity_H <- read_csv("data/derived/p_all_H.csv")

dta <- diversity_H %>% select(-land_bus) %>% gather(key = "category", value = "h", tenure:land_vacant) 
shp <- read_shape(file = "data/shp/scotland_2001_datazones/scotland_dz_2001.shp")

# Let's try nesting this 

dta_nested <- dta %>% group_by(period, category) %>% nest()

fn <- function(DTA){
    out <- append_data(shp, DTA, key.shp = "zonecode", key.data = "datazone") 
    out <- out[!is.na(out$h),]
    return(out)
}

dta_nested <- dta_nested %>% mutate(shp = map(data, fn))

fn <- function(TITLE, SHP){
    graph <- qtm(SHP, fill = "h", borders = NULL, title = TITLE)
    return(graph)
}

dta_nested <- dta_nested %>% 
    mutate(TITLE = paste0(period, ", ", category)) %>% 
    mutate(this_map = map2(TITLE, shp, fn))





