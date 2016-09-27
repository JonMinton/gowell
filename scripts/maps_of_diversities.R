# Mapping of diversity 

rm(list = ls())


#install.packages("pacman")

require(pacman)
p_load(
    readr, stringr, 
    purrr, tidyr, dplyr, 
    ggplot2, 
    tmap, rgeos, sp, maptools,
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

# Need to set common projection

roads <- set_projection(roads, projection = get_projection(shp_ur_gg))

roads_in_gg <- raster::crop(roads, bb(shp_ur_gg))

# Road network and UR class for GG

tm_shape(shp_ur_gg, borders = NULL) + 
    tm_fill("ur_label", title = "Urban Rural Class") + # Note - important to add title at this stage rather than later
    tm_legend(
        title.size = 2.0,
        text.size = 1.5
    ) + 
    tm_shape(roads_in_gg) + 
    tm_lines(col = "grey", alpha = 0.4) -> map_urclass_glasgow_roads

# TAKES AGES - BUT
#tmap_mode("view")
# map_urclass_glasgow_roads
# 
# save_tmap(map_urclass_glasgow_roads, 
#           filename = "maps/gg_urbanrural_standard_roads.png", 
#           width = 30, height = 30, units = "cm", dpi=300
# )


# Cartogram? 
ur_class <- read_csv("data/derived/dz_2001_by_ur_6fold_class.csv")

ur_class %>% 
    mutate(ur_label = factor(
        ur_class, 
        levels = c(1, 2, 3, 5, 6), 
        labels = c("large urban", "other urban", "accessible small towns", "accessible rural", "remoterural")
    )
    ) -> ur_class

shp_cart <- read_shape(file = "data/cartogram/Scotland_2001_population_cartogram.shp")

cart_ur_join <- append_data(shp = shp_cart, data = ur_class, key.shp = "zonecode", key.data = "dz_2001")


tm_shape(cart_ur_join, projection = "longlat") + 
    tm_fill("ur_label", title = "Urban Rural Class") +
    tm_legend(
        title.size = 2.0,
        text.size = 1.5
    )  -> map_cart_ur_scotland
# This works, just remember to set a projection

save_tmap(map_cart_ur_scotland,
          filename = "maps/scotland_cartogram_ur.png",
          width = 20, height = 30, units = "cm", dpi=300
)

# Now, to do a cartogram for GG only 
dzs_in_gg  %>% .$dz_2001 -> tmp
cart_ur_ggonly <- cart_ur_join[cart_ur_join$zonecode %in% tmp,]
rm(tmp)

tm_shape(cart_ur_ggonly, projection = "longlat") + 
    tm_fill("ur_label", title = "Urban Rural Class") +
    tm_legend(
        title.size = 2.0,
        text.size = 1.5
    ) -> map_cart_ur_glasgow

save_tmap(map_cart_ur_glasgow,
          filename = "maps/glasgow_cartogram_ur.png",
          width = 25, height = 25, units = "cm", dpi=300
)

# This ALSO WORKS - GREAT! 


# Now a standard map, with a border around the GG zone

shp_ur_join <- append_data(
    shp_ur_join, dzs_in_gg, 
    key.shp = "zonecode", key.data = "dz_2001")

tmp <- shp_ur_join@data
tmp  %>% mutate(in_gg = !is.na(dz_2001.data))  -> tmp
tmp -> shp_ur_join@data
rm(tmp)

shp_gg_only <- shp_ur_join[shp_ur_join$in_gg,]
shp_gg_only <- unionSpatialPolygons(shp_gg_only, IDs = shp_gg_only$chp)
# This shows the separate health boards 

# Note unionSpatialPolygons returns a SpatialPolygons object not a spatialpolygons DF
# to convert back
#http://gis.stackexchange.com/questions/61633/r-convert-a-spatial-polygon-objet-to-spatial-polygon-data-frame
tmp_df <- data.frame(id = getSpPPolygonsIDSlots(shp_gg_only))
row.names(tmp_df) <- getSpPPolygonsIDSlots(shp_gg_only)

shp_gg_only <- SpatialPolygonsDataFrame(shp_gg_only, data = tmp_df)
rm(tmp_df)

# To get the labels for each health partnership
chp_lookup <- read_csv("data/geographies/greater_glasgow_definitions_simplified.csv")

short_name <- c(
    "East Dunbartonshire",
    "East Glasgow",
    "East Renfrewshire",
    "Inverclyde",
    "North Glasgow",
    "North Lanarkshire",
    "Renfrewshire",
    "South East Glasgow",
    "South Lanarkshire",    
    "South West Glasgow",
    "West Dunbartonshire",
    "West Glasgow"
)


chp_lookup %>% mutate(short_label = short_name) -> chp_lookup
rm(short_name)

shp_gg_only <- append_data(shp = shp_gg_only, data = chp_lookup, key.shp = "id", key.data = "chcp_code")

tm_shape(shp_ur_gg) + 
    tm_fill("ur_label", title = "Urban Rural Class") + 
    tm_shape(shp_gg_only) + 
    tm_borders(lwd = 2) + 
    tm_legend(
        title.size = 2.0,
        text.size = 1.5
    ) + 
    tm_text("short_label", shadow = T) -> map_gg_ur_healthboard


save_tmap(map_gg_ur_healthboard,
          filename = "maps/glasgow_healthboard_ur.png",
          width = 25, height = 25, units = "cm", dpi=300
)



# To display this interactively using leaflet
# tmap_mode("view")
# this_map
# 
# # try the same with html
# save_tmap(this_map, filename = "Scotland_urbanrural_standard.html") 
# # This sort of works, though not if you want to save to a subdirectory



# Produce maps of diversities  --------------------------------------------------------------------


dzs_in_gg <- read_csv("data/geographies/dzs_in_greater_glasgow.csv")

all_diversities <- read_csv("data/derived/all_diversities.csv")

all_diversities %>% 
    filter(!is.na(year)) %>% 
    group_by(category) %>% 
    mutate(
        nr_2001 = abs(year - 2001),
        nr_2011 = abs(year - 2011),
        
        mn_2001 = nr_2001 == min(nr_2001),
        mn_2011 = nr_2011 == min(nr_2011),
        
        period = ifelse(mn_2001, "t1", ifelse(mn_2011, "t2", NA))
    ) %>% 
    select(-nr_2001, -nr_2011, -mn_2001, -mn_2011) %>% 
    ungroup() -> tidied_diversities 



dta <- tidied_diversities %>% filter(category != "land_bus") 

shp <- read_shape(file = "data/shp/scotland_2001_datazones/scotland_dz_2001.shp")
shp_cart <- read_shape(file = "data/cartogram/Scotland_2001_population_cartogram.shp", current.projection = "longlat")

# Let's try nesting this 

dta_nested <- dta %>% filter(!is.na(period)) %>% group_by(period, category) %>% nest()

fn <- function(DTA, SHP){
    DTA %>% select(datazone, simpson) -> DTA 
    out <- append_data(SHP, DTA, key.shp = "zonecode", key.data = "datazone") 
    out <- out[!is.na(out$simpson),]
    return(out)
}


dta_nested <- dta_nested %>% 
    mutate(
        shp = map(data, fn, SHP = shp)
        ) 
    

fn <- function(TITLE, SHP){
    this_map <- tm_shape(SHP) + 
        tm_layout(
            title = TITLE, 
            title.position = c("right", "top"), 
            title.bg.color = "lightgrey"
        ) +  
        tm_fill(
            "simpson", 
            n = 5,
            max_categories = 10,
            palette = "Spectral", 
            title = "Diversity Scores", 
            legend.hist = T,
            style = "fixed",
            breaks = 0:10/10
            ) + 
        tm_shape(shp_gg_only) + 
        tm_borders(lwd = 2)
        
    return(this_map)
}


dta_nested <- dta_nested %>% 
    mutate(tmp1 = ifelse(period == "t1", "2001", "2011"),
           TITLE = paste0(tmp1, ", ", category)) %>% 
    mutate(this_map = map2(TITLE, shp, fn))

# Now to print out 

fn <- function(FILENAME, MAP){
    full_filename = paste0("maps/diversities/diversity_", FILENAME, ".png")
    
    save_tmap(MAP,
              filename = full_filename,
              width = 15, height = 15, units = "cm", dpi=300
    )
    return(NULL)
}


# Interestingly the walk function needs to be within the mutate function even though it 
# doesn't return anything... 

dta_nested %>% 
    filter(!is.na(period)) %>% 
    mutate(filenm = paste0(category, "_", period)) %>% 
    mutate(walk2(filenm, this_map, fn))



# Now to try the same kind of thing but with cartograms 


fn <- function(DTA, SHP){
    DTA %>% select(datazone, simpson) -> DTA 
    out <- append_data(SHP, DTA, key.shp = "zonecode", key.data = "datazone") 
    out <- out[!is.na(out$simpson),]
    return(out)
}

dta_nested <- dta_nested %>% 
    mutate(
        shp_cart = map(data, fn, SHP = shp_cart)
    ) 



fn <- function(TITLE, SHP){
    this_map <- tm_shape(SHP, projection = "longlat") + 
        tm_fill(
            "simpson", 
            n = 5,
            max_categories = 10,
            palette = "Spectral", 
            title = "Diversity Scores", 
            legend.hist = T,
            style = "fixed",
            breaks = 0:10/10
        ) +
        tm_layout(
            title = TITLE, 
            title.position = c("left", "bottom"), 
            title.bg.color = "lightgrey"
        )  + 
        tm_legend(
            position = c("right", "top"),
            frame = T,
            legend.width = 0.30, legend.height = 0.35,
            bg.color = "lightgrey"
        )
    
    return(this_map)
}

    
dta_nested <- dta_nested %>% 
    mutate(tmp1 = ifelse(period == "t1", "2001", "2011"),
           TITLE = paste0(tmp1, ", ", category)) %>% 
    mutate(this_cart = map2(TITLE, shp_cart, fn))


# Now to print out 

fn <- function(FILENAME, MAP){
    full_filename = paste0("maps/diversities/cartogram_diversity_", FILENAME, ".png")
    
    save_tmap(MAP,
              filename = full_filename,
              width = 20, height = 20, units = "cm", dpi=300
    )
    return(NULL)
}

dta_nested %>% 
    select(-tmp1) %>% 
    mutate(filenm = paste0(category, "_", period)) %>% 
    mutate(walk2(filenm, this_cart, fn))