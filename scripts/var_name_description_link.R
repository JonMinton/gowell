

# A plyr function that, extracts all variable names from each of the files

rm(list=ls())

require(plyr)
require(dplyr)
require(stringr)

base_dir <- "G:/dropbox/Dropbox/Data/SNS/rearranged_data"


all_files <- list.files(base_dir, include.dirs=T, recursive=T, pattern=".csv$")
all_files <- all_files[-1]
all_files <- all_files[-length(all_files)]

fn <- function(x){
    file_name <- paste(base_dir, x, sep="/")
    
    this_file <- read.csv(file_name)
    
    min_year <- min(this_file$year)
    max_year <- max(this_file$year)
    var_names <- this_file %>% 
        select(-datazone, -year) %>% 
        names
    
    tmp <- str_split(x, "/")[[1]]
    if (length(tmp)!=2){ 
        out <- NULL
    } else {
        dir_name <- tmp[1]
        file_name <- str_replace(tmp[2], ".csv$", "")
        
        out <- data.frame(
            dir_name=dir_name,
            file_name=file_name,
            var_name=var_names,
            min_year=min_year,
            max_year=max_year
            ) 
    }    
    return(out)
}

output <- ldply(all_files, fn, .progress="text")

output <- output %>% tbl_df %>% filter(!(var_name %in% c("year_first", "year_last", "quarter")))


write.csv(output, file="G:/dropbox/Dropbox/Data/SNS/rearranged_data/var_names_by_location.csv", row.names=F)


#### Now to merge files 

location_id <- output 

id_desc <- read.csv("G:/dropbox/Dropbox/Data/SNS/rearranged_data/all_variable_names.csv")
id_desc <- id_desc %>% tbl_df
id_desc$simple_id <- id_desc$Identifier %>% tolower %>% str_replace_all("[-.]", "_")

location_id$simple_id <- location_id$var_name %>% tolower %>% str_replace_all("[-.]", "_")

location_id_desc <- location_id %>% left_join(id_desc)
location_id_desc <- location_id_desc %>% select(var_name, description=ShortTitle,min_year, max_year, file_name, dir_name)

write.csv(location_id_desc, file="G:/dropbox/Dropbox/Data/SNS/rearranged_data/var_names_and_dir_locations.csv", row.names=F)
