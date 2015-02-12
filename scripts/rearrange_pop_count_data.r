# Script to manage population count data

# want males and females from sns

require(repmis)
require(plyr)
require(tidyr)
require(stringr)
require(dplyr)
require(ggplot2)
require(car)

# Females

females_raw <- source_DropboxData(
    file="females.csv",
    key="qq083qk9iah5txz"
    )

# males

males_raw <- source_DropboxData(
    file="males.csv",
    key="n77e4r376gz368e"
    )

# people (to check against)
# persons_raw <- source_DropboxData(
#     file="persons.csv",
#     key="vcz7qngb44vbynq"
#     )

females_raw <- females_raw %>% tbl_df()
males_raw <- males_raw %>% tbl_df()

# let's combine

combined_raw <- males_raw %>% 
    full_join(females_raw)

rm(females_raw, males_raw)
gc()
# take 1000 rows at a time, re-arrange, save to file

for (i in )
fn <- function(x){
    out <- x %>% 
        select(datazone=datazone, year=year, matches["0-9"]) %>%
        gather(key=var_name)
    
    
    write.table(out, file=XXX, append=T, sep=",")
}

combined_raw <- combined_raw %>% 
    select(datazone=datazone, year=year, matches("[0-9]")) %>%
    gather(key=var_name, value=count, -datazone, -year)

combined_raw <- combined_raw %>% 
    filter(!is.na(count))


combined_raw %>% sample_n(50)

# Want to remove rows wehere variable name ends with 
# an uppercase F or M

combined_raw <- combined_raw %>%
    filter(
        !str_detect(combined_raw$var_name, "[M|F]$")
        )

combined_raw$sex <- NA
combined_raw$sex[str_detect(combined_raw$var_name, "fem")] <- "female"
combined_raw$sex[str_detect(combined_raw$var_name, "mal")] <- "male"

# Now want to extract from the first to the last number

combined_raw$numeric_part <- str_extract(combined_raw$var_name, "[0-9].*$")

combined_raw <- combined_raw %>% mutate(
    n2= 
    ifelse(
        str_length(combined_raw$numeric_part)==4,
        str_c(str_sub(combined_raw$numeric_part, 1,2), "_", str_sub(combined_raw$numeric_part, 3,4)),
        ifelse(
            combined_raw$numeric_part=="15", 
            "15_15",
            ifelse(
               str_length(combined_raw$numeric_part)==2,
               str_c(str_sub(combined_raw$numeric_part, 1,1), "_", str_sub(combined_raw$numeric_part, 2, 2)),
               ifelse(
                   str_detect(combined_raw$numeric_part, "over$"),
                   str_c(str_sub(combined_raw$numeric_part, 1, 2), "_", "101"), 
                   combined_raw$numeric_part
                   )
               )
            )
        ) 
    )
# forgot about this
combined_raw$n2[combined_raw$n2=="0"] <- "0_0"

# can now drop var_name and numeric_part
combined_raw <- combined_raw %>% 
    select(datazone, year, sex, age_range=n2, count) %>%
    separate(col=age_range, into=c("lower_age", "upper_age"), "_", remove=F)
write.csv(combined_raw, file="data/derived/populations_by_age_year_sex.csv", row.names=F)
# Now want to write this out


# key terms to look out for are peop, mal, and fe
# also anything numeric


