# insert sampling features
library(dplyr)
library(rgdal) # spatial data types
library(rgeos) # spatial 
library(sf) # spatial
library(uuid) # unique ids
library(readr)
library(tidyr)

survey_points <- read_csv("survey_final_klh.csv")

# separate out inundation boundary points from sites
# ib points will be results
ib <- survey_points %>% 
  dplyr::filter(Description == "Inundation Outline")

new_sites <- survey_points %>% 
  dplyr::filter(Description != "Inundation Outline")

new_sites <- new_sites %>% 
  mutate(samplingfeaturecode = paste(Wetland, Description))

# check to see if there are any sites without codenames
new_sites[which(is.na(new_sites$samplingfeaturecode)),]
new_sites <- new_sites %>% dplyr::filter(Description != "Test Point")

# check to see if there are any duplicate sampling codes
new_sites[duplicated(new_sites$samplingfeaturecode),]
dnames <- new_sites$samplingfeaturecode[duplicated(new_sites$samplingfeaturecode)]

# for duplicates, change samplingfeaturecode to include number

site_duplicates <- new_sites %>% 
  dplyr::filter(samplingfeaturecode %in% dnames) %>%
  dplyr::arrange(samplingfeaturecode) %>%
  group_by(samplingfeaturecode) %>%
  mutate(dup_no = min_rank(Survey_ID)) %>%
  mutate(samplingfeaturecode_dup = paste(samplingfeaturecode, dup_no)) 

# replace duplicate names in new sites data frame
# this needs to be sorted correctly
# merge on survey ID
new_sites2 <- left_join(new_sites, 
                        dplyr::select(site_duplicates, Survey_ID, samplingfeaturecode_dup)
                        # by = c("Survey_ID" = "Survey_ID")
                        )
new_sites2[!is.na(new_sites2$samplingfeaturecode_dup), "samplingfeaturecode"] <- 
  new_sites2[!is.na(new_sites2$samplingfeaturecode_dup), "samplingfeaturecode_dup"]

new_sites <- new_sites2 %>% dplyr::select(-samplingfeaturecode_dup)

# check again to see if there are any duplicate sampling feature codes
new_sites[duplicated(new_sites$samplingfeaturecode),]

# hooray!
# add survey id and date to the description column
new_sites %<>%
  mutate(samplingfeaturedescription = paste("Survey point", Survey_ID, "measured on", Date, "as", Wetland, Description)) %>%
  dplyr::select(POINT_X, POINT_Y, samplingfeaturecode, samplingfeaturedescription)

# save new file that meets requirements of having unique codenames

write_csv(new_sites, "../maps/survey_samplingfeatures.csv")

