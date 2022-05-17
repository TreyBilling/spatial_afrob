library(tidyverse)
library(sf)
source(here::here("code", "funs.R"))

load(here::here("processed", "afrob_merge_sf.Rdata"))

# ADM1s in Africa
joined1 <- get_africa(level = 1)
joined0 <- get_africa(level = 0)



# Re-project and buffer ---------------------------------------------------

# Albers projection in Africa is 102022
joined1 <- st_transform(joined1, crs = 102022)
joined0 <- st_transform(joined0, crs = 102022)
afrob_merge_sf <- st_transform(afrob_merge_sf, crs = 102022)

# Map through 10km, 25km, 50,000km, 100,000km buffers
afrob_sf_unique <- afrob_merge_sf %>% # Unique clusters instead of all points individually
  group_by(geoid, date, NAME_0) %>% 
  tally() 

# This creates a list of 4 dataframes, one for each spatial buffer 
afrob_sf_buffers <- map(.x = c(10000, 25000, 50000, 100000), 
                        ~st_buffer(afrob_sf_unique, dist = .x))


# save
save(afrob_sf_buffers, file = here::here("processed", "afrob_sf_buffers.Rdata"))

