library(tidyverse)
library(sf)
source(here::here("code", "funs.R"))



# Load merged geocoded data -----------------------------------------------
load(here::here("processed", "afrob_merge.Rdata"))

afrob_merge <-
  afrob_merge %>% 
  rename(date = ymd) %>% 
  dplyr::select(country, date, everything())


### Location ###
joined1 <- get_africa(level = 1) %>% 
  dplyr::select(NAME_0, NAME_1, HASC_1)

joined2 <- get_africa(level = 2) %>% 
  dplyr::select(NAME_0, NAME_1, NAME_2)


joinedtemp <- joined1 %>% 
  filter(NAME_0 %in% c("Western Sahara" , "Seychelles" , "Mauritius" , 
                       "Mayotte" , "Cape Verde" , "Comoros" , 
                       "French Southern Territories" , "Lesotho" , "Libya")) %>% 
  mutate(NAME_2 = NAME_1)

proj <- sf::st_crs(joined2)
joined2 <- 
  bind_rows(joined2, joinedtemp)

# Convert to sf
afrob_merge <- 
  afrob_merge %>% 
  mutate(geoid = paste(ylong, xlat, sep = ", ")) %>% 
  filter(!is.na(ylong)) %>% 
  st_as_sf(., coords = c("ylong", "xlat"),
           crs = proj)



# Match to ADM2s
afrob_merge_sf <- st_join(afrob_merge, joined2) %>% 
  dplyr::select(country, date, NAME_0, NAME_1, NAME_2, everything())

afrob_merge_sf <-
  afrob_merge_sf %>% 
  mutate(country = case_when(country == "LIb" ~ "LIB", TRUE ~ country))


# Add in Vdem from vdemdata package
vdem <-
  vdemdata::vdem %>% 
  select("country_name", "year", "COWcode", 
         "v2x_regime", "v2x_polyarchy", "v2x_libdem")

# Looking for mismatches
unique(afrob_merge_sf$NAME_0)[-which(unique(afrob_merge_sf$NAME_0) %in% unique(vdem$country_name))]

vdem <- 
  vdem %>% 
  mutate(NAME_0 = case_when(country_name == "Ivory Coast" ~ "Côte d'Ivoire",
                            country_name == "Eswatini" ~ "Swaziland",
                            country_name == "Republic of the Congo"  ~ "Republic of Congo",
                            country_name == "The Gambia" ~ "Gambia",
                            country_name == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
                            TRUE ~ country_name))

afrob_merge_sf <- afrob_merge_sf %>% 
  mutate(year = lubridate::year(date)) %>% 
  left_join(vdem, by = c("NAME_0", "year")) %>% 
  mutate(v2x_regime = case_when(NAME_0 == "Western Sahara" ~ 0,
                                TRUE ~ v2x_regime))

# Save
save(afrob_merge_sf, file = here::here("processed", "afrob_merge_sf.Rdata"))



