library(tidyverse)
library(lubridate)
library(sf)


# Load buffered afrob buffered points
load(file = here::here("processed", "afrob_sf_buffers.Rdata"))

# prio grid
grid <- read_sf(here::here("prio", "priogrid_cell.shp"))

# yearly data
yearly <- read_csv(here::here("prio", "yearly.csv"))
yearly <- yearly %>% group_by(gid) %>% arrange(gid, year) %>% 
  mutate(year = year + 1) %>% 
  rename_at(vars(agri_ih:water_ih), list(~paste0("lag_",.)))


# Fill years forward
t15 <- yearly %>% filter(year == 2014) %>% 
  mutate(year = 2015)
t16 <- yearly %>% filter(year == 2014) %>% 
  mutate(year = 2016)
t17 <- yearly %>% filter(year == 2014) %>% 
  mutate(year = 2017)
t18 <- yearly %>% filter(year == 2014) %>% 
  mutate(year = 2018)
yearly <- bind_rows(yearly, t15, t16, t17, t18)

# fill gpw forward
yearly <- 
  yearly %>% group_by(gid) %>% 
  tidyr::fill(lag_pop_gpw_sum) %>% 
  ungroup()

# static
static <- read_csv(here::here("prio", "static.csv"))

# join afrob clusters to prio grid
afrob_sf_buffers <- map(.x = afrob_sf_buffers,
                        ~.x %>% 
                          as_tibble() %>% 
                          st_as_sf(sf_column_name = "geometry"))

st_crs(afrob_sf_buffers[[1]]) <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
grid <- st_transform(grid, st_crs(afrob_sf_buffers[[1]] %>% st_as_sf()))
buffers_grid <- st_join(afrob_sf_buffers[[1]] %>% st_as_sf(), grid)

# join to prio data
buffers_grid2 <-
  buffers_grid %>% 
  mutate(year = lubridate::year(date)) %>% 
  left_join(static, by = c("gid", "xcoord", "ycoord", "col", "row")) %>% 
  left_join(yearly, by = c("gid", "year")) %>% 
  dplyr::select(-year)


buffers_grid3 <-
  buffers_grid2 %>% st_drop_geometry() %>% 
  group_by(geoid, date, n) %>% 
  summarise_all(., list(~mean(., na.rm = T)))

gc()

# join to afrob tidy
load(file = here::here("processed", "afrob_tidy2.Rdata"))

afrob_tidy_full <-
  afrob_tidy2 %>% mutate(year = lubridate::year(date)) %>% 
  left_join(., select(buffers_grid3, -NAME_0), by = c("geoid", "date")) %>% 
  # select(-NAME_0.y, NAME_0 = NAME_0.x) %>% 
  select(NAME_0, everything())


rm(buffers_grid, buffers_grid2, buffers_grid3, grid, yearly)
gc()


scaler <- function(x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}
df_analysis <- afrob_tidy_full %>% 
  mutate(Lights = scaler(log1p(lag_nlights_mean)),
         Population = scaler(log1p(lag_pop_gpw_sum)),
         id1 = paste(NAME_0, "_", NAME_1),
         id2 = paste(NAME_0, "_", NAME_2)) 


save(df_analysis, file = here::here("processed", "df_analysis2.Rdata"))




