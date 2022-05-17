### Goal: match GTD event locations to spatio-temporal afrob buffers ### 
library(tidyverse)
library(sf)
library(lubridate)
library(data.table)


# Load data ---------------------------------------------------------------

# Load buffered afrob buffered points
load(file = here::here("processed", "afrob_sf_buffers.Rdata"))
afrob_sf_buffers <- map(.x = afrob_sf_buffers,
                        ~.x %>% 
                          as_tibble() %>% 
                          st_as_sf(sf_column_name = "geometry"))


st_crs(afrob_sf_buffers[[1]]) <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
st_crs(afrob_sf_buffers[[2]]) <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
st_crs(afrob_sf_buffers[[3]]) <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
st_crs(afrob_sf_buffers[[4]]) <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

# These are lists of each unique point-date, by buffer size
head(afrob_sf_buffers[[1]])

# Load gtd points; add date and subset to recent years
load(file = here::here("attacks", "gtd_sf.Rdata"))
gtd_sf <- gtd_sf %>% 
  mutate(gtd_date = lubridate::ymd(paste(iyear, imonth, iday, sep = "-"))) %>% 
  filter(iyear > 1997)

# Islamist groups
gnames <- readxl::read_excel(here::here("attacks", "gtd_groupnames_islamist.xlsx"),
                             sheet = 1)
# Only islamist attacks
gtd_sf <- 
  gtd_sf %>% 
  left_join(gnames, by = c("NAME_0" = "country",
                           "gname" = "group")) %>% 
  select(Islamist, everything()) %>% 
  filter(Islamist == 1)


# Match up projections (albers africa) for spatial join
gtd_sf <- st_transform(gtd_sf, "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")



# Buffers to only unique obs per location to match with gtd
afrob_sf_buffers2 <- map(.x = afrob_sf_buffers, 
                         ~.x %>% st_as_sf() %>% 
                           group_by(geoid, NAME_0) %>% 
                           tally())

# Identify each buffer that intersects with each GTD event for each spatial window
# This includes any overlap between an event and a buffer, regardless of timing
gtd_buffers <- 
  map(.x = afrob_sf_buffers2, ~st_join(gtd_sf, .x) %>% 
        st_drop_geometry() %>% 
        filter(!is.na(geoid)) %>% 
        setDT())



# Create data.table for month lag indicators; only need 1
afrob.dt <- afrob_sf_buffers[[1]] %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>%  
  mutate(
    lag_1 = date %m-% months(1),
    lag_2 = date %m-% months(2),
    lag_3 = date %m-% months(3),
    lag_4 = date %m-% months(4),
    lag_5 = date %m-% months(5),
    lag_6 = date %m-% months(6),
    lag_7 = date %m-% months(7),
    lag_8 = date %m-% months(8),
    lag_9 = date %m-% months(9),
    lag_10 = date %m-% months(10),
    lag_11 = date %m-% months(11),
    lag_12 = date %m-% months(12),
    
    lead_1 = date %m+% months(1),
    lead_2 = date %m+% months(2),
    lead_3 = date %m+% months(3),
    lead_4 = date %m+% months(4),
    lead_5 = date %m+% months(5),
    lead_6 = date %m+% months(6),
    lead_7 = date %m+% months(7),
    lead_8 = date %m+% months(8),
    lead_9 = date %m+% months(9),
    lead_10 = date %m+% months(10),
    lead_11 = date %m+% months(11),
    lead_12 = date %m+% months(12),
    lead_max = lubridate::ymd("2021-01-01"),
    date2 = date) %>% 
  setDT()




# Map a data.table join on geoid and conditional on a date window
# This should all be wrapped into a single function...

# All attacks
# Lags
afrob_lag1 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_1)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '1 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag2 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_2)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '2 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))



afrob_lag3 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_3)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '3 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag4 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_4)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '4 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag5 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_5)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '5 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag6 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_6)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '6 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag7 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_7)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '7 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag8 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_8)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '8 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag9 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_9)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '9 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag10 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_10)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = '10 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag11 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_11)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = '11 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag12 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_12)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = '12 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))



# Leads
afrob_lead1 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_1)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 1 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead2 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_2)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 2 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))



afrob_lead3 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_3)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 3 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead4 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_4)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 4 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead5 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_5)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 5 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead6 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_6)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 6 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead7 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_7)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 7 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead8 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_8)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 8 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead9 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_9)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = 'lead 9 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead10 <- imap(.x = gtd_buffers, 
                     ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_10)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid, date) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                       mutate(temporal_window = 'lead 10 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead11 <- imap(.x = gtd_buffers, 
                     ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_11)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid, date) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                       mutate(temporal_window = 'lead 11 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lead12 <- imap(.x = gtd_buffers, 
                     ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_12)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid, date) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                       mutate(temporal_window = 'lead 12 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))


afrob_lead99 <- imap(.x = gtd_buffers, 
                     ~.x[afrob.dt, on = .(geoid, gtd_date > date2, gtd_date <= lead_max)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid, date) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                       mutate(temporal_window = '99 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))




# All attacks
attacks_buff <- bind_rows(afrob_lag3,
                          # afrob_lead3,
                          afrob_lag6, 
                          # afrob_lead6,
                          afrob_lag12,
                          # afrob_lead12,
                          afrob_lead99) %>% 
  # Cleanup the naming of the spatial and temporal identifiers
  mutate(spatial_window = forcats::fct_inorder(case_when(spatial_window == 1 ~ "10km",
                                                         spatial_window == 2 ~ "25km",
                                                         spatial_window == 3 ~ "50km",
                                                         spatial_window == 4 ~ "100km")),
         temporal_window = forcats::fct_inorder(temporal_window)) %>% 
  # Force back into an sf object
  st_as_sf()

rm(afrob_lag3, afrob_lag6, afrob_lag12, afrob_lead3, afrob_lead6, afrob_lead12)
# save(attacks_buff, file = here::here("processed", "attacks_buff_islamist.Rdata"))

df_attacks_isl <- attacks_buff %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  mutate(windows = paste0(temporal_window, "-", spatial_window)) %>% 
  select(geoid, date, NAME_0, windows, attacks) %>% 
  pivot_wider(., names_from = "windows", 
              values_from = "attacks", 
              names_prefix = "isl_attacks") %>% 
  janitor::clean_names()



df_attacks_isl <- 
  df_attacks_isl %>% 
  mutate(
    
    
    future_isl_attacks3_months_10km = case_when(isl_attacks3_months_10km == 0 & isl_attacks99_months_10km > 0 ~ 1,
                                                TRUE ~ 0),
    future_isl_attacks6_months_10km = case_when(isl_attacks6_months_10km == 0 & isl_attacks99_months_10km > 0 ~ 1,
                                                TRUE ~ 0),
    future_isl_attacks12_months_10km = case_when(isl_attacks12_months_10km == 0 & isl_attacks99_months_10km > 0 ~ 1,
                                                 TRUE ~ 0),
    
    
    future_isl_attacks3_months_25km = case_when(isl_attacks3_months_25km == 0 & isl_attacks99_months_25km > 0 ~ 1,
                                                TRUE ~ 0),
    future_isl_attacks6_months_25km = case_when(isl_attacks6_months_25km == 0 & isl_attacks99_months_25km > 0 ~ 1,
                                                TRUE ~ 0),
    future_isl_attacks12_months_25km = case_when(isl_attacks12_months_25km == 0 & isl_attacks99_months_25km > 0 ~ 1,
                                                 TRUE ~ 0),
    
    future_isl_attacks3_months_50km = case_when(isl_attacks3_months_50km == 0 & isl_attacks99_months_50km > 0 ~ 1,
                                                TRUE ~ 0),
    future_isl_attacks6_months_50km = case_when(isl_attacks6_months_50km == 0 & isl_attacks99_months_50km > 0 ~ 1,
                                                TRUE ~ 0),
    future_isl_attacks12_months_50km = case_when(isl_attacks12_months_50km == 0 & isl_attacks99_months_50km > 0 ~ 1,
                                                 TRUE ~ 0),
    
    
    future_isl_attacks3_months_100km = case_when(isl_attacks3_months_100km == 0 & isl_attacks99_months_100km > 0 ~ 1,
                                                 TRUE ~ 0),
    future_isl_attacks6_months_100km = case_when(isl_attacks6_months_100km == 0 & isl_attacks99_months_100km > 0 ~ 1,
                                                 TRUE ~ 0),
    future_isl_attacks12_months_100km = case_when(isl_attacks12_months_100km == 0 & isl_attacks99_months_100km > 0 ~ 1,
                                                  TRUE ~ 0))




save(df_attacks_isl, 
     file = here::here("processed", 
                       "df_attacks_isl.Rdata"))



