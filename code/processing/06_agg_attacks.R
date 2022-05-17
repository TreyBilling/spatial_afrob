library(tidyverse)
library(sf)
source(here::here("code", "funs.R"))

# Load data ---------------------------------------------------------------
joined1 <- get_africa(level = 1)
load(file = here::here("attacks", "gtd_sf.Rdata"))


# Spatial join and aggregate to adm1-month counts -------------------------
gtd_sf <- gtd_sf %>% 
  mutate(gtd_date = lubridate::ymd(paste(iyear, imonth, iday, sep = "-"))) %>% 
  filter(iyear > 1997)

gtd_agg <- 
  gtd_sf %>% 
  dplyr::select(country_txt, gtd_date) %>% 
  st_join(., joined1) %>% 
  st_drop_geometry() %>% 
  group_by(NAME_0, NAME_1, gtd_date) %>% 
  count()

gtd_agg <- 
  gtd_agg %>% 
  group_by(NAME_0, NAME_1, gtd_date) %>% 
  summarise(attacks_agg = sum(n))

gtd_agg_full <-
  expand_grid(id = paste(joined1$NAME_0, joined1$NAME_1, sep = ";"),
              gtd_date = seq(lubridate::ymd("1998-01-01"),
                             lubridate::ymd("2018-12-01"),
                             by = "day")) %>% 
  mutate(NAME_0 = stringr::str_split(id, pattern = ";", simplify = T)[,1],
         NAME_1 = stringr::str_split(id, pattern = ";", simplify = T)[,2]) %>% 
  dplyr::select(-id) %>% 
  left_join(., gtd_agg) %>% 
  mutate(attacks_agg = case_when(is.na(attacks_agg) ~ as.numeric(0), TRUE ~ as.numeric(attacks_agg)))


# Create lags -------------------------------------------------------------

### From Romain Francois https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/ ###

lags <- function(var, n){
  library(rlang)
  var <- enquo(var)
  
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("lag_%s_%02d", quo_text(var), indices))
}            


gtd_agg_full <- 
  gtd_agg_full %>% 
  arrange(NAME_0, NAME_1, gtd_date) %>% 
  group_by(NAME_0, NAME_1) %>% 
  mutate(!!!lags(attacks_agg, 12))

gtd_agg_full <-
  gtd_agg_full %>% 
  mutate(attacks_agg3 = lag_attacks_agg_01 + lag_attacks_agg_02 + lag_attacks_agg_03,
         attacks_agg6 = attacks_agg3 + lag_attacks_agg_04 + 
           lag_attacks_agg_05 + lag_attacks_agg_06,
         attacks_agg12 = attacks_agg6 + lag_attacks_agg_07 + 
           lag_attacks_agg_08 + lag_attacks_agg_09 + lag_attacks_agg_10 + 
           lag_attacks_agg_11 + lag_attacks_agg_12) %>% 
  select(gtd_date, NAME_0, NAME_1, attacks_agg3:attacks_agg12)



save(gtd_agg_full, file = here::here("processed", "gtd_agg_full.Rdata"))

