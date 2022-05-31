library(tidyverse)
library(lfe)
library(sf)
library(cowplot)
library(kableExtra)

# Setup -------------------------------------------------------------------

# Data
load(here::here("processed", "df_analysis2.Rdata"))


# Inverse hyperbolic sine transformation for attack variables
ihs <- function(x) {
  log(x + sqrt(x ^ 2 + 1))
}

df_analysis <-
  df_analysis %>% 
  mutate(across(starts_with("attacks"),
                ~ihs(.),
                .names = "ihs_{.col}"))



# Dummy for regime type
df_analysis <- 
  df_analysis %>% 
  mutate(regime2 = case_when(v2x_regime < 2 ~ 0,
                             v2x_regime > 1 ~ 1,
                             TRUE ~ NA_real_),
         DA_supdem_dum = case_when(DA_supdem == 3 ~ 1,
                                   DA_supdem < 3 ~ 0,
                                   TRUE ~ NA_real_),
         SD_demhappy_dum = case_when(SD_demhappy > 2 ~ 1, 
                                     SD_demhappy < 3 ~ 0,
                                     TRUE ~ NA_real_)
         
  )

# Setup FE and trend variables
df_analysis <- 
  df_analysis %>% 
  mutate(time = as.numeric(year - 2001),
         round = as.factor(round),
         id1 = as.factor(id1),
         id2 = as.factor(id2))


# Ever attacks indicators
df_analysis <-
  df_analysis %>% 
  group_by(id1) %>% 
  mutate(across(starts_with("attacks"),
                ~as.numeric(sum(., na.rm = T) > 0),
                .names = "ever_{.col}"))

evertreated <- c(
  "ever_attacks3_months_10km",
  "ever_attacks3_months_25km",
  "ever_attacks3_months_50km",
  "ever_attacks3_months_100km",
  "ever_attacks6_months_10km",
  "ever_attacks6_months_25km",
  "ever_attacks6_months_50km",
  "ever_attacks6_months_100km",
  "ever_attacks12_months_10km",
  "ever_attacks12_months_25km",
  "ever_attacks12_months_50km",
  "ever_attacks12_months_100km"
)

# IHS IVs
ivsihs <- c(
  "ihs_attacks3_months_10km",
  "ihs_attacks3_months_25km",
  "ihs_attacks3_months_50km",
  "ihs_attacks3_months_100km",
  "ihs_attacks6_months_10km",
  "ihs_attacks6_months_25km",
  "ihs_attacks6_months_50km",
  "ihs_attacks6_months_100km",
  "ihs_attacks12_months_10km",
  "ihs_attacks12_months_25km",
  "ihs_attacks12_months_50km",
  "ihs_attacks12_months_100km")


futuresivs <- c(
  "future_attacks3_months_10km",
  "future_attacks3_months_25km",
  "future_attacks3_months_50km",
  "future_attacks3_months_100km",
  "future_attacks6_months_10km",
  "future_attacks6_months_25km",
  "future_attacks6_months_50km",
  "future_attacks6_months_100km",
  "future_attacks12_months_10km",
  "future_attacks12_months_25km",
  "future_attacks12_months_50km",
  "future_attacks12_months_100km")




# Models ------------------------------------------------------------------


out_support <-
  tibble(ivs = ivsihs,
         future = futuresivs) %>% 
  mutate(out = map2(.x = ivs, 
                    .y = future,
                    ~former(dv = "DA_supdem",
                            iv = paste(.x, "+", .y),
                            covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                            spec = "| round + id1 | 0 | geoid") %>% 
                      felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)),
         coef1 = map(outtab, ~.x[["estimate"]][[1]]),
         se1 = map(outtab, ~.x[["std.error"]][[1]]),
         coef2 = map(outtab, ~.x[["estimate"]][[2]]),
         se2 = map(outtab, ~.x[["std.error"]][[2]]),
         difference = map(outtab, ~.x[["estimate"]][[1]] - .x[["estimate"]][[2]]),
         ftest = map(.x = out,
                     ~car::linearHypothesis(.x, 
                                            paste(attr(.x[["terms"]], "term.labels")[[1]],
                                                  "=",
                                                  attr(.x[["terms"]], "term.labels")[[2]]),
                                            vcov = .x[["vcv"]],
                                            white.adjust = "hc1",
                                            test = "F")[2,]),
         n = map_dbl(.x = out, ~.x[["N"]]),
         ADM1 = map_dbl(.x = out, ~.x[["fe"]][["id1"]] %>% unique() %>% length()),
         Rounds = map_dbl(.x = out, ~.x[["fe"]][["round"]] %>% unique() %>% length()),
         Clusters = map_dbl(.x = out, ~.x[["clustervar"]][[1]] %>% unique() %>% length()),
         R2 = map_dbl(.x = out, ~summary(.x)[["r2"]]))

suptab <-
  out_support %>% 
  unnest(c(coef1, se1, coef2, se2, difference, ftest)) %>% 
  select(ivs, coef1, se1, coef2, se2, difference, F, `Pr(>F)`) %>% 
  mutate(`Current Attacks` = paste0(round(coef1, 3), " (", round(coef1 / se1, 3),")"),
         `Future Attacks` = paste0(round(coef2, 3), " (", round(coef2 / se2, 3),")"),
         Difference = as.character(round(difference, 3)),
         `F-test [Pr(>F)]` = paste0(round(`F`, 3), " [", round(`Pr(>F)`, 3),"]")) %>% 
  mutate(temporal_window = str_split(ivs, pattern = "_", simplify = T)[,2],
         spatial_window = str_split(ivs, pattern = "_", simplify = T)[,4]) %>% 
  select(temporal_window, spatial_window, `Current Attacks`, `Future Attacks`, 
         `Current - Future` = Difference, 
         `F-test: Current - Future = 0`  = `F-test [Pr(>F)]`) %>% 
  pivot_longer(cols = c(`Current Attacks`, `Future Attacks`, `Current - Future`, 
                        `F-test: Current - Future = 0`)) %>% 
  pivot_wider(names_from = spatial_window) %>% 
  select(-temporal_window) 





suptab %>% 
  bind_rows(tibble(name = c(paste("N =", format(out_support$n[[1]], big.mark = ",")),
                            paste("ADM1 =", format(out_support$ADM1[[1]], big.mark = ",")),
                            paste("Rounds =", format(out_support$Rounds[[1]], big.mark = ",")),
                            paste("Clusters =", format(out_support$Clusters[[1]], big.mark = ","))))) %>% 
  mutate(across(.cols = everything(),~ case_when(is.na(.) ~ "", TRUE ~.))) %>%
  
  kbl(booktabs = T, 
      "latex",
      caption = "", linesep = "", escape = F,
      align = c("l","c","c","c","c")) %>% 
  kable_styling() %>% 
  add_header_above(c("", "Spatial Window" = 4)) %>% 
  group_rows("3 months", start_row = 1, end_row = 4) %>%
  group_rows("6 months", start_row = 5, end_row = 8) %>%
  group_rows("12 months", start_row = 9, end_row = 12) %>% 
  group_rows("", start_row = 13, end_row = 16) %>% 
  
  add_footnote("Note: T-statistics in parentheses from robust standard errors clustered at survey cluster level in parentheses. F-tests for whether the difference between the coefficients for Current and Future are statistically different from zero are shown with p-values in brackets. All models include ADM1-level and survey round fixed effects, as well as the full set of control variables as discussed above.",
               notation = 'none',
               threeparttable = T)  %>%
  writeLines(., here::here("Tables",
                           "diff_support.tex"))





out_happy <-
  tibble(ivs = ivsihs,
         future = futuresivs) %>% 
  mutate(out = map2(.x = ivs, 
                    .y = future,
                    ~former(dv = "SD_demhappy",
                            iv = paste(.x, "+", .y),
                            covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                            spec = "| round + id1 | 0 | geoid") %>% 
                      felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)),
         coef1 = map(outtab, ~.x[["estimate"]][[1]]),
         se1 = map(outtab, ~.x[["std.error"]][[1]]),
         coef2 = map(outtab, ~.x[["estimate"]][[2]]),
         se2 = map(outtab, ~.x[["std.error"]][[2]]),
         difference = map(outtab, ~.x[["estimate"]][[1]] - .x[["estimate"]][[2]]),
         ftest = map(.x = out,
                     ~car::linearHypothesis(.x, 
                                            paste(attr(.x[["terms"]], "term.labels")[[1]],
                                                  "=",
                                                  attr(.x[["terms"]], "term.labels")[[2]]),
                                            vcov = .x[["vcv"]],
                                            white.adjust = "hc1",
                                            test = "F")[2,]),
         n = map_dbl(.x = out, ~.x[["N"]]),
         ADM1 = map_dbl(.x = out, ~.x[["fe"]][["id1"]] %>% unique() %>% length()),
         Rounds = map_dbl(.x = out, ~.x[["fe"]][["round"]] %>% unique() %>% length()),
         Clusters = map_dbl(.x = out, ~.x[["clustervar"]][[1]] %>% unique() %>% length()),
         R2 = map_dbl(.x = out, ~summary(.x)[["r2"]]))




happytab <-
  out_happy %>% 
  unnest(c(coef1, se1, coef2, se2, difference, ftest)) %>% 
  select(ivs, coef1, se1, coef2, se2, difference, F, `Pr(>F)`) %>% 
  mutate(`Current Attacks` = paste0(round(coef1, 3), " (", round(coef1 / se1, 3),")"),
         `Future Attacks` = paste0(round(coef2, 3), " (", round(coef2 / se2, 3),")"),
         Difference = as.character(round(difference, 3)),
         `F-test [Pr(>F)]` = paste0(round(`F`, 3), " [", round(`Pr(>F)`, 3),"]")) %>% 
  mutate(temporal_window = str_split(ivs, pattern = "_", simplify = T)[,2],
         spatial_window = str_split(ivs, pattern = "_", simplify = T)[,4]) %>% 
  select(temporal_window, spatial_window, `Current Attacks`, `Future Attacks`, 
         `Current - Future` = Difference, 
         `F-test: Current - Future = 0`  = `F-test [Pr(>F)]`) %>% 
  pivot_longer(cols = c(`Current Attacks`, `Future Attacks`, `Current - Future`, 
                        `F-test: Current - Future = 0`)) %>% 
  pivot_wider(names_from = spatial_window) %>% 
  select(-temporal_window) 


happytab %>% 
  bind_rows(tibble(name = c(paste("N =", format(out_happy$n[[1]], big.mark = ",")),
                            paste("ADM1 =", format(out_happy$ADM1[[1]], big.mark = ",")),
                            paste("Rounds =", format(out_happy$Rounds[[1]], big.mark = ",")),
                            paste("Clusters =", format(out_happy$Clusters[[1]], big.mark = ","))))) %>% 
  mutate(across(.cols = everything(),~ case_when(is.na(.) ~ "", TRUE ~.))) %>%
  
  kbl(booktabs = T, 
      "latex",
      caption = "", linesep = "", escape = F,
      align = c("l","c","c","c","c")) %>% 
  kable_styling() %>% 
  add_header_above(c("", "Spatial Window" = 4)) %>% 
  group_rows("3 months", start_row = 1, end_row = 4) %>%
  group_rows("6 months", start_row = 5, end_row = 8) %>%
  group_rows("12 months", start_row = 9, end_row = 12) %>% 
  group_rows("", start_row = 13, end_row = 16) %>% 
  
  add_footnote("Note: T-statistics in parentheses from robust standard errors clustered at survey cluster level in parentheses. F-tests for whether the difference between the coefficients for Current and Future are statistically different from zero are shown with p-values in brackets. All models include ADM1-level and survey round fixed effects, as well as the full set of control variables as discussed above.",
               notation = 'none',
               threeparttable = T)  %>%
  writeLines(., here::here("Tables",
                           "diff_happy.tex"))



