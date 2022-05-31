library(tidyverse)
library(lfe)
library(sf)
library(cowplot)

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



# Model and plot prep ----------------------------------------------------

# IHS IVs
ivsihs <- c("ihs_attacks3_months_10km",
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



# Helper to make many formulas
former <- function(dv, iv, covariates = NULL, spec = NULL) {
  
  as.formula(paste0(dv, "~", iv, covariates, spec))
  
}

# Plot theme
theme_tb <- function () { 
  theme_minimal() %+replace%
    theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
          panel.grid = element_line(color = "gray95"),
          strip.text = element_text(margin = margin(0.2,0,0,0, "cm"), vjust = 1.5,
                                    color = "white", size = 10),
          panel.background = element_rect(color = "gray20"),
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0, size = 10), 
          plot.title.position = "plot", 
          plot.caption.position =  "plot")
}


# For coef plots
plotter <- function(mod, dv_lab, iv_lab, ivs, subset = F, id = "id1"){
  
  
  
  dat <- map2(.x = mod[["outtab"]], 
              .y = mod[["out"]],
              ~tibble(estimate = .x %>% pluck(2,1),
                      std.error = .x %>% pluck(3,1),
                      n = .y[["N"]],
                      ADM = .y[["fe"]][[id]] %>% unique() %>% length(),
                      Rounds = .y[["fe"]][["round"]] %>% unique() %>% length(),
                      Clusters = .y[["clustervar"]][[1]] %>% unique() %>% length())) %>% 
    bind_rows() %>% 
    bind_cols(iv = ivs) %>% 
    mutate(temporal_window = paste(str_split(str_split(ivs, pattern = "_attacks", simplify = T)[,2], 
                                             pattern = "_", simplify = T)[,1], "months"),
           spatial_window = str_split(iv, pattern = "_", simplify = T)[,4]) 
  
  
  p1 <- 
    ggplot(dat) +
    geom_hline(yintercept = 0, color = "darkred") +
    geom_linerange(aes(x = forcats::fct_inorder(spatial_window), y = estimate, ymin = estimate - 1.96 * std.error,
                       ymax = estimate + 1.96 * std.error),
                   position = position_dodge(width = 0.5),
                   size = 0.75) +
    
    geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), y = estimate, ymin = estimate - 1.68 * std.error,
                        ymax = estimate + 1.68 * std.error),
                    position = position_dodge(width = 0.5),
                    size = 1.5, fatten = 2, fill = "white", pch = 21) +
    
    facet_wrap(~forcats::fct_inorder(temporal_window), nrow = 1) +
    
    theme_tb() +
    scale_color_manual(values = c("gray10", "firebrick4")) +
    scale_shape_manual(values = c(21, 22)) +
    ylim(c(-0.1, 0.05)) +
    labs(x = "Spatial Window",
         y = "Coefficient",
         subtitle = paste0("DV: ", dv_lab = dv_lab, "\nIV: ", iv_lab = iv_lab))
  
  p2 <- ggplot(dat) +
    geom_label(aes(x = forcats::fct_inorder(spatial_window), 
                   y = 0,
                   label = paste0(" N = ", 
                                  format(n, big.mark = ","),
                                  paste0("\n ADM", substr(id, 3, 3)," = "),
                                  format(ADM, big.mark = ","),
                                  "\n Clust. = ",  format(Clusters, big.mark = ","))), 
               size = 2.25,
               label.r = unit(0, "lines")) +
    facet_wrap(~forcats::fct_inorder(temporal_window), nrow = 1) + 
    theme_void() +
    theme(strip.text = element_blank())
  
  ifelse(isTRUE(subset),
         outplot <- plot_grid(p1, NULL, p2, ncol = 1, align = "hv",
                              rel_heights = c(1,-0.15, 0.4)),
         outplot <- p1 + labs(caption = paste0(" N = ", 
                                               format(dat$n, big.mark = ","),
                                               paste0("\n ADM", substr(id, 3, 3)," = ")
                                               , 
                                               format(dat$ADM, big.mark = ","),
                                               "\n Rounds = ",  dat$Rounds,
                                               "\n Clusters = ",  format(dat$Clusters, big.mark = ","))))
  
  outplot
  
  
}







# Democratic Support ------------------------------------------------------

# Baseline
out_support <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "DA_supdem",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| round + id1 | 0 | geoid") %>% 
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))

plotter(mod = out_support, 
        dv_lab = "Democratic support (ordinal, 1-3)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs,
        subset = F) 

ggsave(here::here("figures", "update", "supdem_baseline.pdf"),
       dpi = 600, height = 5, width = 8)



# Subset to ever treated units
out_support_sub <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map2(.x = ivs, 
                    .y = evertreated,
                    ~former(dv = "DA_supdem",
                            iv = .x,
                            covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                            spec = "| round + id1 | 0 | geoid") %>% 
                      felm(., data = df_analysis[df_analysis[[.y]] == 1,])),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_support_sub, 
        dv_lab = "Democratic support (ordinal, 1-3)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs,
        subset = T) 

ggsave(here::here("figures", "update", "supdem_baseline_sub.pdf"),
       dpi = 600, height = 6, width = 10)



# Democratic satisfaction -------------------------------------------------

# Baseline
out_happy <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map2(.x = ivs, 
                    .y = evertreated,
                    ~former(dv = "SD_demhappy",
                            iv = .x,
                            covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                            spec = "| round + id1 | 0 | geoid") %>% 
                      felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_happy, 
        dv_lab = "Democratic satisfaction (ordinal, 0-4)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs,
        subset = F)

ggsave(here::here("figures", "update", "demhappy_baseline.pdf"),
       dpi = 600, height = 5, width = 8)


# Subset to ever treated units
out_happy_sub <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map2(.x = ivs, 
                    .y = evertreated,
                    ~former(dv = "SD_demhappy",
                            iv = .x,
                            covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                            spec = "| round + id1 | 0 | geoid") %>% 
                      felm(., data = df_analysis[df_analysis[[.y]] == 1,])),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_happy_sub, 
        dv_lab = "Democratic satisfaction (ordinal, 0-4)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs,
        subset = T)

ggsave(here::here("figures", "update", "demhappy_baseline_sub.pdf"),
       dpi = 600, height = 6, width = 10)



# Baseline heterogeneity by regime type -----------------------------------

# Democratic support
out_support0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "DA_supdem",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_support1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "DA_supdem",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 


dat_support_regime <-
  map2(.x = out_support1[["outtab"]],
       .y = out_support1[["out"]],
       ~tibble(estimate = .x %>% pluck(2,1),
               std.error = .x %>% pluck(3,1),
               n = .y[["N"]],
               ADM1 = .y[["fe"]][["id1"]] %>% unique() %>% length(),
               Rounds = .y[["fe"]][["round"]] %>% unique() %>% length(),
               Clusters = .y[["clustervar"]][[1]] %>% unique() %>% length())) %>% 
  bind_rows() %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, 
                                                     pattern = "_attacks", 
                                                     simplify = T)[,2], pattern = "_", 
                                           simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map2(.x = out_support0[["outtab"]],
                 .y = out_support0[["out"]],
                 ~tibble(estimate = .x %>% pluck(2,1),
                         std.error = .x %>% pluck(3,1),
                         n = .y[["N"]],
                         ADM1 = .y[["fe"]][["id1"]] %>% unique() %>% length(),
                         Rounds = .y[["fe"]][["round"]] %>% unique() %>% length(),
                         Clusters = .y[["clustervar"]][[1]] %>% unique() %>% length())) %>% 
              bind_rows() %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, 
                                                                 pattern = "_attacks", 
                                                                 simplify = T)[,2], 
                                                       pattern = "_", simplify = T)[,1], 
                                             "months"),
                     spatial_window = str_split(iv, pattern = "_", 
                                                simplify = T)[,4],
                     z = "Non-Democracy"))

p1_supregime <-
  ggplot(dat_support_regime) +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(spatial_window), 
                     y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error, color = z),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), 
                      y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error, color = z, pch = z),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white") +
  facet_wrap(~forcats::fct_inorder(temporal_window), nrow = 1) +
  theme_tb() +
  scale_color_manual(values = c("steelblue3", "firebrick4"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22), guide = guide_legend(title = NULL)) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = "DV: Democratic support (ordinal, 1-3)\nIV: IHS(Attacks)") +
  ylim(c(-0.12, 0.07)) +
  theme(legend.position = "top")



t1_supregime <-
  ggplot() +
  labs(subtitle = paste0("Democracy",
                         "\n N = ", 
                         format(dat_support_regime$n[dat_support_regime$z == "Democracy"], big.mark = ",")
                         , "\n ADM1 = ", 
                         dat_support_regime$ADM1[dat_support_regime$z == "Democracy"]
                         , "\n Rounds = ", 
                         dat_support_regime$Rounds[dat_support_regime$z == "Non-Democracy"],
                         "\n Clust. = ",  format(dat_support_regime$Clusters[dat_support_regime$z == "Democracy"], big.mark = ",")))

t0_supregime <-
  ggplot() +
  labs(subtitle = paste0("Non-Democracy",
                         "\n N = ", 
                         format(dat_support_regime$n[dat_support_regime$z == "Non-Democracy"], big.mark = ",")
                         , "\n ADM1 = ", 
                         dat_support_regime$ADM1[dat_support_regime$z == "Non-Democracy"]
                         , "\n Rounds = ", 
                         dat_support_regime$Rounds[dat_support_regime$z == "Non-Democracy"],
                         "\n Clust. = ",  format(dat_support_regime$Clusters[dat_support_regime$z == "Non-Democracy"], big.mark = ",")))


tab_support_regime <-
  dat_support_regime %>% 
  filter(temporal_window == "3 months",
         spatial_window == "10km") %>% 
  
  select(z, n, ADM1, Clusters, Rounds) %>% 
  ggplot() +
  geom_label(aes(x= z,
                 y = 0,
                 label = paste0(z,
                                "\n N = ", 
                                format(n, big.mark = ",")
                                , "\n ADM1 = ", 
                                ADM1
                                , "\n Rounds = ", 
                                Rounds,
                                "\n Clust. = ",  format(Clusters, big.mark = ","))),
             size = 2.5,
             label.r = unit(0, "lines")) +
  theme_void() 

plot_grid(p1_supregime, NULL, 
          plot_grid(tab_support_regime, NULL, NULL, NULL, 
                    ncol = 4),
          ncol = 1,
          rel_heights = c(1,0, 0.2))



ggsave(here::here("figures", "update", "demsupport_baseline_het.pdf"),
       dpi = 600, height = 5, width = 8)




# Democratic satisfaction
out_happy0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "SD_demhappy",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_happy1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "SD_demhappy",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 


dat_happy_regime <-
  map2(.x = out_happy1[["outtab"]],
       .y = out_happy1[["out"]],
       ~tibble(estimate = .x %>% pluck(2,1),
               std.error = .x %>% pluck(3,1),
               n = .y[["N"]],
               ADM1 = .y[["fe"]][["id1"]] %>% unique() %>% length(),
               Rounds = .y[["fe"]][["round"]] %>% unique() %>% length(),
               Clusters = .y[["clustervar"]][[1]] %>% unique() %>% length())) %>% 
  bind_rows() %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, 
                                                     pattern = "_attacks", 
                                                     simplify = T)[,2], pattern = "_", 
                                           simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map2(.x = out_happy0[["outtab"]],
                 .y = out_happy0[["out"]],
                 ~tibble(estimate = .x %>% pluck(2,1),
                         std.error = .x %>% pluck(3,1),
                         n = .y[["N"]],
                         ADM1 = .y[["fe"]][["id1"]] %>% unique() %>% length(),
                         Rounds = .y[["fe"]][["round"]] %>% unique() %>% length(),
                         Clusters = .y[["clustervar"]][[1]] %>% unique() %>% length())) %>% 
              bind_rows() %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, 
                                                                 pattern = "_attacks", 
                                                                 simplify = T)[,2], 
                                                       pattern = "_", simplify = T)[,1], 
                                             "months"),
                     spatial_window = str_split(iv, pattern = "_", 
                                                simplify = T)[,4],
                     z = "Non-Democracy"))

p1_happyregime <-
  ggplot(dat_happy_regime) +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(spatial_window), 
                     y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error, color = z),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), 
                      y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error, color = z, pch = z),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white") +
  facet_wrap(~forcats::fct_inorder(temporal_window), nrow = 1) +
  theme_tb() +
  scale_color_manual(values = c("steelblue3", "firebrick4"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22), guide = guide_legend(title = NULL)) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = "DV: Democratic satisfaction (ordinal, 0-4)\nIV: IHS(Attacks)") +
  ylim(c(-0.12, 0.07)) +
  theme(legend.position = "top")



t1_happyregime <-
  ggplot() +
  labs(subtitle = paste0("Democracy",
                         "\n N = ", 
                         format(dat_happy_regime$n[dat_happy_regime$z == "Democracy"], big.mark = ",")
                         , "\n ADM1 = ", 
                         dat_happy_regime$ADM1[dat_happy_regime$z == "Democracy"]
                         , "\n Rounds = ", 
                         dat_happy_regime$Rounds[dat_happy_regime$z == "Non-Democracy"],
                         "\n Clust. = ",  format(dat_happy_regime$Clusters[dat_happy_regime$z == "Democracy"], big.mark = ",")))

t0_happyregime <-
  ggplot() +
  labs(subtitle = paste0("Non-Democracy",
                         "\n N = ", 
                         format(dat_happy_regime$n[dat_happy_regime$z == "Non-Democracy"], big.mark = ",")
                         , "\n ADM1 = ", 
                         dat_happy_regime$ADM1[dat_happy_regime$z == "Non-Democracy"]
                         , "\n Rounds = ", 
                         dat_happy_regime$Rounds[dat_happy_regime$z == "Non-Democracy"],
                         "\n Clust. = ",  format(dat_happy_regime$Clusters[dat_happy_regime$z == "Non-Democracy"], big.mark = ",")))


tab_happy_regime <-
  dat_happy_regime %>% 
  filter(temporal_window == "3 months",
         spatial_window == "10km") %>% 
  
  select(z, n, ADM1, Clusters, Rounds) %>% 
  ggplot() +
  geom_label(aes(x= z,
                 y = 0,
                 label = paste0(z,
                                "\n N = ", 
                                format(n, big.mark = ",")
                                , "\n ADM1 = ", 
                                ADM1
                                , "\n Rounds = ", 
                                Rounds,
                                "\n Clust. = ",  format(Clusters, big.mark = ","))),
             size = 2.5,
             label.r = unit(0, "lines")) +
  theme_void() 

plot_grid(p1_happyregime, NULL, 
          plot_grid(tab_happy_regime, NULL, NULL, NULL, 
                    ncol = 4),
          ncol = 1,
          rel_heights = c(1,0, 0.2))



ggsave(here::here("figures", "update", "demhappy_baseline_het.pdf"),
       dpi = 600, height = 5, width = 8)



# Time trend --------------------------------------------------------------

# Democratic support
out_support_trend <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "DA_supdem",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| time:id1 + round + id1 | 0 | geoid") %>% 
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))

plotter(mod = out_support_trend, 
        dv_lab = "Democratic support (ordinal, 1-3)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs, 
        subset = F) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", 
                         dv_lab =  "Democratic support (ordinal, 1-3)", "\nIV: ", 
                         iv_lab = "IHS(Attacks)"))

ggsave(here::here("figures", "update", "supdem_trend.pdf"),
       dpi = 600, height = 5, width = 8)


# Democratic satisfaction
out_happy_trend <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "SD_demhappy",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| time:id1 + round + id1 | 0 | geoid") %>% 
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_happy_trend, 
        dv_lab = "Democratic satisfaction (ordinal, 0-4)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs,
        subset = F) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", 
                         dv_lab =  "Democratic support (ordinal, 1-3)", "\nIV: ", 
                         iv_lab = "IHS(Attacks)"))

ggsave(here::here("figures", "update", "demhappy_trend.pdf"),
       dpi = 600, height = 5, width = 8)




# ADM2 FEs  ---------------------------------------------------------------

# Democratic support
out_support_adm2 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "DA_supdem",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| round + id2 | 0 | geoid") %>% 
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_support_adm2, 
        dv_lab = "Democratic support (ordinal, 1-3)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs,
        subset = F,
        id = "id2") +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", dv_lab =  "Democratic support (ordinal, 1-3)", 
                         "\nIV: ", 
                         iv_lab = "IHS(Attacks)"))

ggsave(here::here("figures", "update", "supdem_adm2fe.pdf"),
       dpi = 600, height = 5, width = 8)



# Democratic satisfaction
out_happy_adm2 <-
  tibble(ivs = ivsihs) %>%
  mutate(out = map(.x = ivs, ~former(dv = "SD_demhappy",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) +
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood +
                                      factor(supragroup) + Lights + Population",
                                     spec = "| round + id2 | 0 | geoid") %>%
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_happy_adm2,
        dv_lab = "Democratic satisfaction (ordinal, 0-4)", 
        iv_lab = "IHS(Attacks)",
        ivs = ivsihs,
        subset = F,
        id = "id2") +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", dv_lab =  "Democratic satisfaction (ordinal, 0-4)", 
                         "\nIV: ", 
                         iv_lab = "IHS(Attacks)"))

ggsave(here::here("figures", "update", "demhappy_adm2.pdf"),
       dpi = 600, height = 5, width = 8)



# ADM2 Clusters  -------------------------------------------------------------

# Democratic support
out_support_adm2cl <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "DA_supdem",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| round + id1 | 0 | id2") %>% 
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_support_adm2cl, 
        dv_lab = "Democratic support (ordinal, 1-3)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", 
                         dv_lab =  "Democratic support (ordinal, 1-3)", "\nIV: ", 
                         iv_lab = "IHS(Attacks)"))

ggsave(here::here("figures", "update", "supdem_adm2cl.pdf"),
       dpi = 600, height = 5, width = 8)



# Democratic satisfaction
out_happy_adm2cl <-
  tibble(ivs = ivsihs) %>%
  mutate(out = map(.x = ivs, ~former(dv = "SD_demhappy",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) +
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood +
                                      factor(supragroup) + Lights + Population",
                                     spec = "| round + id1 | 0 | id2") %>%
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_happy_adm2cl,
        dv_lab = "Democratic satisfaction (ordinal, 0-4)", 
        iv_lab = "IHS(Attacks)",
        ivs = ivsihs) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", dv_lab =  "Democratic satisfaction (ordinal, 0-4)", 
                         "\nIV: ", 
                         iv_lab = "IHS(Attacks)"),
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: ADM2")

ggsave(here::here("figures", "update", "demhappy_adm2cl.pdf"),
       dpi = 600, height = 5, width = 8)



# Democratic extent -------------------------------------------------------

out_extent <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "DA_demext",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| round + id1 | 0 | geoid") %>% 
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_extent, 
        dv_lab = "Democratic support (ordinal, 1-3)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", 
                         dv_lab =  "Democratic extent (ordinal, 1-4)", 
                         "\nIV: ", iv_lab = "IHS(Attacks)"))

ggsave(here::here("figures", "update", "demextent_baseline.pdf"),
       dpi = 600, height = 5, width = 8)


# Incumbent support -------------------------------------------------------

out_incumbent <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "support_incumbent",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| round + id1 | 0 | geoid") %>% 
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))


plotter(mod = out_incumbent, 
        dv_lab = "Incumbent support (dummy)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", dv_lab =  "Incumbent support (dummy)", 
                         "\nIV: ", iv_lab = "IHS(Attacks)")) 

ggsave(here::here("figures", "update", "incumbent_baseline.pdf"),
       dpi = 600, height = 5, width = 8)




# Aggregated attacks to ADM1 level ----------------------------------------

load(here::here("processed", "gtd_agg_full.Rdata"))


df_analysis2 <-
  df_analysis %>% 
  left_join(., gtd_agg_full, 
            by = c("NAME_0", "NAME_1", "date" = "gtd_date"))


# attacks_agg
df_analysis2 <-
  df_analysis2 %>% 
  mutate(across(starts_with("attacks_agg"),
                ~ihs(.),
                .names = "ihs_{.col}"))

ivsagg <- c("ihs_attacks_agg3",
            "ihs_attacks_agg6",
            "ihs_attacks_agg12")

out_agg_support <-
  tibble(ivs = ivsagg) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "DA_supdem",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| round + id1 | 0 | geoid") %>% 
                     felm(., data = df_analysis2)),
         outtab = map(out, ~broom::tidy(.x)))





map_df(out_agg_support[["outtab"]], 
       ~tibble(estimate = .x %>% pluck(2,1),
               std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsagg) %>% 
  mutate(temporal_window = case_when(str_sub(iv, -2, -1) == "g3" ~ "3 months",
                                     str_sub(iv, -2, -1) == "g6" ~ "6 months",
                                     str_sub(iv, -2, -1) == "12" ~ "12 months",
  )) %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(temporal_window), y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(temporal_window), y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white", pch = 21) +
  theme_tb() +
  ylim(c(-0.12, 0.07)) +
  labs(x = "Temporal Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", dv_lab =  "Democratic support (ordinal, 1-3)", "\nIV: ", 
                         iv_lab = "IHS(Attacks aggregated to ADM1-level)"),
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: Survey clusters")

ggsave(here::here("figures", "update", "supdem_agg.pdf"),
       dpi = 600, height = 5, width = 8)



out_agg_happy <-
  tibble(ivs = ivsagg) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "SD_demhappy",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| round + id1 | 0 | geoid") %>% 
                     felm(., data = df_analysis2)),
         outtab = map(out, ~broom::tidy(.x)))



map_df(out_agg_happy[["outtab"]], 
       ~tibble(estimate = .x %>% pluck(2,1),
               std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsagg) %>% 
  mutate(temporal_window = case_when(str_sub(iv, -2, -1) == "g3" ~ "3 months",
                                     str_sub(iv, -2, -1) == "g6" ~ "6 months",
                                     str_sub(iv, -2, -1) == "12" ~ "12 months",
  )) %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(temporal_window), y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(temporal_window), y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white", pch = 21) +
  theme_tb() +
  ylim(c(-0.17, 0.07)) +
  labs(x = "Temporal Window",
       y = "Coefficient",
       subtitle = paste0("DV: ", dv_lab = "Democratic satisfaction (ordinal, 0-4)", "\nIV: ", 
                         iv_lab = "IHS(Attacks aggregated to ADM1-level)"),
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: Survey clusters")

ggsave(here::here("figures", "update", "demhappy_agg.pdf"),
       dpi = 600, height = 5, width = 8)


# Trust outcomes ----------------------------------------------------------

# President
out_pres0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustpres",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_pres1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustpres",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 



p1 <- map_df(out_pres1$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                       std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                           pattern = "_", simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map_df(out_pres0$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                             std.error = .x %>% pluck(3,1))) %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                                       pattern = "_", simplify = T)[,1], "months"),
                     spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
                     z = "Non-Democracy")) 

p1 %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(spatial_window), 
                     y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error, color = z),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), 
                      y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error, color = z, pch = z),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white") +
  facet_wrap(~forcats::fct_inorder(temporal_window), nrow = 1) +
  theme_tb() +
  scale_color_manual(values = c("steelblue3", "firebrick4"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22), guide = guide_legend(title = NULL)) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = "DV: Trust president (ordinal, 0-3)\nIV: IHS(Attacks)",
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: Survey clusters") +
  ylim(c(-0.17, 0.1))

ggsave(here::here("figures", "update", "trustpres_baseline_het.pdf"),
       dpi = 600, height = 5, width = 8)



# President
out_parl0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustparl",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_parl1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustparl",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 



p2 <- 
  map_df(out_parl1$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                   std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                           pattern = "_", simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map_df(out_parl0$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                             std.error = .x %>% pluck(3,1))) %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                                       pattern = "_", simplify = T)[,1], "months"),
                     spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
                     z = "Non-Democracy")) 
p2 %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(spatial_window), 
                     y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error, color = z),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), 
                      y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error, color = z, pch = z),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white") +
  facet_wrap(~forcats::fct_inorder(temporal_window), nrow = 1) +
  theme_tb() +
  scale_color_manual(values = c("steelblue3", "firebrick4"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22), guide = guide_legend(title = NULL)) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = "DV: Trust parliament (ordinal, 0-3)\nIV: IHS(Attacks)",
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: Survey clusters") +
  ylim(c(-0.17, 0.1))

ggsave(here::here("figures", "update", "trustparl_baseline_het.pdf"),
       dpi = 600, height = 5, width = 8)

# Ruling party
out_rlpt0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustrlparty",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_rlpt1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustrlparty",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 



p3 <- 
  map_df(out_rlpt1$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                   std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                           pattern = "_", simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map_df(out_rlpt0$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                             std.error = .x %>% pluck(3,1))) %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                                       pattern = "_", simplify = T)[,1], "months"),
                     spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
                     z = "Non-Democracy"))

p3 %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(spatial_window), 
                     y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error, color = z),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), 
                      y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error, color = z, pch = z),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white") +
  facet_wrap(~forcats::fct_inorder(temporal_window), nrow = 1) +
  theme_tb() +
  scale_color_manual(values = c("steelblue3", "firebrick4"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22), guide = guide_legend(title = NULL)) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = "DV: Trust ruling party (ordinal, 0-3)\nIV: IHS(Attacks)",
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: Survey clusters") +
  ylim(c(-0.17, 0.1))

ggsave(here::here("figures", "update", "trustrlprt_baseline_het.pdf"),
       dpi = 600, height = 5, width = 8)



bind_rows(bind_cols(p1, dv = "President"),
          bind_cols(p2, dv = "Parliament"),
          bind_cols(p3, dv = "Ruling Party")) %>% 
  
  ggplot(.) +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(spatial_window), 
                     y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error, color = z),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), 
                      y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error, color = z, pch = z),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white") +
  facet_grid(cols = vars(forcats::fct_inorder(temporal_window)),
             rows = vars(forcats::fct_inorder(dv))) +
  theme_tb() +
  scale_color_manual(values = c("steelblue3", "firebrick4"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22), guide = guide_legend(title = NULL)) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = "DV: Trust (ordinal, 0-3)\nIV: IHS(Attacks)",
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: Survey clusters") +
  ylim(c(-0.17, 0.1)) +
  theme(strip.text = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"), 
                                  vjust = 1,
                                  color = "white", size = 10))

ggsave(here::here("figures", "update", "trustmain_baseline_het.pdf"),
       dpi = 600, height = 10, width = 10)





# Tax officials
out_tax0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trusttax",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_tax1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trusttax",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 



p4 <- 
  map_df(out_tax1$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                  std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", 
                                                     simplify = T)[,2], 
                                           pattern = "_", 
                                           simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map_df(out_tax0$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                            std.error = .x %>% pluck(3,1))) %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", 
                                                                 simplify = T)[,2],
                                                       pattern = "_", 
                                                       simplify = T)[,1], "months"),
                     spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
                     z = "Non-Democracy")) %>% 
  mutate(dv = "Tax Officials")





# Opposition party
out_opp0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustopprty",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_opp1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustopprty",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 



p5 <- 
  map_df(out_opp1$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                  std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", 
                                                     simplify = T)[,2],
                                           pattern = "_", simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map_df(out_opp0$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                            std.error = .x %>% pluck(3,1))) %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", 
                                                                 simplify = T)[,2],
                                                       pattern = "_", simplify = T)[,1], "months"),
                     spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
                     z = "Non-Democracy")) %>% 
  mutate(dv = "Opposition Parties")



# Traditional leaders
out_trad0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trusttrad",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_trad1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trusttrad",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 



p6 <- 
  map_df(out_trad1$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                   std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                           pattern = "_", simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map_df(out_trad0$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                             std.error = .x %>% pluck(3,1))) %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", 
                                                                 simplify = T)[,2],
                                                       pattern = "_", simplify = T)[,1], "months"),
                     spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
                     z = "Non-Democracy")) %>% 
  mutate(dv = "Traditional Leaders")



# Local council
out_council0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustcouncil",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_council1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "PT_trustcouncil",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 



p7 <- 
  map_df(out_council1$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                      std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], pattern = "_", simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map_df(out_council0$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                                std.error = .x %>% pluck(3,1))) %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], pattern = "_", simplify = T)[,1], "months"),
                     spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
                     z = "Non-Democracy")) %>% 
  mutate(dv = "Local Council")


bind_rows(p4, p5, p6, p7) %>% 
  ggplot(.) +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(spatial_window), y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error, color = z),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error, color = z, pch = z),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white") +
  facet_grid(cols = vars(forcats::fct_inorder(temporal_window)),
             rows = vars(forcats::fct_inorder(dv))) +
  theme_tb() +
  scale_color_manual(values = c("steelblue3", "firebrick4"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22), guide = guide_legend(title = NULL)) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = "DV: Trust (ordinal, 0-3)\nIV: IHS(Attacks)",
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: Survey clusters") +
  ylim(c(-0.17, 0.1)) +
  theme(strip.text = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"), 
                                  vjust = 1,
                                  color = "white", size = 10))

ggsave(here::here("figures", "update", "trustappend_baseline_het.pdf"),
       dpi = 600, height = 12, width = 10)







# Free to move ------------------------------------------------------------
r7 <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                 "merged_r7_data.sav")) %>% 
  mutate(respno_r = paste0(RESPNO, "_r7"))

r7 <- 
  r7 %>% 
  mutate(ymd = lubridate::ymd(DATEINTR)) %>% 
  select(RESPNO, date = ymd, Q62)

r7 <- 
  r7 %>% 
  mutate(free_to_move = case_when(Q62 %in% c(1, 2) ~ 1,
                                  Q62 %in% c(3, 4, 5) ~ 0,
                                  TRUE ~ NA_real_))

table(r7$Q62, r7$free_to_move)


df_analysis <-
  df_analysis %>% 
  left_join(r7 %>% janitor::clean_names(), by = c("respno", "date")) 


df_analysis %>% 
  filter(!is.na(free_to_move)) %>% 
  janitor::tabyl(free_to_move) %>% 
  janitor::adorn_rounding(digits = 2)

df_analysis %>% 
  filter(!is.na(free_to_move) & !is.na(regime2)) %>% 
  janitor::tabyl(free_to_move, regime2) %>% 
  janitor::adorn_percentages("col") %>% 
  janitor::adorn_rounding(digits = 2) %>% 
  janitor::adorn_totals() %>% 
  janitor::adorn_ns()



# Baseline
out_freemove <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "free_to_move",
                                     iv = .x,
                                     covariates = "+ regime2 + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| round + id1 | 0 | geoid") %>% 
                     felm(., data = df_analysis)),
         outtab = map(out, ~broom::tidy(.x)))

plotter(mod = out_freemove, 
        dv_lab = "Free to Move (dummy)", 
        iv_lab = "IHS(Attacks)", 
        ivs = ivsihs,
        subset = F) 


out_freetomove0 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "free_to_move",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 0,])),
         outtab = map(out, ~broom::tidy(.x)))


out_freetomove1 <-
  tibble(ivs = ivsihs) %>% 
  mutate(out = map(.x = ivs, ~former(dv = "free_to_move",
                                     iv = .x,
                                     covariates = "  + factor(DM_fem) + 
                                      factor(DM_urban) + DM_age + DM_edu + WF_nofood + 
                                      factor(supragroup) + Lights + Population", 
                                     spec = "| id1 + round | 0 | geoid") %>% 
                     felm(., data = df_analysis[df_analysis$regime2 == 1,])),
         outtab = map(out, ~broom::tidy(.x))) 



p1free <- 
  map_df(out_freetomove1$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                         std.error = .x %>% pluck(3,1))) %>% 
  bind_cols(iv = ivsihs) %>% 
  mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                           pattern = "_", simplify = T)[,1], "months"),
         spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
         z = "Democracy") %>% 
  bind_rows(map_df(out_freetomove0$outtab, ~tibble(estimate = .x %>% pluck(2,1),
                                                   std.error = .x %>% pluck(3,1))) %>% 
              bind_cols(iv = ivsihs) %>% 
              mutate(temporal_window = paste(str_split(str_split(iv, pattern = "_attacks", simplify = T)[,2], 
                                                       pattern = "_", simplify = T)[,1], "months"),
                     spatial_window = str_split(iv, pattern = "_", simplify = T)[,4],
                     z = "Non-Democracy")) 

p1free %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "darkred") +
  geom_linerange(aes(x = forcats::fct_inorder(spatial_window), 
                     y = estimate, ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error, color = z),
                 position = position_dodge(width = 0.5),
                 size = 0.75) +
  
  geom_pointrange(aes(x = forcats::fct_inorder(spatial_window), 
                      y = estimate, ymin = estimate - 1.68 * std.error,
                      ymax = estimate + 1.68 * std.error, color = z, pch = z),
                  position = position_dodge(width = 0.5),
                  size = 1.5, fatten = 2, fill = "white") +
  facet_wrap(~forcats::fct_inorder(temporal_window), nrow = 1) +
  theme_tb() +
  scale_color_manual(values = c("steelblue3", "firebrick4"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(21, 22), guide = guide_legend(title = NULL)) +
  labs(x = "Spatial Window",
       y = "Coefficient",
       subtitle = "DV: Free to move (dummy, 0-1)\nIV: IHS(Attacks)",
       caption = "Fixed effects: ADM1 & Survey round\nClustered SEs: Survey clusters") +
  ylim(c(-0.17, 0.1))
