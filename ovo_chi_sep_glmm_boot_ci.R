### 2022-12-22 Ty Stanford and Dot Dumuid
# code to produce bootstrapped 95% CIs for the binary
# obesity outcome (Bernoulli outcome, log-odds link 
# function, random intercept GLMMs) on the child 
# longitudinal cohort

# ---- libs ----

# GLMMs
library("glmmTMB")
library("optimx")

# data manipulation and plotting
library("dplyr")
library("tidyr")
library("forcats")
library("ggplot2")

# functional programming for bootstrapping
library("purrr")
library("furrr")
library("tictoc")
library("foreach")

# efficient results storage and reading (.parquet)
library("arrow")

# ---- check_parallel_comp ----

# furrr parallel workers/cores setup
# change `workers = 5` based on cores available in processor being used
plan(multisession, workers = 5) 

### test parallel works
# test code from https://furrr.futureverse.org/
# sequential
tic()
dev_null <- map(c(2, 2, 2), ~Sys.sleep(.x))
toc() # ~6 sec
# parallel: should be a third of the time of sequential
tic()
dev_null <- future_map(c(2, 2, 2), ~Sys.sleep(.x))
toc() # ~2 sec

# ---- consts ----

alpha <- 0.05

sep5tile_lvls <- as.character(1:5)
age_cat_lvls <- c("2-3", "4-5", "6-7", "8-9", "10-11", "12-13", "14+")

# ---- read_dat ----

long <- read.csv("dat/long_child_prs_data.csv")




long$age_cat <- factor(long$age_cat, levels = age_cat_lvls)
long$age_cat <- relevel(factor(long$age_cat), ref = "2-3")
long$ovob <- factor(long$ovob)

# cut continuous variables into quintiles
long <- 
  long %>%
  group_by(wave) %>%
  mutate(
    sep5tile = ntile(sep, 5),
    seifa5tile = ntile(seifa, 5),
    prs5tile = ntile(prs, 5)
  ) %>%
  ungroup()

long$sep5tile   <- factor(long$sep5tile,   levels = sep5tile_lvls)
long$seifa5tile <- factor(long$seifa5tile, levels = sep5tile_lvls)
long$prs5tile   <- factor(long$prs5tile,   levels = sep5tile_lvls)

dat_lite <- 
  long %>% 
  dplyr::select(hicid, ovob, bmi, age_cat, sep5tile, seifa5tile, prs5tile)


# ---- model_ovob_child_zSEP ----

dat_mod1_ovo_chi_sep <-
  dat_lite %>%
  select(hicid, ovob, age_cat, sep5tile) %>%
  arrange(hicid, age_cat, sep5tile)

dat_mod1_ovo_chi_sep <- na.omit(dat_mod1_ovo_chi_sep)
nrow(dat_mod1_ovo_chi_sep)

# odds ratios
# research question - does change in risk of overweight/obesity (OVOB) across 
# childhood differ by SES?
mod1 <- 
  glmmTMB(
    # ovob ~ age_cat * sep5tile + (1 | hicid),
    ovob ~ age_cat * sep5tile + (1 | hicid),
    family = binomial(), 
    data = dat_mod1_ovo_chi_sep,
    # control = glmmTMBControl(parallel = 4),
    # REML = TRUE,
    # verbose = TRUE
  )
summary(mod1)


dat_mod2_ovo_chi_sep <-
  dat_lite %>%
  select(hicid, ovob, age_cat, sep5tile, prs5tile) %>%
  arrange(hicid, age_cat, sep5tile, prs5tile)

dat_mod2_ovo_chi_sep <- na.omit(dat_mod2_ovo_chi_sep)
nrow(dat_mod2_ovo_chi_sep)

# odds ratios
# research question - does change in risk of overweight/obesity (OVOB) across 
# childhood differ by SES?
mod2 <- 
  glmmTMB(
    ovob ~ (age_cat + sep5tile + prs5tile)^2 + (1 | hicid),
    family = binomial(), 
    data = dat_mod2_ovo_chi_sep,
    # control = glmmTMBControl(parallel = 4),
    # REML = TRUE,
    # verbose = TRUE
  )
summary(mod2)



# ---- check_preds ----

newdat_base <- dat_mod2_ovo_chi_sep 
newdat_basep <- predict(mod2, type = "response", allow.new.levels = TRUE) # re.form=NULL
newdat_base <- cbind(newdat_base, newdat_basep)
newdat_base <-
  newdat_base %>%
  group_by(sep5tile, prs5tile, age_cat) %>%
  summarise(pred_p = mean(newdat_basep), .groups = "drop") %>%
  arrange(sep5tile, prs5tile, age_cat)
preds <- 
  newdat_base %>% 
  dplyr::select(age_cat, sep5tile, prs5tile, pred_p) %>%
  mutate(type = "predicted")

# compare with observed
check <- 
  dat_mod2_ovo_chi_sep %>%
  group_by(age_cat, sep5tile, prs5tile) %>%
  summarise(freq = sum(ovob == "1"), n = n(), .groups = "drop") %>%
  mutate(pred_p = freq / n)
check <- 
  check %>% 
  dplyr::select(age_cat, sep5tile, prs5tile, pred_p) %>%
  mutate(type = "observed")

rbind.data.frame(preds, check) %>%
  ggplot(data = ., aes(x = age_cat, y = pred_p, group = type)) +
  geom_line(aes(color = type)) +
  facet_grid(prs5tile ~ sep5tile) +
  theme_bw()

other_p_dat <-
  inner_join(
    newdat_base,
    check %>% rename(obs_p = pred_p) %>% select(-type),
    c("age_cat", "sep5tile", "prs5tile")
  )


# ---- boot_setup ----

n_map <-
  dat_mod2_ovo_chi_sep %>%
  arrange(hicid) %>%
  group_by(hicid) %>%
  summarise(n = n()) %>%
  ungroup()

n_obs <- sum(pull(n_map, n))
n_id <- nrow(n_map)
n_id_samp <- n_id + 100 # sample more ids to make sure the total n is reached

rand_id_rows <- sample(x = n_id, size = n_id_samp, replace = TRUE)

n_map_samp <- 
  n_map[rand_id_rows, ] %>%
  mutate(cum_n = cumsum(n)) 

n_map_samp$cum_n[nrow(n_map_samp)] 

n_map_samp <- 
  n_map_samp %>%
  dplyr::filter(cum_n <= n_obs)

n_map_samp$cum_n[nrow(n_map_samp)] 

inner_join(
  n_map_samp %>% select(hicid),
  dat_mod2_ovo_chi_sep,
  "hicid"
)

# ---- boot_fns ----

sample_dat_with_replace <- function(x = NULL) {
  
  rand_id_rows <- sample(x = n_id, size = n_id_samp, replace = TRUE)
  
  n_map_samp <- 
    n_map[rand_id_rows, ] %>%
    mutate(cum_n = cumsum(n)) %>%
    dplyr::filter(cum_n <= n_obs)
  
  inner_join(
    n_map_samp %>% select(hicid),
    dat_mod2_ovo_chi_sep,
    "hicid"
  )
  
}

# test
sample_dat_with_replace()
sample_dat_with_replace(23423)

# ---- bootstrap_fns ----

# preds per category
get_pred_by_cats <- function(mod, dat) {
  pred_val <- predict(mod, type = "response")
  pred_dat <- cbind.data.frame(pred_val, dat)
  pred_dat %>%
    group_by(sep5tile, prs5tile, age_cat) %>%
    summarise(pred_p = mean(pred_val), .groups = "drop") %>%
    arrange(sep5tile, prs5tile, age_cat)
}

# test
all(get_pred_by_cats(mod2, dat_mod2_ovo_chi_sep) == newdat_base)


# ---- mod_sed_boot ----

fit_glmm_tmb <- function(x) {
  mod_fit <-
    try({
      glmmTMB(
        # ovob ~ age_cat * sep5tile + (1 | hicid),
        ovob ~ (age_cat + sep5tile + prs5tile)^2 + (1 | hicid),
        family = binomial(), 
        data = x
        # control = glmmTMBControl(parallel = 4),
        # REML = TRUE,
        # verbose = TRUE
      )
    })
  
  if ("try-error" %in% class(mod_fit)) {
    return(NA)
  } else {
    return(mod_fit)
  }
  
}

# test
all(
  get_pred_by_cats(fit_glmm_tmb(dat_mod2_ovo_chi_sep), dat_mod2_ovo_chi_sep) == 
    newdat_base
)

# ---- do_bootstrap ----

R <- 1000
set.seed(1234)

# created table with data as elements
tic()
boot_df <-
  tibble(rep = 1:R) %>%
  mutate(data = map(.x = rep, .f = sample_dat_with_replace))
toc()

# fit model over random sample data with replacement for n~= 10630
# takes ~ 1400 sec =~ 25 min
tic()
boot_df <-
  boot_df %>%
  mutate(mod = future_map(.x = data, .f = fit_glmm_tmb))
toc()

# extract model predictions over all model fits
# takes ~ 5 min
tic()
boot_df <-
  boot_df %>%
  mutate(preds_df = future_map2(.x = mod, .y = data, .f = get_pred_by_cats))
toc()

### check elements
boot_df$data[[1]]
summary(boot_df$mod[[1]])
boot_df$preds_df[[1]]
boot_df$preds_df[[1]] %>%
  mutate(boot = as.integer(1))

# ---- save_results ----

boot_df_preds <-
  foreach(i = 1:nrow(boot_df), .combine = bind_rows) %do% {
    boot_df$preds_df[[i]] %>%
      mutate(boot = as.integer(i))
  }

arrow::write_parquet(boot_df_preds, sink = "res/mod2_ovo_chi_sep_boot_df_preds.parquet")


# ---- plot_results ----

boot_df_preds <- arrow::read_parquet("res/mod2_ovo_chi_sep_boot_df_preds.parquet")

boot_df_ci <-
  boot_df_preds %>%
  group_by(sep5tile, prs5tile, age_cat) %>%
  summarise(
    n_boot = n(), 
    mean_p = mean(pred_p  ), 
    med_p = median(pred_p  ), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sep5tile, prs5tile, age_cat)


boot_df_ci %>%
  rename(`PRS quintile` = prs5tile) %>%
  ggplot(., aes(x = age_cat, group = sep5tile, fill = sep5tile)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  facet_wrap(`PRS quintile` ~ ., labeller = "label_both", ncol = 5) +
  geom_line(
    aes(x = age_cat, y = mean_p, group = sep5tile, col = sep5tile),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = mean_p, col = sep5tile),
    alpha = 0.9
  ) +
  scale_fill_viridis_d(option = "C") +
  scale_colour_viridis_d(option = "C") +
  labs(
    col = "SEP\nquintile",
    fill = "SEP\nquintile",
    x = "Age category",
    y = "Probability"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 8)
  )



ggsave(
  filename = "fig/supp_fig_6.png",
  width = 12,
  height = 7,
  dpi = 300
)


cat(
  paste0(
    "Supp Fig 6: Overweight/obese probability across childhood by family ",
    "disadvantage (SEP) quintile (1=most, 5=least disadvantage), stratified ",
    "by PRS quintile (1=lowest, 5=highest risk)"
  ),
  "\n"
)

cat("Table: Raw and predicted probabilties on complete data\n")

other_p_dat %>%
  knitr::kable(., digits = 3)

cat("Table: Bootstrap results\n")

boot_df_ci %>%
  knitr::kable(., digits = 3)
