### 2022-02-15 Ty Stanford and Dot Dumuid
# code to produce bootstrapped 95% CIs for the continuous
# BMI outcome (Gaussian outcome, identity link 
# function, random intercept + slope GLMMs) on the child 
# longitudinal cohort

# ---- libs ----

# GLMMs
library("glmmTMB")
library("optimx")

# data manipulation and plotting
library("dplyr")
library("forcats")
library("ggplot2")

# functional programming for bootstrapping
library("purrr")
library("furrr")
library("tictoc")
library("foreach")

# efficient results storage and reading (.parquet)
library("arrow")

# tables of model parameters, summaries
library("report")

source("r/_global_funcs.R")

# ---- check_parallel_comp ----

# furrr parallel workers/cores setup
# change `workers = 4` based on cores available in processor being used
plan(multisession, workers = 4) 

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


# ---- read_dat ----

# sep dat
dat_chi_bmi_sep <- arrow::read_parquet("dat/chi_bmi_sep_dat.parquet")
dat_chi_bmi_sep




# ---- model_chi_bmi ----


# research question - does change in bmi across 
# childhood differ by SES?
mod_chi_bmi_sep <- 
  glmmTMB(
    bmi ~ sex + (age_cat + sep + prs)^2 + (1 + waveC | hicid),
    family = gaussian(), 
    data = dat_chi_bmi_sep,
    REML = TRUE
    # verbose = TRUE
  )
summary(mod_chi_bmi_sep)
car::Anova(mod_chi_bmi_sep, type = "III")



# output summary to text file
sink("res/mod_chi_bmi_sep.txt")

  print_mod_results(mod_chi_bmi_sep)
  
sink()



# ---- mod_preds ----

df_preds <-
  dat_chi_bmi_sep %>%
  select(-bmi) %>%
  mutate(
    pred = 
      predict(
        mod_chi_bmi_sep, 
        newdata = ., 
        type = "response", 
        allow.new.levels = FALSE, 
        re.form = NULL
      )
  ) %>%
  group_by(sep, prs, age_cat, sex) %>%
  summarise(pred_p = mean(pred), .groups = "drop") %>%
  arrange(sep, prs, age_cat, sex)

arrow::write_parquet(df_preds, sink = "res/mod_chi_bmi_sep_preds.parquet")




# ---- check_preds ----

newdat_base <- dat_chi_bmi_sep 
newdat_base <- newdat_base %>% mutate(sex = 0.5)
newdat_basep <- predict(mod_chi_bmi_sep, newdata = newdat_base, type = "response", allow.new.levels = FALSE, re.form=NULL)
# newdat_basep <- predict(mod_chi_bmi_sep, type = "response", allow.new.levels = TRUE)
newdat_base <- cbind(newdat_base, newdat_basep)
newdat_base <-
  newdat_base %>%
  group_by(sep, prs, age_cat) %>%
  summarise(pred_p = mean(newdat_basep), .groups = "drop") %>%
  arrange(sep, prs, age_cat)
preds <- 
  newdat_base %>% 
  dplyr::select(age_cat, sep, prs, pred_p) %>%
  mutate(type = "predicted")

# compare with observed
check <- 
  dat_chi_bmi_sep %>%
  group_by(age_cat, sep, prs) %>%
  summarise(pred_p = mean(bmi), n = n(), .groups = "drop") 
check <- 
  check %>% 
  dplyr::select(age_cat, sep, prs, pred_p) %>%
  mutate(type = "observed")

rbind.data.frame(preds, check) %>%
  ggplot(data = ., aes(x = age_cat, y = pred_p, group = type)) +
  geom_line(aes(color = type)) +
  facet_grid(prs ~ sep, labeller = label_both) +
  theme_bw()

# preds %>%
#   arrange(type, age_cat, sep)

rbind.data.frame(preds, check) %>%
  group_by(type, age_cat, sep) %>%
  summarise(pred_p = mean(pred_p), .groups = "drop") %>%
  ggplot(
    data = ., 
    aes(x = age_cat, y = pred_p, group = interaction(type, sep))
  ) +
  geom_line(aes(color = sep, linetype = type)) +
  theme_bw()

# wide format
inner_join(
  newdat_base,
  check %>% rename(obs_p = pred_p) %>% select(-type),
  c("age_cat", "sep", "prs")
)


# ---- boot_setup ----

n_map <-
  dat_chi_bmi_sep %>%
  arrange(hicid) %>%
  group_by(hicid) %>%
  summarise(n = n()) %>%
  ungroup()

n_obs <- sum(pull(n_map, n))
n_id <- nrow(n_map)
# could make bootstrap samples larger and trim but 
# decision to allow total n in bootstraps to vary slightly but number of IDs constant
# e.g., n_id <- nrow(n_map) + 100 to sample more ids to make sure the total n is reached
n_id_samp <- n_id 

rand_id_rows <- sample(x = n_id, size = n_id_samp, replace = TRUE)

n_map_samp <- 
  n_map[rand_id_rows, ] %>%
  mutate(cum_n = cumsum(n)) 

n_map_samp$cum_n[nrow(n_map_samp)] 

# n_map_samp <- 
#   n_map_samp %>%
#   dplyr::filter(cum_n <= n_obs)
# 
# n_map_samp$cum_n[nrow(n_map_samp)] 

# check returns bootstrap sample
inner_join(
  n_map_samp %>% select(hicid),
  dat_chi_bmi_sep,
  "hicid"
)

# ---- boot_fns ----

sample_dat_with_replace <- function(x = NULL) {
  
  rand_id_rows <- sample(x = n_id, size = n_id_samp, replace = TRUE)
  
  n_map_samp <- n_map[rand_id_rows, ] # %>%
    # mutate(cum_n = cumsum(n)) %>%
    # dplyr::filter(cum_n <= n_obs)
  
  # return boot sample
  inner_join(
    n_map_samp %>% select(hicid),
    dat_chi_bmi_sep,
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
  # pred_dat
  pred_dat %>%
    group_by(sep, prs, age_cat, sex) %>%
    summarise(pred_p = mean(pred_val), .groups = "drop") %>%
    arrange(sep, prs, age_cat, sex)
}



newdat_base_pred <- 
  predict(
    mod_chi_bmi_sep, 
    newdata = dat_chi_bmi_sep, 
    type = "response", 
    allow.new.levels = FALSE, 
    re.form=NULL
  )
newdat_base <- 
  cbind(dat_chi_bmi_sep, pred_bmi = newdat_base_pred) %>%
  group_by(sep, prs, age_cat, sex) %>%
  summarise(pred_p = mean(pred_bmi), .groups = "drop") %>%
  arrange(sep, prs, age_cat, sex)

# test
get_pred_by_cats(mod_chi_bmi_sep, dat_chi_bmi_sep)
get_pred_by_cats(mod_chi_bmi_sep, dat_chi_bmi_sep) %>%
  full_join(
    .,
    newdat_base,
    c("sep", "prs", "age_cat", "sex")
  ) %>%
  mutate(diff = pred_p.x - pred_p.y) %>%
  dplyr::filter(is.na(diff) | (abs(diff) > 1e-9)) %>%
  nrow(.)


# ---- mod_sed_boot ----

fit_glmm_tmb <- function(x) {
  mod_fit <-
    try({
      glmmTMB(
        bmi ~ sex + (age_cat + sep + prs)^2 + (1 + waveC | hicid),
        family = gaussian(), 
        data = x,
        REML = TRUE
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
  abs(
    get_pred_by_cats(fit_glmm_tmb(dat_chi_bmi_sep), dat_chi_bmi_sep)$pred_p - 
    newdat_base$pred_p
  ) < 1e-9
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

# fit model over random sample data with replacement for n~= 10,000
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

arrow::write_parquet(boot_df_preds, sink = "res/mod_chi_bmi_sep_boot_preds.parquet")

