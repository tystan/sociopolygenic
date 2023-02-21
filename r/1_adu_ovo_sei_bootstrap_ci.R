### 2022-02-21 Ty Stanford and Dot Dumuid
# code to produce bootstrapped 95% CIs for the binary
# overweight/obese outcome (binary outcome, log-odds link 
# function, random intercept + slope GLMMs) on the adult 
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


# ---- check_parallel_comp ----

options(future.globals.maxSize = 1e+3 * 1024 ^ 2)

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

# sei dat
dat_adu_ovo_sei <- arrow::read_parquet("dat/adu_ovo_sei_dat.parquet")
dat_adu_ovo_sei


# ---- model_adu_ovo ----


# research question - does change in ovo across 
# adulthood differ by SES?
mod_adu_ovo_sei <- 
  glmmTMB(
    ovo ~ waveC + sex + (age_cat + sei + prs)^2 + 
      (1 + waveC | hicid) + (1 | personid),
    family = binomial(), 
    data = dat_adu_ovo_sei,
    REML = TRUE
    # verbose = TRUE
  )
summary(mod_adu_ovo_sei)
car::Anova(mod_adu_ovo_sei, type = "III")

# output summary to text file
sink("res/mod_adu_ovo_sei.txt")

  report_model(mod_adu_ovo_sei); cat(".\n\n")
  report_performance(mod_adu_ovo_sei); cat("\n")
  report_intercept(mod_adu_ovo_sei); cat("\n\n")
  # report_random(mod_adu_ovo_sei)
  
  mod_rep <- report(mod_adu_ovo_sei) # str(mod_rep)
  mod_rep_df <- summary(as.data.frame(mod_rep)) # str(mod_rep_df)
  mod_rep_df$df_error <- mod_rep_df$p <- mod_rep_df$df_error <- 
    mod_rep_df$Effects <- mod_rep_df$Group <- mod_rep_df$Component <- 
    mod_rep_df$Std_Coefficient <- NULL
  
  mod_rep_df

sink()



# ---- mod_preds ----

df_preds <-
  dat_adu_ovo_sei %>%
  select(-ovo) %>%
  mutate(
    # waveC = 3L, # latest wave
    pred = 
      predict(
        mod_adu_ovo_sei, 
        newdata = ., 
        type = "response", 
        allow.new.levels = FALSE, 
        re.form = NULL # NULL is conditional on random effects
      )
  ) %>%
  group_by(sei, prs, age_cat, sex) %>%
  summarise(pred_p = mean(pred), .groups = "drop") %>%
  arrange(sei, prs, age_cat, sex)

arrow::write_parquet(df_preds, sink = "res/mod_ovo_adu_sei_preds.parquet")


# age_wave <-
#   dat_adu_ovo_sei %>%
#   with(., table(waveC, age_cat, useNA = "ifany"))
# lvls_age_cat <- levels(dat_adu_ovo_sei$age_cat)
# wave_dist <- age_wave[, lvls_age_cat[4]]
# wave_dist <- wave_dist / sum(wave_dist)
# sum(as.numeric(names(wave_dist)) * wave_dist)
# 
# 
# dat_adu_ovo_sei %>%
#   with(., table(prs, sei, useNA = "ifany"))

# ---- check_preds ----

newdat_base <- dat_adu_ovo_sei 
# newdat_base <- newdat_base %>% mutate(sex = 0.5, waveC = 3L)
newdat_base <- newdat_base %>% mutate(sex = 0.5)
newdat_basep <- predict(mod_adu_ovo_sei, newdata = newdat_base, type = "response", allow.new.levels = FALSE, re.form = NULL)
# newdat_basep <- predict(mod_adu_ovo_sei, type = "response", allow.new.levels = TRUE)
newdat_base <- cbind(newdat_base, newdat_basep)
newdat_base <-
  newdat_base %>%
  group_by(sei, prs, age_cat) %>%
  summarise(pred_p = mean(newdat_basep), .groups = "drop") %>%
  arrange(sei, prs, age_cat)
preds <- 
  newdat_base %>% 
  dplyr::select(age_cat, sei, prs, pred_p) %>%
  mutate(type = "predicted")

# compare with observed
check <- 
  dat_adu_ovo_sei %>%
  group_by(age_cat, sei, prs) %>%
  summarise(pred_p = mean(ovo == "1"), n = n(), .groups = "drop") 
check <- 
  check %>% 
  dplyr::select(age_cat, sei, prs, pred_p) %>%
  mutate(type = "observed")

rbind.data.frame(preds, check) %>%
  ggplot(data = ., aes(x = age_cat, y = pred_p, group = interaction(type, sei))) +
  geom_point(aes(color = sei, shape = type)) +
  geom_line(aes(color = sei)) +
  facet_grid(prs ~ sei, labeller = label_both) +
  theme_bw()

# preds %>%
#   arrange(type, age_cat, sei)

rbind.data.frame(preds, check) %>%
  group_by(type, age_cat, sei) %>%
  summarise(pred_p = mean(pred_p), .groups = "drop") %>%
  ggplot(
    data = ., 
    aes(x = age_cat, y = pred_p, group = interaction(type, sei))
  ) +
  geom_line(aes(color = sei, linetype = type)) +
  theme_bw()

rbind.data.frame(preds, check) %>%
  group_by(type, age_cat, prs) %>%
  summarise(pred_p = mean(pred_p), .groups = "drop") %>%
  ggplot(
    data = ., 
    aes(x = age_cat, y = pred_p, group = interaction(type, prs))
  ) +
  geom_line(aes(color = prs, linetype = type)) +
  theme_bw()

# wide format
inner_join(
  newdat_base,
  check %>% rename(obs_p = pred_p) %>% select(-type),
  c("age_cat", "sei", "prs")
)


# ---- boot_setup ----

n_map <-
  dat_adu_ovo_sei %>%
  arrange(personid) %>%
  group_by(personid) %>%
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
  n_map_samp %>% select(personid),
  dat_adu_ovo_sei,
  "personid",
  multiple = "all"
)

# ---- boot_fns ----

sample_dat_with_replace <- function(x = NULL) {
  
  rand_id_rows <- sample(x = n_id, size = n_id_samp, replace = TRUE)
  
  n_map_samp <- n_map[rand_id_rows, ] # %>%
  # mutate(cum_n = cumsum(n)) %>%
  # dplyr::filter(cum_n <= n_obs)
  
  # return boot sample
  inner_join(
    n_map_samp %>% select(personid),
    dat_adu_ovo_sei,
    "personid",
    multiple = "all"
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
    group_by(sei, prs, age_cat, sex) %>%
    summarise(pred_p = mean(pred_val), .groups = "drop") %>%
    arrange(sei, prs, age_cat, sex)
}



newdat_base_pred <- 
  predict(
    mod_adu_ovo_sei, 
    newdata = dat_adu_ovo_sei, 
    type = "response", 
    allow.new.levels = FALSE, 
    re.form=NULL
  )
newdat_base <- 
  cbind(dat_adu_ovo_sei, pred_ovo = newdat_base_pred) %>%
  group_by(sei, prs, age_cat, sex) %>%
  summarise(pred_p = mean(pred_ovo), .groups = "drop") %>%
  arrange(sei, prs, age_cat, sex)

# test
get_pred_by_cats(mod_adu_ovo_sei, dat_adu_ovo_sei)
get_pred_by_cats(mod_adu_ovo_sei, dat_adu_ovo_sei) %>%
  full_join(
    .,
    newdat_base,
    c("sei", "prs", "age_cat", "sex")
  ) %>%
  mutate(diff = pred_p.x - pred_p.y) %>%
  dplyr::filter(is.na(diff) | (abs(diff) > 1e-9)) %>%
  nrow(.)


# ---- mod_sed_boot ----

fit_glmm_tmb <- function(x) {
  options(warn = 2) # make warnings errors to be caught
  mod_fit <-
    try({
      glmmTMB(
        ovo ~ waveC + sex + (age_cat + sei + prs)^2 + 
          (1 + waveC | hicid) + (1 | personid),
        family = binomial(), 
        data = x,
        REML = TRUE
        # verbose = TRUE
      )
    })
  options(warn = 0) # make warnings warnings again
  
  if ("try-error" %in% class(mod_fit)) {
    return(NA)
  } else {
    return(mod_fit)
  }
  
}

# test
all(
  abs(
    get_pred_by_cats(fit_glmm_tmb(dat_adu_ovo_sei), dat_adu_ovo_sei)$pred_p - 
      newdat_base$pred_p
  ) < 1e-9
)

# ---- do_bootstrap ----

R <- 1100 # allow model failures
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

conv_success <- function(x) {
  single_na <- 
    ifelse(
      class(x) == "logical", 
      any(is.na(x)),
      FALSE
    )
    
  if (single_na) {
    return(FALSE)
  } else {
    return(x$fit$convergence == 0)
  }
  
}
# conv_success(NA)
# conv_success(boot_df$mod[[5]])
# conv_success(boot_df$mod[[6]])

# remove rows of tibble that model failed
converge_success <- unlist(lapply(boot_df$mod, conv_success))
boot_df <-
  boot_df %>%
  dplyr::filter(converge_success)


# extract model predictions over all model fits
# takes ~ 5 min
tic()
boot_df <-
  boot_df %>%
  mutate(preds_df = future_map2(.x = mod, .y = data, .f = get_pred_by_cats))
toc()

# unlist(lapply(boot_df$mod, diagnose))

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

arrow::write_parquet(boot_df_preds, sink = "res/mod_ovo_adu_sei_boot_preds.parquet")

