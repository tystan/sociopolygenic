
# ---- libs ----

# suppressPackageStartupMessages({suppressWarnings({
  
  # data manipulation and plotting
  library("dplyr")
  library("forcats")
  library("ggplot2")
  library("ggpubr")
  library("tidyr")

  # table printing
  library("knitr")
  library("stringi")

  # efficient results storage and reading (.parquet)
  library("arrow")
  
# })})

# ---- consts ----

alpha <- 0.05


fig_y_scale <- function(y1, y2, by_val = 3) {
  scale_y_continuous(limits = c(y1, y2), breaks = seq(y1, y2, by = by_val))
}

fig_bmi_y_scale <- function() {
  fig_y_scale(14, 28)
}

fig_ovo_y_scale <- function() {
  fig_y_scale(0, 0.75, 0.15)
}

fig_sei_scale <- function() {
  scale_fill_viridis_d(option = "D", aesthetics = c("colour", "fill"))
}

fig_sep_scale <- function() {
  scale_fill_viridis_d(option = "C", aesthetics = c("colour", "fill"))
}

fig_theme <- function() {
    theme_bw() %+%
    theme(
      text = element_text(family = "serif"),
      axis.text = element_text(size = 8)
    )
}


print_mod_text <- function(file_string) {
  mod_txt <- readLines(file_string)
  cat(stri_wrap(mod_txt, normalize = FALSE, width = 100), sep = "\n")
}




# ---- chi_bmi_prs_sei_tab ----

# sei plot

df_preds <- arrow::read_parquet("res/mod_chi_bmi_sei_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds <-
  df_preds %>%
  group_by(sei, prs, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sei, prs, age_cat)


boot_df_preds <- arrow::read_parquet("res/mod_chi_bmi_sei_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci <-
  boot_df_preds %>%
  group_by(sei, prs, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sei, prs, age_cat)

chi_bmi_sei_tab_dat <-
  full_join(
    df_preds,
    boot_df_ci,
    c("sei", "prs", "age_cat")
  )

chi_bmi_sei_tab_dat %>%
  mutate(est_ci = sprintf("%2.1f (%2.1f, %2.1f)", mean_p, lo_p, up_p)) %>%
  select(sei, prs, age_cat, est_ci) %>%
  pivot_wider(., names_from = age_cat, values_from = est_ci) %>%
  kable(.)
  



# ---- chi_bmi_prs_sei_fig ----

prs_sei_plot <-
  chi_bmi_sei_tab_dat %>%
  rename(`PRS quintile` = prs) %>%
  ggplot(., aes(x = age_cat, group = sei, fill = sei)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  facet_wrap(`PRS quintile` ~ ., labeller = "label_both", ncol = 5) +
  geom_line(
    aes(x = age_cat, y = mean_p, group = sei, col = sei),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = mean_p, col = sei),
    alpha = 0.9
  ) +
  fig_sei_scale() +
  fig_bmi_y_scale() +
  fig_theme() +
  labs(
    col = "SEIFA\nquintile",
    fill = "SEIFA\nquintile",
    x = "Age category",
    y = "BMI"
  ) 

ggsave(
  plot = prs_sei_plot,
  filename = "fig/supp_fig_chi_bmi_prs_sei.png",
  width = 12,
  height = 7,
  dpi = 300
)


print(prs_sei_plot)


# ---- chi_bmi_prs_sep_tab ----



# sep plot

df_preds <- arrow::read_parquet("res/mod_chi_bmi_sep_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds <-
  df_preds %>%
  group_by(sep, prs, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sep, prs, age_cat)


boot_df_preds <- arrow::read_parquet("res/mod_chi_bmi_sep_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci <-
  boot_df_preds %>%
  group_by(sep, prs, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sep, prs, age_cat)

chi_bmi_sep_tab_dat <-
  full_join(
    df_preds,
    boot_df_ci,
    c("sep", "prs", "age_cat")
  )

chi_bmi_sep_tab_dat %>%
  mutate(est_ci = sprintf("%2.1f (%2.1f, %2.1f)", mean_p, lo_p, up_p)) %>%
  select(sep, prs, age_cat, est_ci) %>%
  pivot_wider(., names_from = age_cat, values_from = est_ci) %>%
  kable(.)



# ---- chi_bmi_prs_sep_fig ----

prs_sep_plot <-
  chi_bmi_sep_tab_dat %>%
  rename(`PRS quintile` = prs) %>%
  ggplot(., aes(x = age_cat, group = sep, fill = sep)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  facet_wrap(`PRS quintile` ~ ., labeller = "label_both", ncol = 5) +
  geom_line(
    aes(x = age_cat, y = mean_p, group = sep, col = sep),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = mean_p, col = sep),
    alpha = 0.9
  ) +
  fig_sep_scale() +
  fig_bmi_y_scale() +
  fig_theme() +
  labs(
    col = "SEP\nquintile",
    fill = "SEP\nquintile",
    x = "Age category",
    y = "BMI"
  ) 

ggsave(
  plot = prs_sep_plot,
  filename = "fig/supp_fig_chi_bmi_prs_sep.png",
  width = 12,
  height = 7,
  dpi = 300
)

print(prs_sep_plot)




# ---- chi_bmi_plot ----



# sei plot

df_preds_sei <- arrow::read_parquet("res/mod_chi_bmi_sei_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds_sei <-
  df_preds_sei %>%
  group_by(sei, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sei, age_cat)


boot_df_preds_sei <- arrow::read_parquet("res/mod_chi_bmi_sei_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci_sei <-
  boot_df_preds_sei %>%
  group_by(sei, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sei, age_cat)

plot_dat_sei <-
  full_join(
    df_preds_sei,
    boot_df_ci_sei,
    c("sei", "age_cat")
  )



sei_plot <-
  plot_dat_sei %>%
  ggplot(., aes(x = age_cat, group = sei, fill = sei)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  geom_line(
    aes(x = age_cat, y = mean_p, group = sei, col = sei),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = mean_p, col = sei),
    alpha = 0.9
  ) +
  fig_sei_scale() +
  fig_bmi_y_scale() +
  fig_theme() +
  labs(
    col = "SEIFA\nquintile",
    fill = "SEIFA\nquintile",
    x = "Age category",
    y = "BMI"
  ) 




# sep plot

df_preds_sep <- arrow::read_parquet("res/mod_chi_bmi_sep_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds_sep <-
  df_preds_sep %>%
  group_by(sep, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sep, age_cat)



boot_df_preds_sep <- arrow::read_parquet("res/mod_chi_bmi_sep_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci_sep <-
  boot_df_preds_sep %>%
  group_by(sep, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sep, age_cat)

plot_dat_sep <-
  full_join(
    df_preds_sep,
    boot_df_ci_sep,
    c("sep", "age_cat")
  )



sep_plot <-
  plot_dat_sep %>%
  ggplot(., aes(x = age_cat, group = sep, fill = sep)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  geom_line(
    aes(x = age_cat, y = mean_p, group = sep, col = sep),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = mean_p, col = sep),
    alpha = 0.9
  ) +
  fig_sep_scale() +
  fig_bmi_y_scale() +
  fig_theme() +
  labs(
    col = "SEP\nquintile",
    fill = "SEP\nquintile",
    x = "Age category",
    y = "BMI"
  ) 



ggarrange(sei_plot, sep_plot, nrow = 1, labels = c("(A)", "(B)")) 


ggsave(
  filename = "fig/supp_fig_3.png",
  width = 12,
  height = 7,
  dpi = 300
)




















# ---- chi_ovo_prs_sei_tab ----

# sei plot

df_preds <- arrow::read_parquet("res/mod_chi_ovo_sei_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds <-
  df_preds %>%
  group_by(sei, prs, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sei, prs, age_cat)


boot_df_preds <- arrow::read_parquet("res/mod_chi_ovo_sei_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci <-
  boot_df_preds %>%
  group_by(sei, prs, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sei, prs, age_cat)

chi_ovo_sei_tab_dat <-
  full_join(
    df_preds,
    boot_df_ci,
    c("sei", "prs", "age_cat")
  )

chi_ovo_sei_tab_dat %>%
  mutate(est_ci = sprintf("%0.2f (%0.2f, %0.2f)", mean_p, lo_p, up_p)) %>%
  select(sei, prs, age_cat, est_ci) %>%
  pivot_wider(., names_from = age_cat, values_from = est_ci) %>%
  kable(.)




# ---- chi_ovo_prs_sei_fig ----


prs_sei_plot <-
  chi_ovo_sei_tab_dat %>%
  rename(`PRS quintile` = prs) %>%
  ggplot(., aes(x = age_cat, group = sei, fill = sei)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  facet_wrap(`PRS quintile` ~ ., labeller = "label_both", ncol = 5) +
  geom_line(
    aes(x = age_cat, y = mean_p, group = sei, col = sei),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = mean_p, col = sei),
    alpha = 0.9
  ) +
  fig_sei_scale() +
  fig_ovo_y_scale() +
  fig_theme() +
  labs(
    col = "SEIFA\nquintile",
    fill = "SEIFA\nquintile",
    x = "Age category",
    y = "Probability of overweight/obese"
  ) 

ggsave(
  plot = prs_sei_plot,
  filename = "fig/supp_fig_chi_ovo_prs_sei.png",
  width = 12,
  height = 7,
  dpi = 300
)


print(prs_sei_plot)


# ---- chi_ovo_prs_sep_tab ----



# sep plot

df_preds <- arrow::read_parquet("res/mod_chi_ovo_sep_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds <-
  df_preds %>%
  group_by(sep, prs, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sep, prs, age_cat)


boot_df_preds <- arrow::read_parquet("res/mod_chi_ovo_sep_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci <-
  boot_df_preds %>%
  group_by(sep, prs, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sep, prs, age_cat)

chi_ovo_sep_tab_dat <-
  full_join(
    df_preds,
    boot_df_ci,
    c("sep", "prs", "age_cat")
  )

chi_ovo_sep_tab_dat %>%
  mutate(est_ci = sprintf("%0.2f (%0.2f, %0.2f)", mean_p, lo_p, up_p)) %>%
  select(sep, prs, age_cat, est_ci) %>%
  pivot_wider(., names_from = age_cat, values_from = est_ci) %>%
  kable(.)


# ---- chi_ovo_prs_sep_fig ----



prs_sep_plot <-
  chi_ovo_sep_tab_dat %>%
  rename(`PRS quintile` = prs) %>%
  ggplot(., aes(x = age_cat, group = sep, fill = sep)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  facet_wrap(`PRS quintile` ~ ., labeller = "label_both", ncol = 5) +
  geom_line(
    aes(x = age_cat, y = mean_p, group = sep, col = sep),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = mean_p, col = sep),
    alpha = 0.9
  ) +
  fig_sep_scale() +
  fig_ovo_y_scale() +
  fig_theme() +
  labs(
    col = "SEP\nquintile",
    fill = "SEP\nquintile",
    x = "Age category",
    y = "Probability of overweight/obese"
  ) 

ggsave(
  plot = prs_sep_plot,
  filename = "fig/supp_fig_chi_ovo_prs_sep.png",
  width = 12,
  height = 7,
  dpi = 300
)


print(prs_sep_plot)


# ---- chi_ovo_plot ----



# sei plot

df_preds_sei <- arrow::read_parquet("res/mod_chi_ovo_sei_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds_sei <-
  df_preds_sei %>%
  group_by(sei, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sei, age_cat)


boot_df_preds_sei <- arrow::read_parquet("res/mod_chi_ovo_sei_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci_sei <-
  boot_df_preds_sei %>%
  group_by(sei, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sei, age_cat)

plot_dat_sei <-
  full_join(
    df_preds_sei,
    boot_df_ci_sei,
    c("sei", "age_cat")
  )



sei_plot <-
  plot_dat_sei %>%
  ggplot(., aes(x = age_cat, group = sei, fill = sei)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  geom_line(
    aes(x = age_cat, y = mean_p, group = sei, col = sei),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = mean_p, col = sei),
    alpha = 0.9
  ) +
  fig_sei_scale() +
  fig_ovo_y_scale() +
  fig_theme() +
  labs(
    col = "SEIFA\nquintile",
    fill = "SEIFA\nquintile",
    x = "Age category",
    y = "Probability of overweight/obese"
  )




# sep plot

df_preds_sep <- arrow::read_parquet("res/mod_chi_ovo_sep_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds_sep <-
  df_preds_sep %>%
  group_by(sep, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sep, age_cat)



boot_df_preds_sep <- arrow::read_parquet("res/mod_chi_ovo_sep_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci_sep <-
  boot_df_preds_sep %>%
  group_by(sep, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sep, age_cat)

plot_dat_sep <-
  full_join(
    df_preds_sep,
    boot_df_ci_sep,
    c("sep", "age_cat")
  )



sep_plot <-
  plot_dat_sep %>%
  ggplot(., aes(x = age_cat, group = sep, fill = sep)) +
  geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
  geom_line(
    aes(x = age_cat, y = med_p, group = sep, col = sep),
    alpha = 0.6
  ) +
  geom_point(
    aes(x = age_cat, y = med_p, col = sep),
    alpha = 0.9
  ) +
  fig_sep_scale() +
  fig_ovo_y_scale() +
  fig_theme() +
  labs(
    col = "SEP\nquintile",
    fill = "SEP\nquintile",
    x = "Age category",
    y = "Probability of overweight/obese"
  ) 




ggarrange(sei_plot, sep_plot, nrow = 1, labels = c("(A)", "(B)")) 


ggsave(
  filename = "fig/supp_fig_4.png",
  width = 12,
  height = 7,
  dpi = 300
)





























