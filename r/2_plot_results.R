
# data manipulation and plotting
library("dplyr")
library("forcats")
library("ggplot2")
library("ggpubr")



# efficient results storage and reading (.parquet)
library("arrow")


# ---- consts ----

alpha <- 0.05



# ---- supp_fig_3 ----


# sei plot

df_preds <- arrow::read_parquet("res/mod_bmi_chi_sei_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds <-
  df_preds %>%
  group_by(sei, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sei, age_cat)


boot_df_preds <- arrow::read_parquet("res/mod_bmi_chi_sei_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci <-
  boot_df_preds %>%
  group_by(sei, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sei, age_cat)

plot_dat <-
  full_join(
    df_preds,
    boot_df_ci,
    c("sei", "age_cat")
  )


sei_plot <-
  plot_dat %>%
    ggplot(., aes(x = age_cat, group = sei, fill = sei)) +
    geom_ribbon(aes(ymin = lo_p, ymax = up_p), alpha = 0.1) +
    geom_line(
      aes(x = age_cat, y = med_p, group = sei, col = sei),
      alpha = 0.6
    ) +
    geom_point(
      aes(x = age_cat, y = med_p, col = sei),
      alpha = 0.9
    ) +
    scale_fill_viridis_d(option = "D") +
    scale_colour_viridis_d(option = "D") +
    ylim(15, 27) +
    labs(
      col = "SEIFA\nquintile",
      fill = "SEIFA\nquintile",
      x = "Age category",
      y = "BMI"
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "serif"),
      axis.text = element_text(size = 8)
    )

# sep plot

df_preds <- arrow::read_parquet("res/mod_bmi_chi_sep_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
df_preds <-
  df_preds %>%
  group_by(sep, age_cat) %>%
  summarise(
    mean_p = mean(pred_p),
    .groups = "drop"
  ) %>%
  arrange(sep, age_cat)



boot_df_preds <- arrow::read_parquet("res/mod_bmi_chi_sep_boot_preds.parquet")

# assuming equal gender and prs quintile weightings, 
# plain average is what we are after
boot_df_ci <-
  boot_df_preds %>%
  group_by(sep, age_cat) %>%
  summarise(
    n_boot = n(), 
    med_p = median(pred_p), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sep, age_cat)

plot_dat <-
  full_join(
    df_preds,
    boot_df_ci,
    c("sep", "age_cat")
  )

sep_plot <-
  plot_dat %>%
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
  scale_fill_viridis_d(option = "C") +
  scale_colour_viridis_d(option = "C") +
  ylim(15, 27) +
  labs(
    col = "SEP\nquintile",
    fill = "SEP\nquintile",
    x = "Age category",
    y = "BMI"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(size = 8)
  )

ggarrange(sei_plot, sep_plot, nrow = 1) 


ggsave(
  filename = "fig/supp_fig_3.png",
  width = 12,
  height = 7,
  dpi = 300
)


# ---- supp_fig_6 ----

boot_df_preds <- arrow::read_parquet("res/mod_ovo_chi_sep_boot_preds.parquet")

boot_df_ci <-
  boot_df_preds %>%
  group_by(sep, prs, age_cat) %>%
  summarise(
    n_boot = n(), 
    mean_p = mean(pred_p  ), 
    med_p = median(pred_p  ), 
    lo_p = quantile(pred_p, alpha / 2), 
    up_p = quantile(pred_p, 1 - alpha / 2), 
    .groups = "drop"
  ) %>%
  arrange(sep, prs, age_cat)


boot_df_ci %>%
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
