library(tidyverse)
library(hrbrthemes)

source("src/utils-mcmc.R")
source("src/utils-math-stats.R")
source("utils.R")

# Set example / project paths and source required utilities.
ex_path <- get_ex_path()
output_path <- get_output_path(ex_path, base_dir = "assets")

# filelist_raw <- list.files('output/example-1', pattern = '.csv$', full.names = TRUE, recursive = TRUE)
# filelist <- grep('99-misc/d-|output/d-', filelist_raw, value = TRUE)
# d <- lapply(filelist, read_csv) %>% bind_rows()
# message(paste(filelist, collapse = ', '))
# dir.create('assets', showWarnings = FALSE) # temporary?
# write_csv(d, 'assets/d-combo.csv')

# Load the data we need for plotting.
z_jags <- readRDS("output/example-1/complete/99-misc/z-jags.rds")
z_jags_null <- readRDS("output/example-1/null/99-misc/z-jags.rds")
jags_data <- readRDS(file.path("output/example-1/complete", "00-input", "jags-data.rds"))
d <- read_csv("output/example-1/complete/00-input/state-variable-data.csv")

load_font()

# Observer effect summary (median and CIs, and percent change stats).
(obs_effect <- coef_stats(z_jags$Beta[1, , ]))

# readRDS(file.path("example-1/complete", ".00-input", "covariate-moments.rds"))
# read_csv(file.path("example-1/complete", "00-input", "state-variable-data.csv"))
# if p = 0.15 (or e.g., p = boot::inv.logit(-1.25)) in year 0, odds = p / (1 - p)
# if e(b1) = 1.115987, then the odds of hitting non-natives in year 1 would be odds_trend = 1.115987^seq(0, 10) * odds
# p_trend = 1 - 1 / (1 + odds_trend)

all_effects <- list(
  # ---- Multiplicative effects ----
  multiplicative_effects = list(
    obs_effect = c(
      obs_effect,
      c("perc_change" = (1 * obs_effect[["median"]] - 1) / 1)
    ),
    b1_complete = coef_stats(z_jags$mu.B1.park),
    b1_complete_per_yr = coef_stats(z_jags$mu.B1.park / sd(d$rel_year)),
    nonnative_spp_effect = coef_stats(z_jags$Beta[2, , ]),
    nonnative_spp_effect_unscaled = coef_stats(z_jags$Beta[2, , ] / jags_data$x.miss.sd),
    b1_naive = coef_stats(z_jags_null$mu.B1.park),
    # Non-native spp. trend, botanist effect, and rainfall effect.
    g1_park = coef_stats(z_jags$G1.park, transform = TRUE),
    # This next term is the multiplicative change in odds per unit time after accounting for other X.
    g1_park_per_yr = coef_stats(z_jags$G1.park / sd(d$rel_year), transform = TRUE),
    delta1 = coef_stats(z_jags$Gamma[1, , ], transform = TRUE),
    delta2 = coef_stats(z_jags$Gamma[2, , ], transform = TRUE) # ,
    # delta2_unscaled = coef_stats(z_jags$Gamma[2, , ] / 97.4, transform = TRUE)
  ),
  # ---- Additive effects ----
  additive_effects = list(
    delta2 = coef_stats(z_jags$Gamma[2, , ], transform = FALSE),
    # Influence of non-natives.
    nonnative_spp_effect = coef_stats(z_jags$Beta[2, , ], transform = FALSE) # TODO: go to native units, and add multiplicative version
  ),
  other_ests = list(
    nn_cov_first = coef_stats(z_jags$pred.park.p[1, , ], transform = FALSE),
    nn_cov_last = coef_stats(z_jags$pred.park.p[nrow(z_jags$pred.park.p), , ], transform = FALSE)
  )
)
# saveRDS(all_effects, "doc/status-and-trends-manuscript/stats/ex1.rds")
# saveRDS(all_effects, "assets/ex1.rds")
save_object(all_effects, output_path, "ex1.rds")

which_hat <- "hat.park.mean.at.min.x2"
dd <- bind_rows(
  data_for_obj(z_jags, "pred.park.mean", interval = 0.8, method = "hdi"),
  data_for_obj(z_jags, which_hat, interval = 0.8, method = "hdi")
)

dd_inset <- dd %>%
  filter(stat == "median") %>%
  mutate(lab = ifelse(obj == "pred.park.mean", "Actual", "Average"))
p_ex1_fig_2b_inset <- ggplot(dd_inset) +
  geom_line(aes(
    x = cal_year, y = val, group = interaction(stat, obj),
    linetype = lty, color = obj
  ), size = 1.1) + #
  scale_linetype_identity() +
  scale_color_grey(expression(Non * "-" * native ~ spp. ~ cover),
    labels = c("Average", "Actual"),
    start = 0.75, end = 0
  ) +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18
  ) +
  labs(x = "", y = expression(Mean ~ number ~ of ~ native ~ spp. %*% m^-2)) +
  theme(
    legend.position = "bottom", legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-15, -15, -15, -15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks())
ggsave(file.path(output_path, "example-1-inset.jpg"), # "doc/status-and-trends-manuscript/figures/example-1-inset.jpg",
  plot = p_ex1_fig_2b_inset, width = 7 * 0.95, height = 5 * 0.95
)

z_jags$diff.park.mean <- z_jags$pred.park.mean -
  z_jags[[which_hat]][which(jags_data$x.hat.raw %% 1 == 0), , ]
dd_diff <- data_for_obj(z_jags, "diff.park.mean",
  interval = 0.8, method = "hdi",
  ci_lty = "solid", include_draws = FALSE
)
dd_diff_wide <- dd_diff %>%
  filter(stat != "median") %>%
  pivot_wider(names_from = stat, values_from = val)
p_ex1_fig_2b <- ggplot(dd_diff) +
  geom_ribbon(
    data = dd_diff_wide, aes(x = cal_year, ymin = lower, ymax = upper),
    alpha = 0.5
  ) +
  geom_line(
    aes(
      x = cal_year, y = val, group = interaction(stat, obj),
      linetype = lty, size = lwd
    ),
    color = "black", alpha = 0.75
  ) +
  scale_linetype_identity() +
  scale_size_identity() +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18
  ) +
  labs(y = expression(Difference ~ "(" * native ~ spp. %*% m^-2 * ")"), x = "") +
  scale_x_continuous(breaks = scales::pretty_breaks())

dd_trythis <- data_for_obj(z_jags, "pred.park.p", interval = 0.8, method = "hdi") %>%
  mutate(val_new = scales::rescale(val, to = range(dd_diff$val)))

dd_trythis_wide <- dd_trythis %>%
  filter(stat != "median") %>%
  pivot_wider(-val_new, names_from = stat, values_from = val)

p_ex1_fig_2a <- ggplot(dd_trythis_wide) +
  geom_ribbon(
    aes(x = cal_year, ymin = lower, ymax = upper),
    alpha = 0.5,
  ) +
  geom_line(
    data = dd_trythis %>% mutate(lty = "solid"),
    aes(
      x = cal_year, y = val, group = interaction(stat, obj),
      linetype = lty, size = lwd
    ), color = "black", alpha = 0.75
  ) +
  scale_linetype_identity() +
  scale_size_identity() +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18
  ) +
  labs(y = expression(Non * "-" * native ~ spp. ~ cover %*% site^-1), x = "") +
  scale_x_continuous(breaks = scales::pretty_breaks())

ht_ests <- read_csv("output/example-1/ht/output/LIBI_StatusEstimates_AllYears_NativeRichnessOnly.csv")
d_comb <- bind_rows(
  data_for_obj(z_jags, "hat.park.mean", interval = 0.95, method = "hdi") %>%
    pivot_wider(matches("year|obj"), names_from = stat, values_from = val),
  ht_ests %>% select(
    cal_year = Year,
    lower = LCB95Pct, median = Estimate, upper = UCB95Pct
  ) %>%
    mutate(obj = "ht.ests")
) %>% mutate(est = ifelse(obj == "ht.ests", "Horvitz–Thompson", "Bayesian"))

set.seed(123)
pos <- position_dodge(0.45)

p_ex1_fig_1 <- ggplot() +
  geom_line(
    data = d_comb %>% filter(obj == "hat.park.mean"),
    aes(x = cal_year, y = median, group = est, color = est),
    size = 0.5, position = pos, linetype = "dashed"
  ) +
  geom_jitter(
    data = d, aes(x = cal_year, y = response),
    width = 0.2, height = 0.1, alpha = 0.075, pch = 16, fill = "gray75"
  ) +
  geom_errorbar(
    data = d_comb, aes(
      x = cal_year, ymin = lower, ymax = upper,
      group = est, color = est
    ),
    width = 0, size = 0.9, position = pos
  ) +
  geom_point(
    data = d_comb, aes(x = cal_year, y = median, group = est, color = est),
    pch = 21, fill = "white", size = 2.5, stroke = 0.9, position = pos
  ) +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18
  ) +
  labs(x = "", y = expression(Number ~ of ~ native ~ spp. %*% m^-2)) +
  theme(
    legend.position = "bottom", legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_color_grey("Estimates", # labels = c("Horvitz–Thompson", "Bayesian"),
    start = 0.5, end = 0
  )

ggsave(file.path(output_path, "example-1-fig1.jpg"), # "doc/status-and-trends-manuscript/figures/example-1-fig1.jpg",
  plot = p_ex1_fig_1,
  width = 7, height = 5
)

ex1_fig2 <- cowplot::plot_grid(
  p_ex1_fig_2a, p_ex1_fig_2b,
  labels = c("(a)", "(b)"),
  label_size = 16, ncol = 2, label_fontfamily = "LM Roman 10"
)
ggsave(file.path(output_path, "example-1-fig2.jpg"), # "doc/status-and-trends-manuscript/figures/example-1-fig2.jpg",
  plot = ex1_fig2, width = 7 * 2, height = 5
)


d_trend_raw <- bind_rows(
  # output/example-1/null/03-inference/03-inference/me/sampled/trend-park-avg-annual-change-posteriors.csv
  read_csv("output/example-1/null/03-inference/me/sampled/trend-park-avg-annual-change-posteriors.csv") %>%
    mutate(model = "null"),
  read_csv("output/example-1/complete/03-inference/me/sampled/trend-park-avg-annual-change-posteriors.csv") %>%
    mutate(model = "complete")
)

d_trend <- d_trend_raw %>% filter(stratum_id == "Park")
d_trend %>%
  group_by(model) %>%
  summarise(p = sum(gen_quant_val > 0) / n(), .groups = "drop")
p_ex1_trend <- ggplot(d_trend) +
  geom_vline(xintercept = 0, color = "black", size = 0.5, alpha = 0.25) +
  geom_density(aes(x = gen_quant_val, fill = model), alpha = 0.5, size = 0.8, adjust = 2) +
  scale_fill_grey("Nuisance (botanist)", labels = c("modeled", "ignored")) +
  theme_ipsum_rc(
    grid = "X", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18
  ) +
  labs(
    x = expression("Change in mean native spp. richness" %*% m^-2 %*% year^-1),
    y = "Density"
  ) +
  lims(x = quantile(d_trend$gen_quant_val, c(0.0005, 0.9995))) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )
ggsave(file.path(output_path, "example-1-trend.jpg"), # "doc/status-and-trends-manuscript/figures/example-1-trend.jpg",
  plot = p_ex1_trend, width = 7, height = 5
)
print(list.files(output_path, full.names = TRUE))
