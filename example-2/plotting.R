library(tidyverse)
library(hrbrthemes)
library(ggridges)
source("utils.R")

# Set example / project paths and source required utilities.
ex_path <- get_ex_path()
output_path <- get_output_path(ex_path, base_dir = "assets")

# save_to_ms <- FALSE
# output_dir <-
#  if (save_to_ms) 'doc/status-and-trends-manuscript' else 'assets/example-2'

load_font()

base_dir <- "output/example-2"
dirs <- c(
  "with_hrs" = file.path(base_dir, "hrs_wyppt_mm"),
  "no_hrs" = file.path(base_dir, "wyppt_mm")
)

# ---- "Bumpy" plot results -----

results <- lapply(dirs, function(dir) {
  message("Loading", " ", dir)
  z_jags <- readRDS(file.path(dir, "99-misc", "z-jags.rds"))
  stratum_lookup <- read_csv(file.path(dir, "00-input", "stratum-ids-and-indices.csv"))
  year_lookup <- read_csv(file.path(dir, "00-input", "calendar-and-relative-years.csv")) %>%
    complete(cal_year = seq(min(cal_year), max(cal_year))) %>%
    mutate(rel_year = cal_year - min(cal_year), timestep = rel_year + 1)

  tmp_strat <- z_jags$pred.strat.mean
  dimnames(tmp_strat) <- list(
    "timestep" = 1:dim(tmp_strat)[1],
    "stratum_index" = 1:dim(tmp_strat)[2],
    "iter" = 1:dim(tmp_strat)[3],
    "chain" = 1:dim(tmp_strat)[4]
  )
  pred_strat_mean <- as.data.frame.table(tmp_strat, responseName = "z") %>%
    as_tibble() %>%
    mutate_if(is.factor, as.integer) %>%
    left_join(stratum_lookup, by = "stratum_index")

  tmp_park <- z_jags$pred.park.mean
  dimnames(tmp_park) <- list(
    "timestep" = 1:dim(tmp_park)[1],
    "iter" = 1:dim(tmp_park)[2],
    "chain" = 1:dim(tmp_park)[3]
  )
  pred_park_mean <- as.data.frame.table(tmp_park, responseName = "z") %>%
    as_tibble() %>%
    mutate_if(is.factor, as.integer) %>%
    mutate(stratum_id = "Park")

  pred_mean_summary_wide <- bind_rows(pred_strat_mean, pred_park_mean) %>%
    left_join(year_lookup, by = "timestep") %>%
    group_by(cal_year, stratum_id) %>%
    summarise(
      median = median(z),
      mean = mean(z),
      lwr = HDInterval::hdi(z, credMass = 0.8)[1],
      upr = HDInterval::hdi(z, credMass = 0.8)[2],
      .groups = "drop"
    )
  pred_mean_summary_long <- pred_mean_summary_wide %>%
    pivot_longer(-any_of(c("cal_year", "stratum_id")), names_to = "stat", values_to = "val") %>%
    mutate(lty = ifelse(stat %in% c("mean", "median"), "solid", "dashed"))

  out <- list(
    z_jags = z_jags,
    pred_mean_summary_long = pred_mean_summary_long
  )
  jags_data <- readRDS(file.path(dir, "00-input", "jags-data.rds"))
  attr(out, "stratum_lookup") <- stratum_lookup
  attr(out, "year_lookup") <- year_lookup
  attr(out, "jags_data") <- jags_data

  if (grepl("hrs", dir)) {
    # note: p is the median of the posterior in this case
    pr_stats_mat <- apply(z_jags$pred.park.pr, c(1, 2), median)
    dimnames(pr_stats_mat) <-
      list(timestep = 1:nrow(pr_stats_mat), response = 1:ncol(pr_stats_mat))
    # browser()
    pr_stats_df <- as_tibble(pr_stats_mat, rownames = "timestep") %>%
      # mutate(across()) %>% # timestep = as.integer(timestep)
      pivot_longer(-timestep, names_to = "response", values_to = "p") %>%
      mutate(across(c(timestep, response), as.integer))
    attr(out, "pr_stats_df") <- pr_stats_df %>% left_join(year_lookup)

    # ggplot(pr_stats_df) +
    #   facet_grid(.~timestep) +
    #   geom_col(aes(x = response, y = median)) +
    #   coord_flip()
  }

  out
  # browser()
})

jags_data <- attr(results$with_hrs, "jags_data")
which_beta <- which(dimnames(jags_data$X)[[2]] == "fe_hrs")
beta_hrs <- results$with_hrs$z_jags$Beta[, which_beta, , ]

which_stratum_id <- c("V101", "V103")
which_stratum_index <- attr(results$with_hrs, "stratum_lookup") %>%
  filter(stratum_id %in% which_stratum_id)

all_effects <- list(
  beta_hrs_v101 = coef_stats(
    beta_hrs[which_stratum_index$stratum_index[which_stratum_index$stratum_id == "V101"], , ],
    transform = FALSE
  ),
  beta_hrs_v103 = coef_stats(
    beta_hrs[which_stratum_index$stratum_index[which_stratum_index$stratum_id == "V103"], , ],
    transform = FALSE
  )
)

data_raw <- lapply(names(dirs), function(x) {
  results[[x]]$pred_mean_summary_long %>% mutate(run = x)
})

data <- bind_rows(data_raw) %>%
  filter(
    run %in% c("no_hrs", "with_hrs"), # c('park_Beta', cstratum_Beta'),
    stat %in% c("lwr", "median", "upr")
  ) %>%
  pivot_wider(-lty, names_from = "stat", values_from = "val") %>%
  mutate(
    cost_dist = ifelse(run == "no_hrs", "ignored", "modeled"),
    cost_dist = factor(cost_dist,
      levels = c("modeled", "ignored"),
      labels = c("Modeled", "Ignored")
    )
  )

just_yrs <- attr(results$with_hrs, "year_lookup")
year_labs <- seq(min(just_yrs$cal_year) + 1, max(just_yrs$cal_year), 4)
p_b <- ggplot(data %>% filter(stratum_id %in% c("Park", "V101", "V103"))) + # c('lwr', 'mean', 'upr')
  facet_wrap(~stratum_id, nrow = 1) +
  geom_ribbon(aes(
    x = cal_year, ymin = lwr, ymax = upr, group = run,
    color = cost_dist, fill = cost_dist
  ), alpha = 0.8) +
  geom_line(aes(
    x = cal_year, y = median, group = run,
    color = cost_dist
  ), size = 0.75) +
  # scale_linetype_identity() +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18, axis_title_just = "c"
  ) +
  scale_y_continuous(breaks = seq(1, 6), lim = c(1, 6)) +
  scale_x_continuous(breaks = year_labs) +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  labs(x = "", y = "Soil stability") +
  scale_color_grey("Hiking time", start = 0, end = 1) +
  scale_fill_grey("Hiking time", start = 1, end = 0) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    # legend.box.margin = margin(-15, -15, -15, -15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )
# p_b

# ---- "Smooth" plots alongside data ----

base_dir <- "output/example-2/hrs_wyppt_mm"

d_obs <- read_csv(
  file.path(base_dir, "00-input", "state-variable-data.csv")
)

chain_posts <- lapply(1:3, function(chain) {
  out <- results$with_hrs$z_jags$hat.park.mean[, , chain]
  dimnames(out) <- list(timestep = 1:nrow(out), iter = 1:ncol(out))
  as_tibble(out, rownames = "timestep") %>%
    mutate(timestep = as.integer(timestep)) %>%
    pivot_longer(-timestep, names_to = "iter", values_to = "mean") %>%
    mutate(chain = chain)
})
posts <- bind_rows(chain_posts) %>%
  left_join(attr(results$with_hrs, "year_lookup")) %>%
  mutate(iter = paste(iter, chain, sep = "-"))


year_labs <- seq(min(d_obs$cal_year), max(d_obs$cal_year), 2)
posts_new <- posts %>% filter(iter %in% sample(unique(iter), 3000)) #

p_a <- ggplot(d_obs) +
  geom_hline(
    yintercept = seq(min(d_obs$cal_year), max(d_obs$cal_year)),
    alpha = 0.25, linetype = "dashed"
  ) +
  geom_path(
    data = posts_new %>% arrange(cal_year),
    aes(x = mean, y = cal_year, group = iter), alpha = 0.01
  ) +
  geom_density_ridges2(aes(x = response, y = cal_year, group = cal_year),
    stat = "binline", binwidth = 1, scale = 0.8,
    draw_baseline = FALSE, fill = "white", alpha = 0.75
  ) + # fill = midpoint_mean / 100
  labs(x = "Soil stability", y = "") +
  scale_y_continuous(breaks = year_labs) +
  scale_x_continuous(breaks = seq(1, 6)) +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18, axis_title_just = "c"
  ) +
  coord_flip() +
  theme(legend.key.width = unit(0.5, "inches"))
# p_a

d_ht_raw <- read_csv("output/example-2/ht/output/ORPI_StatusEstimates_AllYears_MedianStabilityRdUp.csv") %>%
  rename(stratum_id = Subpopulation, response = Category, cal_year = Year, p = Estimate.P) %>%
  mutate(stratum_id = ifelse(stratum_id == "AllSites", "Park", stratum_id))
d_ht <- d_ht_raw %>% filter(stratum_id == "Park") # %>% mutate(p = ifelse(is.na(p), 0, p))

d_ht_hack <- bind_rows(
  d_ht %>% mutate(method = "Horvitz–Thompson"),
  attr(results$with_hrs, "pr_stats_df") %>% mutate(method = "Bayesian")
) %>% # filter(cal_year == 2009) %>%
  # d_ht %>% mutate(method = "Horvitz–Thompson") %>%
  group_by(method) %>%
  mutate(response_left = response - 0.5, response_right = response + 0.5) %>%
  ungroup() %>%
  group_by(method, cal_year) %>%
  mutate(p = p / sum(p, na.rm = T)) %>%
  ungroup() %>%
  select(stratum_id, cal_year, response_left, response_right, p, method) %>%
  pivot_longer(all_of(c("response_left", "response_right")), names_to = "boundary", values_to = "response")


p_ht <- ggplot(
  d_ht_hack, # %>% filter(method == 'Bayesian'),
  aes(
    x = response, y = cal_year, group = interaction(cal_year, method),
    fill = method, height = p
  )
) +
  facet_wrap(~method, ncol = 1) +
  geom_hline(
    yintercept = seq(min(d_obs$cal_year), max(d_obs$cal_year)),
    alpha = 0.25, linetype = "dashed"
  ) +
  geom_density_ridges2(
    stat = "identity", scale = 0.8,
    draw_baseline = FALSE, alpha = 0.8 # fill = "black",
  ) +
  labs(x = "Soil stability", y = "") +
  scale_y_continuous(breaks = year_labs) +
  scale_x_continuous(breaks = seq(1, 6)) +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18, axis_title_just = "c"
  ) +
  coord_flip() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-15, -15, -15, -15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    strip.text = element_blank()
  ) +
  scale_fill_grey("", start = 0, end = 1)
p_ht

plot_list <- lapply(
  list(p_a, p_ht, p_b), function(x) x + theme(plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), "inches"))
)
mapply(function(x, y) {
  # fig_file_path <- get_asset_path(output_dir, sprintf('%s.jpg', y),
  # if (save_to_ms) 'figures' else NULL)
  ggsave(file.path(output_path, sprintf("%s.jpg", y)), plot = x, width = 7, height = 5)
}, x = list(p_a, p_ht, p_b), y = paste("example-2-fig", letters[1:3], sep = "-"))
ex2_fig <- cowplot::plot_grid(
  plotlist = plot_list,
  labels = c("(a)", "(b)", "(c)"), rel_heights = c(1, 1, 1),
  label_size = 16, ncol = 1, label_fontfamily = "LM Roman 10"
)

# fig_file_path <- get_asset_path(save_to_ms, 'example-2-fig-abc.jpg',
# if (save_to_ms) 'figures' else NULL)
ggsave(file.path(output_path, "example-2-fig-abc.jpg"),
  plot = ex2_fig, width = 8.5, height = 3 * 4
)


d_trend <- read_csv("output/example-2/hrs_wyppt_mm/03-inference/me/sampled/trend-park-avg-annual-change-quantiles.csv")
trend_stats_park <- d_trend %>%
  filter(stratum_id == "Park") %>%
  select(mean, lower = quantile_0.25, upper = quantile_97.5) %>%
  unlist()
all_effects <- c(all_effects, list(avg_change_per_year = trend_stats_park))
# stats_file_path <- get_asset_path(save_to_ms, 'ex2.rds',
# if (save_to_ms) 'stats' else NULL)
# saveRDS(all_effects, file.path(output_path, 'ex2.rds'))
save_object(all_effects, output_path, "ex2.rds")
print(list.files(output_path, full.names = TRUE))
