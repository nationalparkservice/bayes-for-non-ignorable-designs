library(ggridges)
library(hrbrthemes)
library(scales)

source("src/wrangle.r")
source("utils.R")

# Set example / project paths and source required utilities.
ex_path <- get_ex_path()
output_path <- get_output_path(ex_path, base_dir = "assets")

load_font()

results_root_naive <- "output/example-3/canopy-gaps-naive-2021"
den_naive <- if (!exists("den_naive")) {
  read_csv(file.path(results_root_naive, "99-misc", "basic-density.csv"))
}
y_rep_maxima_naive <- read_csv(sprintf(
  "%s/99-misc/y-rep-maxima.csv",
  results_root_naive
))
y_moments_naive <- read_csv(sprintf(
  "%s/99-misc/y-moments.csv",
  results_root_naive
))

results_root_informed <- "output/example-3/canopy-gaps-2021"
den_informed <- if (!exists("den_informed")) {
  read_csv(file.path(results_root_informed, "99-misc", "basic-density.csv"))
}
y_rep_maxima_informed <- read_csv(sprintf(
  "%s/99-misc/y-rep-maxima.csv",
  results_root_informed
))
y_moments_informed <- read_csv(sprintf(
  "%s/99-misc/y-moments.csv",
  results_root_informed
))


# ------ comparison ------
d_combined <- bind_rows(
  den_naive %>% mutate(method = "naive"),
  den_informed %>% mutate(method = "informed")
)
d_combined_subset <- d_combined %>%
  distinct(method, m, p_gt_thresh, mu, sigma, stratum_index, stratum_id)

d_stats_combined <- bind_rows(
  bind_rows(
    y_rep_maxima_naive %>%
      mutate(method = "Ignored", stat = max_y_rep, moment = "max(y)"),
    y_rep_maxima_informed %>%
      mutate(method = "Modeled", stat = max_y_rep, moment = "max(y)")
  ),
  bind_rows(
    y_moments_naive %>% mutate(method = "Ignored"),
    y_moments_informed %>% mutate(method = "Modeled")
  )
) %>%
  group_by(moment) %>%
  filter(stat <= quantile(stat, probs = 0.999)) %>%
  ungroup()

my_label_parsed <- function(variable, value) {
  if (variable == "stratum_id") {
    return(str_wrap(as.character(value), 20))
  } else {
    lapply(as.character(value), function(x) parse(text = x))
  }
}
p_a <- ggplot(d_stats_combined %>%
  # filter(moment == 'max(y)') %>%
  mutate(
    method = factor(method, levels = c("Modeled", "Ignored")),
    moment = factor(moment,
      levels = c("mu", "sigma", "max(y)"),
      labels = c("mu", "sigma", 'max*"("*italic(y)^rep*")"')
    )
  )) +
  facet_wrap(~moment, scales = "free", labeller = my_label_parsed) +
  geom_density(aes(x = stat, y = ..density.., fill = method, group = method),
    alpha = 0.5, adjust = 2,
    color = "black",
    size = 0.75
  ) +
  expand_limits(x = -0.05) +
  scale_fill_grey("Censoring and truncation", start = 0, end = 1) +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18
  ) +
  theme(
    axis.title.y = element_blank(), axis.text.y = element_blank(),
    axis.title.x = element_text(hjust = 0.5),
    legend.position = "bottom",
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "inches"), strip.text = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 3),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  ) + # labels = scales::scientific
  labs(x = "Canopy gap size (cm)", y = "") # Canopy gap size (cm)
# ggsave(file.path(output_path, 'example-3.jpg'), width = 16/2, height = 4)
ggsave(file.path(output_path, "example-3-fig-b.jpg"),
  plot = p_a, width = 8.25, height = 3.5
)

rnd <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits)
}
d_stats_combined_summary_raw <- d_stats_combined %>%
  # filter(stratum_index == 2) %>%
  group_by(method, moment, stratum_id) %>%
  # summarise_at(vars(stat), mean) %>%
  summarise(
    lower = get_interval(stat, interval = 0.95, method = "hdi")[1],
    median = median(stat),
    upper = get_interval(stat, interval = 0.95, method = "hdi")[2],
  ) %>%
  ungroup() # %>%
# d_stats_combined_summary <- d_stats_combined_summary_raw %>%
#   mutate(stat = sprintf(
#     "%s (%s, %s)",
#     rnd(mid), rnd(lwr), rnd(upr)
#   )) %>%
#   select(-mid, -lwr, -upr) %>%
#   spread(moment, stat) %>%
#   arrange(stratum_id)
# ex_3_stats <- as.data.frame(d_stats_combined_summary)
# write_csv(ex_3_stats, 'doc/status-and-trends-manuscript/stats/ex-3-stats.csv')
d_stats_ignored <- d_stats_combined_summary_raw %>%
  filter(method == "Ignored")
d_stats_modeled <- d_stats_combined_summary_raw %>%
  filter(method == "Modeled")
to_vector <- function(x, stat) {
  out <- unlist(
    x %>% filter(moment == stat) %>% select(median, lower, upper),
    use.names = FALSE
  )
  names(out) <- c("median", "lower", "upper")
  out
}
ex3_stats <- list(
  ignored = list(
    mu = to_vector(d_stats_ignored, "mu"),
    sigma = to_vector(d_stats_ignored, "sigma"),
    max_y = to_vector(d_stats_ignored, "max(y)")
  ),
  modeled = list(
    mu = to_vector(d_stats_modeled, "mu"),
    sigma = to_vector(d_stats_modeled, "sigma"),
    max_y = to_vector(d_stats_modeled, "max(y)")
  )
)
saveRDS(ex3_stats, file.path(output_path, "ex3.rds"))

d_ht <- read_csv("output/example-3/ht/output/CARE_StatusEstimates_AllYears_MeanCanopyGap.csv") %>%
  rename(
    stratum_id = Subpopulation, year = Year,
    stat = Estimate, lower = LCB95Pct, upper = UCB95Pct
  ) %>%
  mutate(stratum_id = ifelse(stratum_id == "AllSites", "Hartnet-deep grassland", stratum_id), moment = "mu")


d_bayes <- read_csv("output/example-3/canopy-gaps-2021/03-inference/strata/sampled/hat-stratum-means.csv") %>%
  rename(lower = `0.025`, upper = `0.975`, stat = mean)
d_both <- bind_rows(
  d_bayes %>% mutate(method = "bayes"),
  d_ht %>%
    select(any_of(names(d_bayes))) %>%
    mutate(method = "ht")
) %>%
  mutate(method = ifelse(method == "ht", "Horvitz–Thompson", "Bayesian"))
set.seed(123)
pos <- position_dodge(width = 1 / 3)
ddd <- read_csv("output/example-3/canopy-gaps-2021/00-input/state-variable-data.csv")
p_b <- ggplot(d_both) +
  geom_jitter(
    data = ddd, aes(x = cal_year, y = response), # %>% sample_frac(0.5)
    width = 1 / 6, alpha = 0.01, pch = 16, fill = "gray75"
  ) + # height = 1
  geom_errorbar(aes(
    x = year, ymin = lower, ymax = upper,
    group = method, color = method
  ),
  width = 0, position = pos, size = 0.9
  ) +
  geom_line(
    data = d_both %>% filter(method == "Bayesian"),
    aes(x = year, y = stat, group = method, color = method),
    size = 0.5, position = pos, linetype = "dashed"
  ) +
  # geom_jitter(data = ddd %>% filter(is_censored), aes(x = cal_year, y = censor_limit_vec), # %>% sample_frac(0.5)
  #             width = 1/6, pch = 4, alpha = 1, fill = 'gray75') + # height = 1
  geom_hline(yintercept = 20, alpha = 0.8, linetype = "dashed") +
  scale_y_continuous(trans = log_trans(), breaks = breaks_log()) + # , breaks = scales::pretty_breaks()
  geom_point(aes(x = year, y = stat, color = method),
    position = pos, pch = 21, fill = "white", size = 2.5, stroke = 0.9
  ) +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18, axis_title_just = "c"
  ) +
  scale_color_grey("Estimates", start = 0.5, end = 0) +
  labs(x = "", y = "Canopy gap size (cm)") +
  theme(
    legend.position = "bottom", legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-15, -15, -15, -15),
    axis.title.y = element_text(margin = margin(t = 0, r = 0.75, b = 0, l = 0, unit = "lines"))
  )

ggsave(file.path(output_path, "example-3-fig-a.jpg"),
  plot = p_b, width = 8.25, height = 3.5
)

perc_censored <- nrow(ddd %>% filter(is_censored)) / nrow(ddd) * 100

ex3_fig <- cowplot::plot_grid(p_b, # + labs(x = "Year") + theme(axis.title.x = element_text(hjust = 0.5)),
  p_a,
  labels = c("(a)", "(b)"), rel_heights = c(0.55, 0.45),
  label_size = 16, ncol = 1, label_fontfamily = "LM Roman 10"
)
ggsave(file.path(output_path, "example-3.jpg"),
  plot = ex3_fig, width = 8.25, height = 7
)

get_params <- function(mu, sigma) {
  c(
    alpha = log(mu) - 1 / 2 * log((sigma^2 + mu^2) / mu^2),
    beta = sqrt(log((sigma^2 + mu^2) / mu^2))
  )
}
#
d_labs <- d_combined_subset %>%
  group_by(method, stratum_id) %>%
  summarise(mu = mean(mu), sigma = mean(sigma), p_gt_thresh = mean(p_gt_thresh)) %>%
  ungroup()
d_labs
# which_result <- 1
# lnorm_params <- get_params(d_labs$mu[which_result], d_labs$sigma[which_result])  #
# # lnorm_params
#
#
# x <- seq(0, 1000)
# this_den <- dlnorm(x, lnorm_params[1], lnorm_params[2])
#
# if (which_result == 2) {
#   plot(x, this_den, type = 'l')  # naive
# } else {
#   lines(x, this_den, col = 'red')  # informed
# }
#
# 1 - plnorm(100, lnorm_params[1], lnorm_params[2])
# # plnorm(100, lnorm_params[1], lnorm_params[2], lower.tail = FALSE)
