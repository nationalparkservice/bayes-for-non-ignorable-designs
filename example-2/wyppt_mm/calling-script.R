library(tidyverse)
library(rjags)
library(coda)
load.module("dic")
source("utils.R")

args <- commandArgs(trailingOnly = TRUE)
frac_iter <- if (length(args) == 0) 1 else 1 / 4

# Set example / project paths and source required utilities.
ex_path <- get_ex_path()
output_path <- get_output_path(ex_path)

# Load the tabular data, just for reference.
d <- read_csv(file.path(ex_path, "00-input", "state-variable-data.csv"))

# Load the model description.
jags_model_file <- list.files(ex_path, "^model*.jags$", full.names = TRUE)

# Load the data list required by JAGS.
jags_data <- readRDS(file.path(ex_path, "00-input", "jags-data.rds"))

# Basic analysis breadcrumbs (likelihood, deterministic model, etc.).
jags_info <- readRDS(file.path(ex_path, "00-input", "jags-info.rds"))
jags_n_iters <- readRDS(file.path(ex_path, "00-input", "jags-n-iters.rds"))
eval_mean_for_tv_covariates <- # determines whether 'bumpy' plots are produced
  readRDS(file.path(ex_path, "00-input", "eval-mean-for-tv-covariates.rds"))

# Load the 'inits' originally used to fit the model.
jags_inits <- readRDS(file.path(ex_path, "00-input", "jags-inits.rds"))

# Load the variables we're watching.
jags_vars <- readRDS(file.path(ex_path, "00-input", "jags-vars.rds"))
# coda_vars <- readRDS(file.path(ex_path, '00-input', 'coda-vars.rds'))

# head(jags_data$X)
scale_atts_df <- readRDS(file.path(ex_path, "00-input/covariate-moments.rds"))
stratum_lookup <- read_csv(file.path(ex_path, "00-input", "stratum-ids-and-indices.csv"))

X_raw <- d %>%
  # bind_cols(read_csv(file.path(ex_path, 'new_X.csv'))) %>%
  # select(stratum_id, any_of(dimnames(jags_data$X)[[2]])) %>%
  transmute(stratum_id, fe_WYppt_mm = fe_WYppt_mm *
    scale_atts_df$`scaled:scale`[scale_atts_df$fe_column == "WYppt_mm"] +
    scale_atts_df$`scaled:center`[scale_atts_df$fe_column == "WYppt_mm"])

jags_data$X <- X_raw %>%
  group_by(stratum_id) %>%
  mutate(fe_WYppt_mm = scale(fe_WYppt_mm)[, 1]) %>% # fe_hrs = scale(fe_hrs)[,1],
  ungroup() %>%
  select(fe_WYppt_mm) %>% # fe_hrs,
  as.matrix()

X_raw_moments <- X_raw %>%
  group_by(stratum_id) %>%
  summarise(mean = mean(fe_WYppt_mm), sd = sd(fe_WYppt_mm), .groups = "drop") %>%
  left_join(stratum_lookup)


X_pred_raw <- as_tibble(jags_data$X.pred) %>%
  transmute(fe_WYppt_mm = fe_WYppt_mm *
    scale_atts_df$`scaled:scale`[scale_atts_df$fe_column == "WYppt_mm"] +
    scale_atts_df$`scaled:center`[scale_atts_df$fe_column == "WYppt_mm"])
jags_data$X.pred <- X_pred_raw %>%
  mutate(stratum_index = jags_data$k.pred) %>%
  left_join(X_raw_moments) %>%
  transmute(fe_WYppt_mm = (fe_WYppt_mm - mean) / sd) %>%
  as.matrix()

# Adapt and update.
jags_model <- jags.model(
  file = jags_model_file,
  data = jags_data,
  # inits = jags_inits,
  n.chains = 3,
  n.adapt = floor(5000 * frac_iter) # jags_n_iters['n_adapt']
)
update(
  object = jags_model,
  n.iter = floor(5000 * frac_iter) # jags_n_iters['n_update']
)

# colMeans(jags_data$X)

# d %>% group_by(stratum_id) %>% summarise(mean_hrs = mean(fe_hrs), sd_hrs = sd(fe_hrs))
# ggplot(d) +
#  facet_wrap(~stratum_id) +
#  geom_vline(xintercept = 0, color = 'red') +
#  geom_histogram(aes(x = fe_hrs, y = ..density..))

# Sample.
not_needed <- paste(
  c(
    "*.tilde", "j.hat.draw", "*.site", "pr$", "sigma", "site.wt", "tau",
    "i.pred", "theta", "epsilon"
  ),
  collapse = "|"
)
z_jags <- jags.samples(
  model = jags_model,
  variable.names = grep(not_needed, jags_vars, value = TRUE, invert = TRUE),
  n.iter = floor(3000 * frac_iter) # jags_n_iters['n_iter']
)
save_object(z_jags, file.path(output_path, "99-misc"), "z-jags.rds")

# which(dimnames(jags_data$X)[[2]] == "fe_hrs")
# hist(z_jags$Beta[1, 1, , ])
# hist(z_jags$Beta[2, 1, , ])
# hist(z_jags$Beta[3, 1, , ])
# hist(z_jags$Beta[4, 1, , ])

# z_coda <- coda.samples(
#   model = jags_model,
#   variable.names = coda_vars,
#   n.iter = floor(3000 * frac_iter)#jags_n_iters['n_iter']
# )
#
# # Model checking and diagnostics.
# convergence_diagnostic <- gelman.diag(z_coda, multivariate = FALSE)
# bayesian_p <- sapply(c('p.mean', 'p.sd'), function(t_stat) {
#   summary(z_jags[[t_stat]], mean)$stat
# })  # see also: summary(z_coda)
#
# # Posterior predictive loss, DIC, etc.
# L <- jags_info$likelihood
# post_pred_loss <- z_jags %>% get_ppl(d, L)
# DIC <- z_jags %>% get_dic(d, L, jags_data)

# Inference.
response_desc <- jags_info$description
get_park_scale_inference(z_jags, d, jags_data, output_path,
  response_desc,
  n_draws = 1000, seed = 123
)
get_trend_inference(z_jags, d, output_path, response_desc)
