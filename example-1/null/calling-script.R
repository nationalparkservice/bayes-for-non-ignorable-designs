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
coda_vars <- readRDS(file.path(ex_path, "00-input", "coda-vars.rds"))

# Adapt and update.
jags_model <- jags.model(
  file = jags_model_file,
  data = jags_data,
  inits = jags_inits,
  n.chains = 3,
  n.adapt = floor(5000 * frac_iter) # jags_n_iters['n_adapt']
)
update(
  object = jags_model,
  n.iter = floor(5000 * frac_iter) # jags_n_iters['n_update']
)

# Sample.
z_jags <- jags.samples(
  model = jags_model,
  variable.names = grep("B.hat|Tau.B|site.wt|p.site", jags_vars, invert = TRUE, value = TRUE),
  n.iter = floor(3000 * frac_iter) # jags_n_iters['n_iter']
)
save_object(z_jags, file.path(output_path, "99-misc"), "z-jags.rds")
# saveRDS(z_jags, file.path(output_path[2], 'z-jags.rds')) # was: 'example-1/null/99-misc/z-jags.rds'
z_coda <- coda.samples(
  model = jags_model,
  variable.names = coda_vars,
  n.iter = floor(3000 * frac_iter) # jags_n_iters['n_iter']
)

# Model checking and diagnostics.
convergence_diagnostic <- gelman.diag(z_coda, multivariate = FALSE)
bayesian_p <- sapply(c("p.mean", "p.sd"), function(t_stat) {
  summary(z_jags[[t_stat]], mean)$stat
}) # see also: summary(z_coda)

# Posterior predictive loss, DIC, etc.
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
