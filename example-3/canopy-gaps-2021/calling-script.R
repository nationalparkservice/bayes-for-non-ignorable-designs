library(tidyverse)
library(rjags)
library(coda)
load.module("dic")
source("utils.R")
source("src/utils-fit.R") # for `show_data_in_output()`

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
  n.adapt = floor(3000 * frac_iter) # jags_n_iters['n_adapt']
)
update(
  object = jags_model,
  n.iter = floor(3000 * frac_iter) # jags_n_iters['n_update']
)

# Sample.
z_jags <- jags.samples(
  model = jags_model,
  variable.names = c(
    "mu.B0", "sigma.B0", "B1", "B0.tilde", "sigma.tilde",
    "hat.strat.mean", "hat.park.mean",
    "mu.B0.park", "B1.park",
    "trend.park.diff", "trend.park.avg.annual.change",
    "trend.stratum.diff", "trend.stratum.avg.annual.change",
    "p.mean", "p.sd", "y.rep"
  ),
  n.iter = floor(5000 * frac_iter) # jags_n_iters['n_iter']
)

z_coda <- coda.samples(
  model = jags_model,
  variable.names = coda_vars, #
  n.iter = floor(5000 * frac_iter) # jags_n_iters['n_iter']
)
# sink(file.path(ex_path, '03-inference', 'coda-samples-summary.txt'))
print(summary(z_coda))
# sink()

# Model checking and diagnostics.
convergence_diagnostic <- gelman.diag(z_coda, multivariate = FALSE)
convergence_diagnostic
bayesian_p <- sapply(c("p.mean", "p.sd"), function(t_stat) {
  summary(z_jags[[t_stat]], mean)$stat
}) # see also: summary(z_coda)

# Inference.
response_desc <- jags_info$description

get_park_scale_inference(z_jags, d, jags_data, output_path, response_desc,
  n_draws = 1000, seed = 123
)
get_trend_inference(z_jags, d, output_path, response_desc)
get_density(z_jags, d, jags_info$deterministic_model, jags_info$likelihood, output_path,
  thresh = 250
)

get_stratum_inference(z_jags, d, output_path, response_desc, jags_data$x.hat.raw, "hat",
  jags_info$likelihood,
  n_draws = 250
)
