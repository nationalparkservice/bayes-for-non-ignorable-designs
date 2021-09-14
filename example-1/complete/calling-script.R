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

# Load the 'inits' originally used to fit the model.
jags_inits <- readRDS(file.path(ex_path, "00-input", "jags-inits.rds"))
jags_inits <- lapply(jags_inits, function(x) {
  # browser()
  x$mu.B0 <- rnorm(jags_data$y.n.strata, 2, 0.15)
  x$mu.B1 <- rnorm(jags_data$y.n.strata, 0, 0.05)
  x$mu.G0 <- rnorm(jags_data$y.n.strata, -1.5, 0.2)
  x$G1 <- rnorm(jags_data$y.n.strata, 0.25, 0.1)
  x$sigma.B0 <- runif(jags_data$y.n.strata, 0, 1)
  x$sigma.B1 <- runif(jags_data$y.n.strata, 0, 0.5)
  x$sigma.G0 <- runif(jags_data$y.n.strata, 0, 1)
  x
})

# Load the variables we're watching.
jags_vars <- readRDS(file.path(ex_path, "00-input", "jags-vars.rds"))
# coda_vars <- readRDS(file.path(ex_path, '00-input', 'coda-vars.rds'))

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

# Sample.
z_jags <- jags.samples(
  model = jags_model,
  variable.names = unique(
    c(
      jags_vars, "mu.G0", "sigma.G0", "G1", "Gamma",
      "hat.strat.mean", "pred.strat.mean", "pred.strat.p",
      "pred.strat.new.obs", "hat.strat.new.obs",
      "pred.strat.hits", "pred.strat.hits.z",
      "hat.park.mean", "pred.park.mean",
      "trend.park.diff", "trend.park.avg.annual.change",
      "trend.stratum.diff", "trend.stratum.avg.annual.change",
      "p.mean", "p.sd", "j.hat.draw", "i.pred",
      "mu.sigma.x.miss", "sigma.sigma.x.miss", "sigma.x.miss.tilde",
      "eps.tilde", "tau.tilde", "pred.park.p",
      "hat.park.mean.at.min.x2", "mu.G0.park", "G1.park"
    )
  ),
  n.iter = floor(3000 * frac_iter) # jags_n_iters['n_iter']
)
save_object(z_jags, file.path(output_path, "99-misc"), "z-jags.rds")

z_coda <- coda.samples(
  model = jags_model,
  variable.names = c(
    "Beta", "Gamma", "mu.B0", "sigma.B0", "mu.B1", "sigma.B1",
    "mu.G0", "sigma.G0", "G1"
  ),
  n.iter = floor(3000 * frac_iter) # jags_n_iters['n_iter']
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
get_park_scale_inference(z_jags, d, jags_data, output_path,
  response_desc,
  n_draws = 1000, seed = 123
)
get_trend_inference(z_jags, d, output_path, response_desc)
