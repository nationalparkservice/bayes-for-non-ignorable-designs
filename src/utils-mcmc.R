concat_chains <- function(mcmc_output, axis, n_chains = 3, drop = TRUE) {
  obj_list <- if (axis == 2) {
    lapply(seq_len(n_chains), function(x) mcmc_output[, x, drop = drop])
  } else if (axis == 3) {
    lapply(seq_len(n_chains), function(x) mcmc_output[, , x, drop = drop])
  } else if (axis == 4) {
    lapply(seq_len(n_chains), function(x) mcmc_output[, , , x, drop = drop])
  } else if (axis == 5) {
    lapply(seq_len(n_chains), function(x) mcmc_output[, , , , x, drop = drop])
  }
  out <- abind::abind(obj_list, along = (axis - 1))

  if (length(dim(out)) == 2) {
    dimnames(out) <- list(seq.int(nrow(out)), seq.int(ncol(out)))
  }
  out
}

thin_mcmc_output <- function(mcmc_output, axis, n_draws, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (axis == 2) {
    mcmc_output[, sample(1:ncol(mcmc_output), min(ncol(mcmc_output), n_draws))]
  } else if (axis == 3) {
    mcmc_output[, , sample(1:dim(mcmc_output)[3], min(dim(mcmc_output)[3], n_draws))]
  }
}
