show_data_in_output <- function(mcarray_list) {
  if ("hat.site.new.obs" %in% names(mcarray_list)) {
    c(TRUE, FALSE)
  } else {
    FALSE
  }
}

get_L <- function(analysis_scenario) {
  analysis_scenario[["likelihood"]]
}

get_mod_summary <- function(mcarray_list, post_pred_loss, DIC, convergence_diagnostic,
                            output_dir) {
  if (class(mcarray_list) == "mcarray") {
    p_sd <- signif(summary(mcarray_list$p.sd, mean)$stat, 2)
    p_mean <- signif(summary(mcarray_list$p.mean, mean)$stat, 2)
  } else {
    p_sd <- round(mean(mcarray_list$p.sd), 2)
    p_mean <- round(mean(mcarray_list$p.mean), 2)
  }
  hurdle_ppcs <- if (any(grepl("p.bern", names(mcarray_list)))) {
    tibble(
      p_bern_sd = round(mean(mcarray_list$p.bern.sd), 2),
      p_bern_mean = round(mean(mcarray_list$p.bern.mean), 2)
    )
  } else {
    NULL
  }

  tibble(
    # Bayesian P value for standard deviation of observations and simulated data.
    p_sd = p_sd, # signif(summary(mcarray_list$p.sd, mean)$stat, 2),
    # Bayesian P value for mean of observations and simulated data.
    p_mean = p_mean, # signif(summary(mcarray_list$p.mean, mean)$stat, 2),
    # Posterior predictive loss.
    ppl = round(post_pred_loss),
    # Deviance information criterion.
    dic = round(DIC),
    # Gelman diagnostic.
    gelman_diag = signif(max(convergence_diagnostic$psrf[, 2]), 3)
  ) %>%
    bind_cols(hurdle_ppcs) %T>%
    # Write model convergence and performance metrics to disk.
    write_csv(file.path(output_dir, "mod-summary.csv"))
}
