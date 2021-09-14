source("src/wrangle.r")

get_stratum_inference <- function(mcarray_list, data, folder, resp_var_desc,
                                  timesteps, jags_obj_prefix, likelihood,
                                  wrap_char_n = 15, x_hat_raw = timesteps,
                                  ...) {
  if (!sprintf("%s.strat.mean", jags_obj_prefix) %in% names(mcarray_list)) {
    return(NULL)
  }

  print("Inference on strata...")

  show_preds_against_data <- show_data_in_output(mcarray_list)
  # browser()
  if (likelihood == "hurdle-ordinal-latent-beta") show_preds_against_data <- FALSE
  # browser()
  # Preliminaries.

  oos_entry <- ifelse(any(grepl(
    sprintf("%s.strat.*oos$", jags_obj_prefix),
    names(mcarray_list)
  )), ".oos", NA)
  strat_mean_suffixes <- na.omit(c("", oos_entry))

  in_and_out <- lapply(strat_mean_suffixes, function(strat_mean_suffix) {
    # browser()
    sms_dash <- sub("\\.", "-", strat_mean_suffix)
    sm_sub_dir <- ifelse(grepl("oos", strat_mean_suffix), "all", "sampled")
    is_beta_binomial <- grepl("beta-binomial", likelihood)
    jags_obj_suffix <- "mean" # ifelse(is_beta_binomial, 'p', 'mean')
    stratum_means_obj <- paste0(
      paste(jags_obj_prefix, "strat", jags_obj_suffix, sep = "."), strat_mean_suffix
    )
    stratum_lookup <- get_stratum_lookup(data)

    n_strata <- dim(mcarray_list[[stratum_means_obj]])[2]
    # browser
    if (n_strata == 1) {
      stratum_mu_tmp <- concat_chains(mcarray_list[[stratum_means_obj]][, 1, , ], axis = 3)
      stratum_mu_full <- array(NA, dim = c(nrow(stratum_mu_tmp), n_strata, ncol(stratum_mu_tmp)))
      stratum_mu_full[, 1, ] <- stratum_mu_tmp
    } else {
      stratum_mu_full <- concat_chains(mcarray_list[[stratum_means_obj]], axis = 4)
    }

    # browser()
    year_vec <- data %>%
      get_year_lookup(timesteps) %>%
      pull(year)
    # browser()
    new_obs_obj <- paste(jags_obj_prefix, "strat.new.obs", sep = ".")
    # browser()
    show_preds_against_data <-
      if (new_obs_obj %in% names(mcarray_list) & !grepl("oos", strat_mean_suffix)) {
        show_preds_against_data
      } else {
        FALSE
      }
    # browser()
    if (any(show_preds_against_data)) {
      # browser()
      pred_intervals <-
        whip_y_sim_into_shape(mcarray_list, data,
          x_hat_raw = timesteps,
          obj = new_obs_obj, ...
        )
    }

    # browser()
    stratum_mu_subset <- stratum_mu_full %>% thin_mcmc_output(axis = 3, ...)
    stratum_mu_full <- stratum_mu_full %>%
      thin_mcmc_output(axis = 3, n_draws = min(dim(stratum_mu_full)[3], 1000))
    # LJZ PATCH FOR DANA HERE!

    stratum_mu_draws <- lapply(list(stratum_mu_subset, stratum_mu_full), function(zzz) {
      x_index <- 0
      lapply(seq_len(n_strata), function(x) {
        x_index <<- x_index + 1
        this_stratum_mu_subset <- if (n_strata == 1) {
          zzz
        } else {
          zzz[, x, ]
        }
        # browser()
        this_stratum_mu_subset %>%
          as_tibble(.name_repair = v_digit) %>%
          mutate(year = year_vec, stratum_index = x_index) %>%
          left_join(stratum_lookup) %>%
          gather(k, stratum_mu, -year, -matches("stratum"))
      }) %>% bind_rows()
    })

    names(stratum_mu_draws) <- c("stratum_mu_subset", "stratum_mu_full")
    # browser()
    stratum_mean_mu <- summary(mcarray_list[[stratum_means_obj]], mean)$stat %>%
      as_tibble(.name_repair = NULL) %>%
      mutate(year = year_vec) %>%
      gather(stratum_col, stratum_mean_mu, -year) %>%
      left_join(stratum_lookup)

    x_index <- 0
    # if(strat_mean_suffix == '.oos') browser()
    stratum_mean_cis <-
      apply(summary(mcarray_list[[stratum_means_obj]], quantile, c(.025, .975))$stat,
        MARGIN = 3, function(x) {
          x_index <<- x_index + 1
          # browser()
          x %>%
            as_tibble(.name_repair = NULL) %>%
            mutate(ci = c("0.025", "0.975"), stratum_index = x_index) %>%
            gather(year_col, ci_value, -ci, -stratum_index) %>%
            left_join(get_year_lookup(data, timesteps)) %>%
            left_join(stratum_lookup) %>%
            select(-matches("index|col"))
        }
      ) %>% reduce(rbind)
    # browser()

    # Save plot components for future plotting.
    dir.create(file.path(folder, "03-inference/strata", sm_sub_dir),
      showWarnings = FALSE, recursive = TRUE
    )
    list(
      stratum_mean_mu = stratum_mean_mu,
      stratum_mu_draws = stratum_mu_draws$stratum_mu_subset,
      stratum_mu_complete = stratum_mu_draws$stratum_mu_full,
      stratum_mean_cis = stratum_mean_cis
    ) %T>%
      saveRDS(file.path(
        folder, "03-inference/strata", sm_sub_dir,
        paste(jags_obj_prefix,
          paste0("stratum-means", sms_dash, ".rds"),
          sep = "-"
        )
      ))
    # if(sm_sub_dir == 'all-possible-sites') browser()
    # Save tabular output.
    temp <- stratum_mean_cis %>%
      select(-rel_year, -cal_year) %>%
      spread(ci, ci_value)
    # browser()
    stratum_mu_draws$stratum_mu_subset %>%
      # We used fractional years to get nice smooth lines for plotting. Tabular
      # outputs are probably best summarized by whole years.
      filter(year %% 1 == 0) %>%
      rename(mu = stratum_mu) %>%
      # For each year, compute the mean and standard deviation.
      group_by(year, stratum_id) %>%
      summarise_at("mu", list(~ mean(.), ~ sd(.))) %>%
      ungroup() %>%
      left_join(temp, by = c("year", "stratum_id")) %T>%
      write_csv(file.path(
        folder, "03-inference/strata", sm_sub_dir,
        paste(jags_obj_prefix,
          paste0("stratum-means", sms_dash, ".csv"),
          sep = "-"
        )
      ))

    fncols <- function(data, cname) {
      # https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
      add <- cname[!cname %in% names(data)]

      if (length(add) != 0) data[add] <- 1
      data
    }
    # browser()
    stratum_data <- data %>%
      select(
        year = cal_year, stratum_id, outcome = matches("^response$|^hits$"),
        normalizer = matches("^trials$", ignore.case = FALSE)
      ) %>%
      fncols("normalizer") %>%
      mutate(outcome = outcome / normalizer)

    # Plotting.
    get_y_lab <- function(showing_y_new, diff = FALSE) {
      y_lev <- "Stratum-level"
      if (diff) y_lev <- sprintf("Difference in %s", tolower(y_lev))
      if (showing_y_new) {
        as.expression(
          bquote(atop(.(y_lev), .(tolower(resp_var_desc)) ~
          (mu * "," ~ italic(y)^new)))
        )
      } else {
        as.expression(
          bquote(atop(.(y_lev), .(tolower(resp_var_desc)) ~ (mu)))
        )
      }
    }

    if (n_strata > 1) {
      d_contrasts <- stratum_mu_draws$stratum_mu_full
      d_contrasts_wide <- d_contrasts %>%
        select(-stratum_index, -stratum_col) %>%
        spread(stratum_id, stratum_mu)

      contrasts <- combn(unique(d_contrasts$stratum_id), 2)
      # browser()
      contrast_results <- lapply(1:ncol(contrasts), function(this_contrast) {
        this_diff <- paste(paste0("`", contrasts[, this_contrast], "`"), collapse = " - ")
        diff_label <- paste0("`", paste(contrasts[, this_contrast], collapse = "_minus_"), "`") # gsub(' ', '_', x)
        # browser()
        eval(parse(text = sprintf(
          "select(mutate(d_contrasts_wide, %s = %s), year, k, %s)",
          diff_label, this_diff, diff_label
        )))
      }) %>%
        reduce(left_join) %>%
        gather(contrast, diff, -year, -k)
      saveRDS(contrast_results, file.path(
        folder, "03-inference/strata", sm_sub_dir,
        paste(jags_obj_prefix,
          paste0("stratum-mean-contrasts", sms_dash, ".rds"),
          sep = "-"
        )
      ))


      ggplot(
        contrast_results %>%
          filter(year %% 1 == 0) %>%
          mutate(contrast = sub("_minus_", " - ", contrast)), #
        aes(x = diff, y = year, group = year)
      ) +
        facet_wrap(~contrast) +
        geom_density_ridges(fill = "steelblue", alpha = 0.8) +
        geom_vline(xintercept = 0, color = "red") +
        scale_y_reverse() +
        labs(y = "Year", x = get_y_lab(FALSE, diff = TRUE)) +
        ggthemes::theme_hc(base_size = 16)
      ggsave(file.path(
        folder, "03-inference/strata", sm_sub_dir,
        paste0(
          jags_obj_prefix, "-stratum-mean-contrasts",
          sms_dash, ".png"
        )
      ),
      width = n_distinct(data$stratum_id) * 4, height = 6
      )
    }

    lapply(show_preds_against_data, function(with_data) {
      # browser()
      ggplot() +
        facet_grid(. ~ stratum_id, labeller = label_wrap_gen(wrap_char_n)) +
        # facet_wrap(~stratum_id, labeller = label_wrap_gen(wrap_char_n)) +
        # Add prediction intervals.
        {
          if (with_data & strat_mean_suffix != ".oos") {
            geom_line(
              data = pred_intervals, aes(x = year, y = quantile, group = quantile_lab),
              color = "black", alpha = .5, linetype = "twodash"
            )
          } else {
            NULL
          }
        } +
        geom_line(
          data = stratum_mu_draws$stratum_mu_subset, aes(x = year, y = stratum_mu, group = k),
          color = "gray", alpha = .1
        ) +
        geom_line(
          data = stratum_mean_mu, aes(x = year, y = stratum_mean_mu),
          color = "black", size = 1.01
        ) +
        geom_line(
          data = stratum_mean_cis, aes(x = year, y = ci_value, group = ci),
          color = "black", linetype = "twodash", size = 1.01
        ) +
        # Add layer with actual data.
        {
          if (with_data) {
            geom_point(
              data = stratum_data, aes(x = year, y = outcome),
              color = "black", alpha = .25,
              position = position_jitter(
                width = diff(range(stratum_data$year)) * .01,
                height = diff(range(stratum_data$outcome, na.rm = TRUE)) * .01
              )
            ) # width = 0.1, height = 0.1
          } else {
            NULL
          }
        } +
        labs(x = "Year", y = get_y_lab(with_data)) +
        scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggthemes::theme_hc(base_size = 16)
      # if(with_data) browser()
      # png_dims <- fig_dims('y', ratio = 4/5,
      #                      n_facet_x = 1,
      #                      n_facet_y = n_distinct(data$stratum_id),
      #                      in_per_y_facet = 2.5)
      ggsave(file.path(
        folder, "03-inference/strata", sm_sub_dir,
        paste0(
          jags_obj_prefix, "-stratum-means",
          ifelse(with_data, "-with-data", ""),
          sms_dash, ".png"
        )
      ),
      width = n_distinct(data$stratum_id) * 4, height = 5
      )
    })

    strat_new_obs_obj <- paste(jags_obj_prefix, "strat.new.obs", sep = ".")
    obj_year_lookup <- get_year_lookup(data, timesteps)
    # browser()
    if (!is.null(mcarray_list[[strat_new_obs_obj]]) & strat_mean_suffix != ".oos") {

      # strat_new_obs <- concat_chains(mcarray_list[[strat_new_obs_obj]], axis=4)

      if (n_strata == 1) {
        strat_new_obs_tmp <- concat_chains(mcarray_list[[strat_new_obs_obj]][, 1, , ], axis = 3)
        strat_new_obs <- array(NA, dim = c(nrow(strat_new_obs_tmp), n_strata, ncol(strat_new_obs_tmp)))
        strat_new_obs[, 1, ] <- strat_new_obs_tmp
      } else {
        strat_new_obs <- concat_chains(mcarray_list[[strat_new_obs_obj]], axis = 4)
      }
      # browser()

      new_obs_df <- lapply(seq_len(dim(strat_new_obs)[2]), function(x) {
        stratum_new_obs <- strat_new_obs[, x, ]

        new_obs_bcis <- apply(stratum_new_obs,
          MARGIN = 1, FUN = quantile,
          probs = c(.025, .975), na.rm = TRUE
        )
        new_obs_bcis %>%
          as_tibble(.name_repair = NULL) %>%
          mutate(stat = c("lwr", "upr")) %>%
          gather(year_col, stat_val, -stat) %>%
          left_join(obj_year_lookup) %>%
          mutate(stratum_col = paste0("V", x))
      }) %>%
        bind_rows() %>%
        left_join(get_stratum_lookup(data))
      # browser()
      ggplot() +
        facet_grid(. ~ stratum_id, labeller = label_wrap_gen(wrap_char_n)) + # , scales = 'free'
        # facet_wrap(~stratum_id, labeller = label_wrap_gen(wrap_char_n)) +
        geom_line(
          data = new_obs_df, aes(x = year, y = stat_val, group = stat),
          linetype = "twodash", alpha = .5
        ) +
        geom_line(
          data = stratum_mean_mu, aes(x = year, y = stratum_mean_mu),
          color = "black", size = 1.01
        ) +
        geom_line(
          data = stratum_mean_cis, aes(x = year, y = ci_value, group = ci),
          color = "black", linetype = "twodash", size = 1.01
        ) +
        labs(x = "Year", y = get_y_lab(TRUE)) +
        scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggthemes::theme_hc(base_size = 16)


      ggsave(file.path(
        folder, "03-inference/strata", sm_sub_dir,
        paste0(
          jags_obj_prefix, "-stratum-new-obs",
          sms_dash, ".png"
        )
      ),
      width = n_distinct(data$stratum_id) * 4, height = 5
      )
      # stratum_mean_mu
    }
    list(
      stratum_mean_mu = stratum_mean_mu %>% mutate(type = strat_mean_suffix),
      stratum_mean_cis = stratum_mean_cis %>% mutate(type = strat_mean_suffix)
    )
  })
  in_and_out_mu <- bind_rows(lapply(in_and_out, function(x) x[["stratum_mean_mu"]]))
  in_and_out_cis <- bind_rows(lapply(in_and_out, function(x) x[["stratum_mean_cis"]]))
  ggplot(in_and_out_cis) +
    facet_grid(. ~ stratum_id, labeller = label_wrap_gen(wrap_char_n)) + # , scales = 'free'
    # facet_wrap(~stratum_id, labeller = label_wrap_gen(wrap_char_n)) +
    geom_line(
      data = in_and_out_mu, aes(x = year, y = stratum_mean_mu, color = type),
      size = 1.01
    ) +
    geom_line(
      data = in_and_out_cis, aes(
        x = year, y = ci_value, group = interaction(ci, type),
        color = type
      ),
      linetype = "twodash"
    ) +
    labs(x = "Year", y = "Sampled- vs all possible-sites stratum means") +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_color_manual("Type", labels = c("Sampled sites", "All possible sites"), values = c("red", "black")) +
    ggthemes::theme_hc(base_size = 16)
  ggsave(file.path(
    folder, "03-inference/strata",
    paste0(jags_obj_prefix, "-stratum-means-in-vs-out", ".png")
  ),
  width = n_distinct(data$stratum_id) * 4, height = 5
  )
  # browser()
}
