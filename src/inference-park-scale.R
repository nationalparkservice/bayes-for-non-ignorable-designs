# This functions acts on z_jags to develop park-level inference.

get_park_scale_inference <- function(mcarray_list, data, data_list, folder,
                                     resp_var_desc, interval = 0.95,
                                     method = c("hdi", "eti"), ...) {
  message("Inference at the park level...")

  jags_obj <- grep("^.*.park.mean$", names(mcarray_list), value = TRUE)
  mi_desc <- paste0(method, interval * 100)[1]

  lapply(jags_obj, function(this_jags_obj) {
    root <- file.path(
      folder, "03-inference/park",
      ifelse(grepl(".*.oos", this_jags_obj), # TODO: adjust for finite pop?
        "all", "sampled"
      )
    )

    timesteps_obj <- ifelse(grepl("hat", this_jags_obj),
      "x.hat.raw", "x.pred.raw"
    )
    year_vec <- data %>%
      get_year_lookup(unique(data_list[[timesteps_obj]])) %>%
      pull(year)

    # The mean (evaluated at each time step in `year_vec`) for each of `n_draws`
    # iterations.
    draw_mean <- concat_chains(mcarray_list[[this_jags_obj]], axis = 3) %>%
      thin_mcmc_output(axis = 2, ...) %>%
      as_tibble() %>%
      mutate(year = year_vec) %>%
      gather(draw, mean, -year)

    # The credible intervals for the mean at each time step (evaluated across
    # all MCMC iterations).
    disp_mean <- as_tibble(t(
      get_interval(mcarray_list[[this_jags_obj]],
        interval = interval,
        method = method[1]
      )
    )) %>%
      mutate(year = year_vec) %>%
      gather(stat_id, stat_val, -year)
    # The median of the means at each time step.
    cent_tend_mean <- tibble(
      stat_val = summary(mcarray_list[[this_jags_obj]], mean)$stat,
      year = year_vec, stat_id = "median"
    )
    # The combined statistics.
    stats_mean <- bind_rows(disp_mean, cent_tend_mean) %>%
      mutate(stat_lty = ifelse(stat_id == "median", "solid", "dashed"))

    # Graphical output.
    p <- ggplot() +
      geom_line(
        data = draw_mean, aes(x = year, y = mean, group = draw),
        color = "black",
        alpha = get_draw_alpha(n_distinct(draw_mean$draw))
      ) +
      geom_line(
        data = stats_mean, aes(
          x = year, y = stat_val, group = stat_id,
          linetype = stat_lty
        ),
        color = "black", size = 1.01
      ) +
      scale_linetype_identity() +
      labs(x = "Year", y = paste("Mean", tolower(resp_var_desc))) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggthemes::theme_hc(base_size = 16)
    save_figure(
      plot = p,
      path = root,
      filename = get_filename(
        this_jags_obj, "jpg", "plot",
        mi_desc
      ),
      p_width = n_distinct(data$cal_year) / 1.75, p_height = 4,
      device = NULL
    )

    # Save plot components for future plotting.
    # browser()
    # saveRDS(mcarray_list[['trend.park.avg.annual.change']], file.path(root, 'trend-park-avg-annual-change.rds'))
    saveRDS(
      list(draw_mean = draw_mean, stats_mean = stats_mean),
      file.path(root, get_filename(
        this_jags_obj, "rds", "plot-objects",
        mi_desc
      ))
    )

    # Tabular output.
    stats_tbl <- stats_mean %>%
      select(-stat_lty) %>%
      spread(stat_id, stat_val) %>%
      filter(year %% 1 == 0)
    write_csv(
      stats_tbl,
      file.path(root, get_filename(
        this_jags_obj, "csv", "annual-summary",
        mi_desc
      ))
    )
  })

  NULL
}
