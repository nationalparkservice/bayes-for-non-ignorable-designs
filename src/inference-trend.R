get_trend_inference <- function(mcarray_list, data, folder, resp_var_desc,
                                wrap_char_n = 16, ...) {
  message("Inference on trend...")

  jags_obj <- grep("^trend.park.avg.annual.change.*", names(mcarray_list), value = TRUE)

  lapply(jags_obj, function(this_jags_obj) {
    root <- file.path(
      folder, "03-inference/me",
      ifelse(grepl(".*.oos", this_jags_obj),
        "all", "sampled"
      )
    )

    park_trend_df <- tibble(
      stratum_id = "Park",
      avg_change_per_year = concat_chains(mcarray_list[[this_jags_obj]][1, , ],
        axis = 2
      )
    ) %>%
      gather(gen_quant, gen_quant_val, -stratum_id)

    stratum_trend_df <- if (n_distinct(data$stratum_id) > 1) {
      t(concat_chains(mcarray_list[[sub("park", "stratum", this_jags_obj)]],
        axis = 3
      )) %>%
        as_tibble() %>%
        gather(stratum_col, avg_change_per_year) %>%
        left_join(get_stratum_lookup(data, prepend_V = FALSE),
          by = "stratum_col"
        ) %>%
        select(-stratum_col, -stratum_index) %>%
        gather(gen_quant, gen_quant_val, -stratum_id)
    } else {
      NULL
    }

    park_and_stratum_df <- bind_rows(park_trend_df, stratum_trend_df) %>%
      mutate(stratum_id = relevel(factor(stratum_id), ref = "Park")) %>%
      select(-matches("col|index"))

    # Tabular output.
    save_table(
      park_and_stratum_df, root,
      get_filename(this_jags_obj, "csv", "posteriors")
    )

    # 95% quantiles
    park_and_stratum_trend_quantiles <- park_and_stratum_df %>%
      group_by(stratum_id, gen_quant) %>%
      summarise(
        mean = mean(gen_quant_val),
        quantile_0.25 = quantile(gen_quant_val, probs = 0.25),
        quantile_97.5 = quantile(gen_quant_val, probs = 0.975)
      )
    save_table(
      park_and_stratum_trend_quantiles, root,
      get_filename(this_jags_obj, "csv", "quantiles")
    )

    # Graphical output.
    p_labs <- park_and_stratum_df %>%
      group_by(stratum_id, gen_quant) %>%
      summarise_at(vars(gen_quant_val), list(
        median = ~ median(.),
        p_nonzero = ~ cumulative_mass(.),
        dir_nonzero = ~ names(cumulative_mass(.))
      )) %>%
      ungroup() %>%
      mutate_at(vars(median, p_nonzero), list(chr = ~ formatC(., format = "e", digits = 1))) %>%
      mutate(legend = sprintf("median: %s;\nprob %s: %s", median_chr, dir_nonzero, p_nonzero_chr))
    p <- ggplot(park_and_stratum_df, aes(x = gen_quant_val, y = ..density..)) +
      facet_grid(stratum_id ~ .,
        labeller = label_wrap_gen(wrap_char_n),
        scales = "free"
      ) +
      geom_histogram(color = "black", fill = "white", bins = 50) +
      geom_vline(
        data = p_labs, aes(xintercept = median),
        color = "steelblue", size = 0.75
      ) +
      ggthemes::theme_hc(base_size = 16) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.5) +
      geom_label(
        data = p_labs, aes(x = Inf, y = Inf, label = legend),
        vjust = "inward", hjust = "inward"
      ) +
      labs(
        y = expression("[" ~ italic(z) ~ "|" ~ italic(y) ~ "]"),
        x = as.expression(bquote(italic(z) ~ "=" ~ Delta[~ .(resp_var_desc) ~ (year^-1)]))
      ) +
      lims(x = hdi(park_and_stratum_df$gen_quant_val, credMass = 0.999))
    save_figure(
      plot = p,
      path = root,
      filename = get_filename(this_jags_obj, "jpg", "plot"),
      p_width = 3, p_height = 1,
      device = NULL
    )
  })

  NULL
}

get_zone_trend_inference <- function(mcarray_list, folder, resp_var_desc,
                                     wrap_char_n = 16, ...) {
  message("Inference on trends within zones...")

  jags_obj <- grep("^trend.zone.avg.annual.change.*", names(mcarray_list), value = TRUE)

  lapply(jags_obj, function(this_jags_obj) {
    root.zone <- file.path(
      folder, "03-inference/me",
      ifelse(grepl(".*.oos", this_jags_obj),
        "all", "sampled"
      )
    )

    zone_trend_df <-
      t(concat_chains(mcarray_list[[this_jags_obj]], axis = 3)) %>%
      as_tibble() %>%
      gather(zone_col, avg_change_per_year) %>%
      left_join(get_zone_lookup(folder), by = "zone_col") %>%
      select(-zone_col, -zone_idx) %>%
      gather(gen_quant, gen_quant_val, -zone)

    # Tabular output.
    save_table(
      zone_trend_df, root.zone,
      get_filename(this_jags_obj, "csv", "posteriors")
    )

    # 95% quantiles
    zone_trend_quantiles <- zone_trend_df %>%
      group_by(zone, gen_quant) %>%
      summarise(
        mean = mean(gen_quant_val),
        quantile_0.25 = quantile(gen_quant_val, probs = 0.25),
        quantile_97.5 = quantile(gen_quant_val, probs = 0.975)
      )
    save_table(
      zone_trend_quantiles, root.zone,
      get_filename(this_jags_obj, "csv", "quantiles")
    )

    # Graphical output.
    p_labs <- zone_trend_df %>%
      group_by(zone, gen_quant) %>%
      summarise_at(vars(gen_quant_val), list(
        median = ~ median(.),
        p_nonzero = ~ cumulative_mass(.),
        dir_nonzero = ~ names(cumulative_mass(.))
      )) %>%
      ungroup() %>%
      mutate_at(vars(median, p_nonzero), list(chr = ~ formatC(., format = "e", digits = 1))) %>%
      mutate(legend = sprintf("median: %s;\nprob %s: %s", median_chr, dir_nonzero, p_nonzero_chr))
    p <- ggplot(zone_trend_df, aes(x = gen_quant_val, y = ..density..)) +
      facet_grid(zone ~ .,
        labeller = label_wrap_gen(wrap_char_n),
        scales = "free"
      ) +
      geom_histogram(color = "black", fill = "white", bins = 50) +
      geom_vline(
        data = p_labs, aes(xintercept = median),
        color = "steelblue", size = 0.75
      ) +
      ggthemes::theme_hc(base_size = 16) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.5) +
      geom_label(
        data = p_labs, aes(x = Inf, y = Inf, label = legend),
        vjust = "inward", hjust = "inward"
      ) +
      labs(
        y = expression("[" ~ italic(z) ~ "|" ~ italic(y) ~ "]"),
        x = as.expression(bquote(italic(z) ~ "=" ~ Delta[~ .(resp_var_desc) ~ (year^-1)]))
      ) +
      lims(x = hdi(zone_trend_df$gen_quant_val, credMass = 0.999))
    save_figure(
      plot = p,
      path = root.zone,
      filename = get_filename(this_jags_obj, "jpg", "plot"),
      p_width = 3, p_height = 1.5,
      device = NULL
    )
  })

  NULL
}
