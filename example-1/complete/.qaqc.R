# Plots of the probability of hitting an exotic spp.

which_stratum <- sample(dim(z_jags$pred.strat.p)[2], 1)
which_iter <- sample(dim(z_jags$pred.strat.p)["iteration"], 1)
which_chain <- sample(dim(z_jags$pred.strat.p)["chain"], 1)

this_pred_strat_p <- z_jags$pred.strat.p[, which_stratum, which_iter, which_chain]
this_j <- z_jags$j.hat.draw[which_stratum, which_iter, which_chain]
dev.off()
par(mfrow = c(2, 1))
plot(this_pred_strat_p, type = "l", main = sprintf("For j.draw %s in stratum %s", this_j, which_stratum))


this_sigma_tilde <- sqrt(1 / z_jags$tau.tilde[, which_stratum, which_iter, which_chain])
this_eps_tilde <- z_jags$eps.tilde[, which_stratum, which_iter, which_chain]
plot(this_eps_tilde,
  type = "l",
  main = sprintf("eps ~ N(0, %s)", round(this_sigma_tilde[1], 2))
)
abline(h = 0, lty = 2)


# Plot of rainfall over time at each site.
ggplot(d) +
  facet_wrap(~stratum_index) +
  geom_point(aes(x = rel_year, y = fe_rain.pregr, group = site_index, color = site_in_stratum_index)) +
  geom_line(
    data = tibble(
      fe_rain.pregr = jags_data$W.pred[, 2],
      site_index = jags_data$j.pred, # Must be present in the jags_data obj
      stratum_index = jags_data$k.pred,
      rel_year = jags_data$x.pred.raw
    ),
    aes(x = rel_year, y = fe_rain.pregr, group = site_index, color = site_index)
  )

# Old code used to plot stratum-level cover.
ggplot(data_for_obj(z_jags, "pred.strat.p", interval = 0.8, method = "eti")) +
  facet_wrap(~stratum_id) +
  geom_hline(yintercept = 0.05, color = "orange", linetype = "dashed") +
  geom_line(aes(
    x = cal_year, y = val, group = interaction(stat, obj),
    linetype = lty
  )) +
  scale_linetype_identity() +
  theme_ipsum_rc(
    grid = "Y", base_family = font_an, base_size = 14,
    axis_title_size = 16, strip_text_size = 16
  ) +
  labs(x = "Year", y = expression(Mean ~ cheatgrass ~ cover %*% site^-1)) +
  scale_x_continuous(breaks = scales::pretty_breaks())
