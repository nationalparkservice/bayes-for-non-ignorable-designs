get_stratum_lookup <- function(data, prepend_V = TRUE) {
  stratum_col_prefix <- if (prepend_V) "V" else NULL
  data %>%
    select(stratum_id, stratum_index) %>%
    distinct() %>%
    arrange(stratum_index) %>%
    mutate(stratum_col = paste0(stratum_col_prefix, 1:n()))
}

get_site_lookup <- function(data) {
  data %>%
    select(site_id, site_in_stratum_index, stratum_id) %>%
    distinct()
}

get_year_lookup <- function(data, timesteps, pref_col = "cal_year") {
  tibble(rel_year = timesteps) %>%
    mutate(
      cal_year = data %>% pull(cal_year) %>% min() + rel_year,
      year = get(pref_col),
      year_col = paste0("V", 1:n())
    )
}

get_zone_lookup <- function(folder) {
  tibble(zone = names(readRDS(file.path(folder, "00-input/deflections-indices.rds"))[[1]])) %>%
    mutate(zone_idx = 1:n(), zone_col = as.character(zone_idx))
}
