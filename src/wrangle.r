library(tidyverse)
library(magrittr)
library(stringr)
library(readxl)
library(sf)
library(tools)
source("src/utils-math-stats.R")


perc_to_prop <- function(x) x / 100

comma_concat <- function(x) paste(x, collapse = ", ")

bar_concat <- function(x, ...) paste0(x, ..., collapse = "|")

list_join <- function(...) {
  df1 <- list(...)[[1]]
  df2 <- list(...)[[2]]
  left_join(...)
}

per_group_tally <- function(data, group) {
  data %>%
    group_by_(.dots = group) %>%
    tally() %>%
    spread_(group, "n")
}

expand_grid <- function(...) {
  expand.grid(..., stringsAsFactors = FALSE)
}

adf <- function(data, n = 6) {
  # Returns the first part of an object.
  #
  # Args:
  #   data:   An object.
  #   n:      A single integer. E.g., the number of records to return.
  #
  # Returns:
  #   The first n entries from the object.

  is_inf <- is.infinite(n)
  n <- ifelse(is_inf, nrow(data), n)
  if (!is_inf) message("Printing the first 6 rows...")
  data %>%
    slice(1:n) %>%
    as.data.frame()
}

pipe_assign <- function(data, x, value, ...) {
  position <- list(...)$pos
  assign(x, value, pos = ifelse(is.null(position), 1, position))
}

read_ee_tsv <- function(file) {
  read_tsv(file) %>%
    select(-`system:index`, -.geo) %>%
    distinct()
}

load_data <- function(file, type, ...) {
  switch(file_ext(file),
    csv = as_tibble(read.csv(file, ...)), # read_csv
    xls = read_excel(file, ...),
    xlsx = read_excel(file, ...),
    gz = read_tsv(file, ...),
    shp = st_read(file, ...),
    rds = read_rds(file) %>%
      modify_if(is.factor, as.character) %>%
      as_tibble(.name_repair = "minimal") %>%
      ungroup()
  ) %>%
    # Remove duplicates.
    distinct()
}

save_data <- function(data, file, ...) {
  dir.create(dirname(file), showWarnings = FALSE)
  switch(file_ext(file),
    csv = write_csv(data, file, ...),
    gz = write_tsv(data, file, ...)
  )
}

names_to_title <- function(data) {
  data %>%
    set_colnames(value = str_to_title(
      str_replace(colnames(.), "_", " ")
    ))
}

build_hist_data <- function(data, var, rel_freq = TRUE, bins = NULL) {
  # Used to correct ggplot2's poor assumptions related to breaks.
  var_vec <- data %>% .[[var]]
  bar_width <- 1 # 100% (used to convey we're showing a continuous outcome)
  if (is.null(bins)) {
    breaks <- hist(var_vec, plot = FALSE)$breaks
  } else {
    breaks <- seq(min(var_vec), max(var_vec), length.out = bins + 1)
  }
  if (is.integer(var_vec)) {
    breaks <- seq(min(var_vec), max(var_vec))
    bar_width <- .85 # 90% (used to convey we're showing a discrete outcome)
  }
  if (rel_freq) {
    p <- ggplot(data, aes_string(x = var)) +
      geom_histogram(aes(y = ..density..), breaks = breaks)
  } else {
    p <- ggplot(data, aes_string(x = var)) +
      geom_histogram(breaks = breaks)
  }
  ggplot_build(p)$data[[1]] %>% mutate(width = (xmax - xmin) * bar_width)
}

ee_export_file_basenames <- function(sampling_unit, park,
                                     cov_type = c("static", "dynamic")) {
  paste(cov_type, "covariates", sampling_unit, tolower(park), sep = "-")
}

transform_terrain_vars <- function(data) {
  data %>%
    mutate(northness = cos(rad(aspect)), eastness = sin(rad(aspect))) %>%
    select(-hillshade, -aspect)
}

get_ee_covariate_data <- function(sampling_unit, park, core_grouping_vars) {
  # Obtain a list of Earth Engine export files (one element per covariate type:
  # static or dynamic).
  files_by_type <- ee_export_file_basenames(sampling_unit, park) %>%
    map(function(x) list.files("output", pattern = x, full.names = TRUE))
  # Map across the files associated with each covariate type, and prepare the
  # data by taking group-wise means.
  covariate_data_by_type <- files_by_type %>%
    map(function(x, group = core_grouping_vars) {
      if (grepl("dynamic", x[1])) group <- c(group, "year")
      x %>%
        map_df(read_csv) %>%
        select(-`system:index`, -.geo) %>%
        group_by_(.dots = group) %>%
        summarise_all(list(~mean)) %>%
        ungroup()
    })
  # Join static and dynamic covariates.
  covariate_data_by_type %>%
    Reduce(list_join, .) %>%
    transform_terrain_vars()
}

sample_dists <- function(data, group_vars, loc_vars) {
  euc_dists <- data %>%
    select_(.dots = c(group_vars, loc_vars)) %>%
    distinct() %T>%
    pipe_assign("lookup", select_(., .dots = group_vars)) %>%
    select_(.dots = loc_vars) %>%
    dist(diag = TRUE, upper = TRUE) %>%
    as.matrix()
  list(D = euc_dists, key = lookup %>% mutate(sample_id = 1:n()))
}

add_regex_boundaries <- function(x) paste0("^", x, "$")

bin_any <- function(x, as_numeric = TRUE) {
  # A 'binomial' any.
  x[is.na(x)] <- 0
  suppressWarnings(if (as_numeric) as.numeric(any(x)) else any(x))
}

get_mod_summary_files <- function(folder) {
  # browser()
  jags_info_files <-
    list.files(folder,
      pattern = "jags-info.rds", recursive = TRUE,
      full.names = TRUE
    )
  jags_info_df <- jags_info_files %>%
    map_df(readRDS) %>%
    mutate(full_path = sub("/00-input", "", dirname(jags_info_files)))

  mod_summary_list <- jags_info_df %>%
    pull(full_path) %>%
    map(function(x) list.files(x, pattern = "mod-summary.csv", full.names = TRUE)) %>%
    map(function(x) if (length(x) > 0) read_csv(x) else NA)
  if (all(is.na(mod_summary_list))) {
    mod_summary_list <-
      rep(
        list(tibble(p_sd = NA, p_mean = NA, ppl = NA, dic = NA, gelman_diag = NA)),
        length(mod_summary_list)
      )
  }

  bind_cols(as_tibble(do.call(rbind, mod_summary_list),
    .name_repair = "minimal"
  ), jags_info_df) %>%
    select(-all_vars, -jags_file, -scenario, -shell_command) %>% # -full_path,
    arrange(dic) %T>%
    write_csv(file.path(folder, "mod-summaries.csv"))
}

get_stuff_in_parens <- function(x) {
  str_replace_all(str_extract(x, "\\(.*?\\)"), "[\\(\\)]", "")
}

v_digit <- function(names) {
  names <- as.character(names)
  paste0("V", seq_along(names))
}
