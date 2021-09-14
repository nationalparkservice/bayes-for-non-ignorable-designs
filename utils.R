library(extrafont)
library(HDInterval)
library(rstudioapi)
source("src/utils-caching.R")

hdi2 <- function(x, ...) c(get_interval2(x, ...), median = median(x))
data_for_obj <- function(obj, obj_name, ..., ci_lty = "dashed",
                         include_draws = FALSE, n_draws = 1000) {
  t_incr <-
    if (grepl("hat", obj_name)) jags_data$x.hat.raw else jags_data$x.pred.raw
  to_tbl <- function(x) {
    as_tibble(t(x)) %>%
      mutate(rel_year = unique(t_incr)) %>%
      left_join(d %>% distinct(rel_year, cal_year) %>% arrange(rel_year),
        by = "rel_year"
      ) %>%
      filter(!is.na(cal_year))
  }

  out_raw <- if (grepl("strat", obj_name)) {
    post_stat <- apply(concat_chains(obj[[obj_name]], axis = 4), 1:2, hdi2, ...)
    apply(post_stat, 3, to_tbl) %>%
      bind_rows(.id = "stratum_index") %>%
      mutate(stratum_index = as.integer(stratum_index))
  } else if (grepl("park", obj_name)) {
    post_stat <- apply(concat_chains(obj[[obj_name]], axis = 3), 1, hdi2, ...)
    to_tbl(post_stat)
  }
  # browser()
  draws <- NULL
  if (include_draws) {
    if (grepl("strat", obj_name)) {
      stop("Argument include_draws supported only for park-level objects...")
    }
    n_draws_total <- dim(obj[[obj_name]])["iteration"] * dim(obj[[obj_name]])["chain"]
    which_samples <- sample(n_draws_total, n_draws)
    draws_raw <- to_tbl(t(concat_chains(obj[[obj_name]], axis = 3))[which_samples, ])
    draws <- draws_raw %>%
      pivot_longer(-any_of(c("rel_year", "cal_year")),
        names_to = "iteration", values_to = "val"
      )
  }

  out <- out_raw %>%
    pivot_longer(all_of(c("lower", "median", "upper")),
      names_to = "stat", values_to = "val"
    ) %>%
    mutate(
      lty = ifelse(stat == "median", "solid", ci_lty),
      lwd = ifelse(stat == "median", 0.9, 0.5),
      obj = obj_name
    )
  attr(out, "draws") <- draws

  if (grepl("strat", obj_name)) {
    out %>%
      left_join(d %>% distinct(stratum_id, stratum_index))
  } else if (grepl("park", obj_name)) {
    out
  }
}
coef_stats <- function(x, denom = 1, digits = NULL, transform = TRUE, ...) {
  if (transform) {
    z <- exp(as.vector(x) / denom)
  } else {
    z <- as.vector(x) / denom
  }
  out <- c(median = median(z), hdi(z, ...))
  if (!is.null(digits)) {
    return(round(out, digits))
  }
  out
}
unscale <- function(z, to, from = c(0, 1)) {
  (z - from[1]) / (diff(from)) * diff(to) + to[1]
}
load_font <- function() {
  font_import(prompt = FALSE, pattern = "lmroman*")
  loadfonts()
}
this_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  match <- grep("--file=", args)
  if (length(match) > 0) {
    normalizePath(sub("--file=", "", args[match])) # Rscript
  } else {
    normalizePath(sys.frames()[[1]]$ofile) # 'source'd via R console
  }
}
get_ex_path <- function() {
  ex_path <- ifelse(.Platform$GUI == "RStudio",
    dirname(callFun("getActiveDocumentContext")$path),
    dirname(this_file())
  )
  sapply(
    list.files(file.path(ex_path, "../../src"), ".*.R", full.names = TRUE),
    source
  )
  ex_path
}

get_output_path <- function(ex_path, base_dir = "output", dirs = NULL) { # c('03-inference', '99-misc')
  # browser()
  output_path <-
    file.path(base_dir, stringr::str_match(ex_path, "example-\\d+.*")[, 1])
  # file.path('output', stringr::str_match(ex_path, 'example-\\d+/.*')[, 1])
  message(sprintf("writing complete results to %s", output_path))
  sapply(do.call(file.path, list(c(output_path, dirs))),
    dir.create,
    showWarnings = FALSE, recursive = TRUE
  )
  if (dir.exists(file.path(ex_path, "00-input"))) {
    file.copy(file.path(ex_path, "00-input"), output_path, recursive = TRUE)
  }
  output_path
}

get_asset_path <- function(output_dir, filename, ...) {
  output_dir <- paste(c(output_dir, ...), collapse = "/")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  file.path(output_dir, filename)
}
