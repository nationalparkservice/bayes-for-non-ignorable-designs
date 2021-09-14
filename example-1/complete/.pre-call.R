library(tidyverse)
library(rstudioapi)
library(rjags)
library(coda)
load.module("dic")

this_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  match <- grep("--file=", args)
  if (length(match) > 0) {
    normalizePath(sub("--file=", "", args[match])) # Rscript
  } else {
    normalizePath(sys.frames()[[1]]$ofile) # 'source'd via R console
  }
}

# Set example / project paths and source required utilities.
ex_path <- ifelse(.Platform$GUI == "RStudio",
  dirname(callFun("getActiveDocumentContext")$path),
  dirname(this_file())
)
sapply(
  list.files(file.path(ex_path, "../../src"), ".*.R", full.names = TRUE),
  source
)

# Load the tabular data, just for reference.
d <- read_csv(file.path(ex_path, ".00-input", "state-variable-data.csv"))

# Load the data list required by JAGS.
jags_data <- readRDS(file.path(ex_path, ".00-input", "jags-data.rds"))
jags_data <- jags_data[-which(grepl(
  "^hat.|^pred.|^in.sample.idx$|^X.driver$|^x.pred.index$|j.pred|k.pred",
  names(jags_data)
))]
x_miss_var <- grep("Hits", names(d), value = TRUE)

# Load the variables we're watching.
jags_vars <- readRDS(file.path(ex_path, ".00-input", "jags-vars.rds"))

# Update covariates for each model.

X.raw <- jags_data$X
X.pred.raw <- jags_data$X.pred

# X.pred.raw[jags_data$j.pred == 1 & jags_data$k.pred == 1, "fe_rain.pregr"] %>% plot(type = 'l')

which_richness_covariates <- which(
  dimnames(X.raw)[[2]] %in%
    c("fe_BotanistDS", x_miss_var) # "fe_deficit.MarMay"
)
jags_data$X <- X.raw[, which_richness_covariates, drop = FALSE]
jags_data$X.pred <- X.pred.raw[, which_richness_covariates, drop = FALSE]

which_cheatgrass_covariates <- which(
  dimnames(X.raw)[[2]] %in%
    c("fe_BotanistDS", "fe_rain.pregr") # "fe_deficit.pregr"
)

jt_in_range <- rep(0, length(jags_data$x.pred))
non_missing_recs <- which(!is.na(X.pred.raw[, x_miss_var]))
jt_in_range[non_missing_recs] <- 1
jags_data$jt.in.range <- jt_in_range
# x_miss_idx <- which(!is.na(d$fe_NumberNonBromeExoticHits2))


# Missing data metadata.
x_miss_hits_name <- sub("fe_", "", x_miss_var) # "NumberNonBromeExoticHits2"
x_miss_hits_lab <- sprintf("fe_%s", x_miss_hits_name)

which_is_missing <- grep(x_miss_hits_lab, dimnames(jags_data$X.pred)[[2]])
X_miss_moments <- readRDS(file.path(ex_path, ".00-input/covariate-moments.rds"))

# Create needed JAGS object.
which_is_hits_name <- which(X_miss_moments$fe_column == x_miss_hits_name)
jags_data$x.miss.mean <- X_miss_moments %>%
  slice(which_is_hits_name) %>%
  pull(`scaled:center`)
jags_data$x.miss.sd <- X_miss_moments %>%
  slice(which_is_hits_name) %>%
  pull(`scaled:scale`)

d_x_miss <- d %>%
  mutate(x = jags_data$x, which_row = row_number()) %>%
  group_by(stratum_index, site_in_stratum_index, cal_year, !!x_miss_hits_lab, x) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(x_miss = get(x_miss_hits_lab) * jags_data$x.miss.sd + jags_data$x.miss.mean)
jags_data$W <- X.raw[d_x_miss$which_row, which_cheatgrass_covariates, drop = FALSE]

jags_data$x.miss <- d_x_miss$x_miss
jags_data$x.miss.yr <- d_x_miss$x
jags_data$x.miss.site <- d_x_miss$site_in_stratum_index
jags_data$x.miss.strata <- d_x_miss$stratum_index

jags_data$x.miss.n <- 213

jags_data$which.is.miss <- which_is_missing
jags_data$which.isnt.miss <- grep(x_miss_hits_lab, dimnames(jags_data$X.pred)[[2]], invert = TRUE)

jags_data$W.pred <- X.pred.raw[, which_cheatgrass_covariates, drop = FALSE]

jags_data$X.pred.padded <- jags_data$X.pred
jags_data$X.pred.padded[is.na(jags_data$X.pred.padded)] <- 999

# mgmt_scenario_z <- 0.05 * 213
jags_data$mgmt.scenario.z <- (0.05 * 213 - jags_data$x.miss.mean) / jags_data$x.miss.sd

# Write analysis ready JAGS object to input folder.
saveRDS(jags_data, file.path(ex_path, "00-input", "jags-data.rds"))

files_to_move <- c(
  "state-variable-data.csv", "jags-info.rds", "jags-n-iters.rds", "jags-inits.rds",
  "jags-vars.rds"
)
sapply(files_to_move, function(x) {
  # browser()
  file.copy(file.path(ex_path, ".00-input", x), file.path(ex_path, "00-input", x))
})
