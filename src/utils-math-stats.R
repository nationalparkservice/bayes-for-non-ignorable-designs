library(HDInterval)

get_interval <- function(x, interval, method = c("hdi", "eti")) {
  if (method[1] == "hdi") {
    hdi(x, credMass = interval)
  } else if (method[1] == "eti") {
    quantile(x, probs = c(0 + (1 - interval) / 2, 1 - (1 - interval) / 2))
    # summary(x, quantile,
    #         probs = c(0 + (1 - interval) / 2, 1 - (1 - interval) / 2))['stat']
  } else {
    stop("Illegal interval method supplied!")
  }
}

get_interval2 <- function(x, interval = 0.89, method = c("hdi", "eti")) {
  if (method[1] == "hdi") {
    out <- hdi(x, credMass = interval)
  } else if (method[1] == "eti") {
    out <- quantile(x, probs = c(0 + (1 - interval) / 2, 1 - (1 - interval) / 2))
  } else {
    stop("Illegal interval method supplied!")
  }
  names(out) <- c("lower", "upper")
  out
}

cumulative_mass <- function(x, crit_val = 0) {
  len_x_gt_crit_val <- sum(x > crit_val)
  prop_x_gt_crit_val <- len_x_gt_crit_val / length(x)
  if (len_x_gt_crit_val > length(x) / 2) {
    mass_prop <- prop_x_gt_crit_val
    dir <- "gt"
  } else {
    mass_prop <- 1 - prop_x_gt_crit_val
    dir <- "lt"
  }
  names(mass_prop) <- paste(dir, crit_val, sep = "_")
  mass_prop
}

logit <- function(p) {
  log(p / (1 - p))
}

inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

rad <- function(degrees) {
  degrees * (pi / 180)
}
