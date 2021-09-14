library(tidyverse)
library(rjags)
library(parallel)
library(dclone)
library(doParallel)
library(MCMCvis)

simulate.data <- function(N = 1000,
                          n = 100,
                          n_years = 5,
                          mu.B0 = 2.75,
                          sigma.B0 = 0.05,
                          B1 = 0.1, # 1.5,
                          B2 = -0.5, # 0.5,
                          A0 = 0.6,
                          A1 = -0.25,
                          seed = NULL) {

  # ---- simulation / sampling design ----
  # N: total number of sites (size of the population)
  # n: number of sites sampled (sample size)
  # n_years: number of years

  # ---- parameters used to model the mean of the response variable ----
  # mu.B0: mean of site-level intercepts (hyper distribution)
  # sigma.B0: standard deviation of site-level intercepts (hyper distribution)
  # B1: time slope
  # B2: distance slope

  # ---- parameters associated with the model for inclusion of samples ----
  # A0: intercept in model of probability of inclusion
  # A1: slope in model of probability of inclusion

  set.seed(seed)

  # make site numbers
  sites <- seq.int(N)

  # get distribution of distances
  log_d <- rnorm(N, mean = 1, sd = 0.75) # hist(log_d)
  d <- exp(log_d) # hist(d, breaks = 50)

  # create categorical inclusion probabilities
  q.d <- c(0.5, 1.0, 1.5, 2.0, Inf) # distance breaks; was: quantile(d, c(0.20, 0.40, 0.60, 0.80))
  unorm.probs <- c( # unnormalized inclusion probabilities
    `[0,0.5)` = 1, `[0.5,1)` = 0.9, `[1,1.5)` = 0.8, `[1.5,2)` = 0.5,
    `[2,Inf)` = 0.2
  )
  norm.probs <- unorm.probs / sum(unorm.probs)

  cat_p <- numeric(N)
  for (i in 1:N) {
    if (d[i] < q.d[1]) cat_p[i] <- unorm.probs[1]
    if (d[i] >= q.d[1] & d[i] < q.d[2]) cat_p[i] <- unorm.probs[2]
    if (d[i] >= q.d[2] & d[i] < q.d[3]) cat_p[i] <- unorm.probs[3]
    if (d[i] >= q.d[3] & d[i] < q.d[4]) cat_p[i] <- unorm.probs[4]
    if (d[i] >= q.d[4]) cat_p[i] <- unorm.probs[5]
  } # hist(cat_p); plot(d, cat_p)
  # table(cat_p)
  # cat_p_alt <- cut(d, breaks = c(0, q.d), labels = unorm.probs,
  #                  include.lowest = FALSE, right = FALSE)
  # table(cat_p_alt)
  # all(cat_p == cat_p_alt)

  # normalize to sum to 1
  norm_cat_p <- cat_p / sum(cat_p) # plot(d, norm_cat_p)

  # site_idx <- sample(N, size = N * 100, prob = norm_cat_p, replace = TRUE)
  # d_verify <- cut(d[site_idx], breaks = c(0, q.d), #labels = unorm.probs,
  #     include.lowest = FALSE, right = FALSE)
  # (table(d_verify) / length(d_verify))
  # # Expected number of sites drawn...
  # n1000 <- table(invert(unorm.probs)[as.character(cat_p)]) * norm.probs
  # n1000 / sum(n1000)

  # inclusion probabilities (continuous case)
  p <- boot::inv.logit(A0 + A1 * d)
  hist(p)
  plot(d, p)
  # normalize to sum to one
  norm_cont_p <- p / sum(p)
  hist(norm_cont_p) # note that when a1 = 0, p = 1/N


  # simulate response
  x <- seq(1, n_years) # sequential years
  y <- as.data.frame(matrix(nrow = N * length(x), ncol = 3))
  names(y) <- c("site", "x", "response")
  count <- 0
  for (i in 1:N) {
    B0 <- rnorm(1, mu.B0, sigma.B0)
    for (j in 1:length(x)) {
      count <- count + 1
      lambda <- exp(B0 + B1 * x[j] + B2 * d[i])
      y$response[count] <- rpois(1, lambda)
      y$site[count] <- i
      y$x[count] <- j
    }
  }
  # browser()
  plot(y$x, y$response)
  # ggplot(y, aes(x = x, y = response)) + geom_jitter(alpha = 0.2)
  # simulate inclusion vectors
  # continuous case
  # browser()
  y.sampled <- sample(N, prob = norm_cont_p, size = n)
  Q <- numeric(N)
  Q[y.sampled] <- 1
  # categorical case
  y.sampled.cat <- sample(N, prob = norm_cat_p, size = n)
  Q.cat <- numeric(N)
  Q.cat[y.sampled.cat] <- 1

  # Complete data
  W <- as.data.frame(cbind(sites, d, Q, Q.cat, norm_cont_p, norm_cat_p))
  names(W) <- c("site", "distance", "Q", "Q_cat", "norm_cont_p", "norm_cat_p")
  out <- left_join(y, W, by = "site") %>% as_tibble()
  attr(out, "y_true_mean") <- mean(out$response)
  attr(out, "y_true_sd") <- sd(out$response)
  out
  # browser()
  # y.complete <- merge(W, y, by = "site")
  # return(y.complete)
}


## Complete data model--includes all coefficients used to generate data
sink("example-2/sim/complete_data_model.jags")
cat("
    model{
    mu.B0 ~ dnorm(0, 0.00001)
    sigma.B0 ~ dunif(0, 10)
    tau.B0 <- 1 / sigma.B0^2
    beta1 ~ dnorm(0, 0.000001)
    beta2 ~ dnorm(0, 0.000001)

    for(j in 1:length(site.index)) {  # mixing subscripts
       beta0[j] ~ dnorm(mu.B0, tau.B0)
       beta0_unscaled[j] <- beta0[j] - (beta1 * mean.x/sd.x + beta2 * mean.d/sd.d)
    }
    for(i in 1:length(y)){
      lambda[i] <- min(1000, exp(beta0[site.index[i]] + beta1*x[i] + beta2*d[i]))
      y[i] ~ dpois(lambda[i])
      #y.rep[i] ~ dpois(lambda[i])
    }
    mu.beta0.unscaled = mean(beta0_unscaled)
    beta1.unscaled = beta1 / sd.x
    beta2.unscaled = beta2 / sd.d
    
    #y.rep.mean <- mean(y.rep)
    #y.rep.sd <- mean(y.rep)
  }
    ", fill = TRUE)
sink()

### Make complete dataset
y.complete <- simulate.data(N = 1000, n = 1000, seed = 101)
# attr(y.complete, 'y_true_mean'); attr(y.complete, 'y_true_sd')

nrow(y.complete)
plot(y.complete$x, y.complete$response)
plot(y.complete$distance, y.complete$response)
plot(y.complete$distance, y.complete$norm_cont_p)
plot(y.complete$distance, y.complete$norm_cat_p)


# remove comment when finished testing
y <- y.complete
# delete when finished testing
# y = y.complete[y.complete$Q == 1,]
plot(y$x, y$response)
# make sequential site index
y$site.index <- as.numeric(as.factor(as.character(y$site)))

params <- c(mu.B0 = "mu.B0", sigma.B0 = "sigma.B0", beta1 = "B1", beta2 = "B2")
true_vals <- lapply(formals(simulate.data)[params], function(x) {
  as.numeric(paste(as.character(x), collapse = ""))
})
base_init <- true_vals
names(base_init) <- names(params)
# base_init <- list(
#   mu.B0 = 1.5, sigma.B0 = 0.005, beta0 = rep(1.5, length(y$site.index)),
#   beta1 = 0.08, beta2 = 0.05
# )
make_init <- function(multiplier, base = base_init) {
  inits <- lapply(base, FUN = function(x) x * multiplier)
  inits$.RNG.name <- "base::Mersenne-Twister"
  inits$.RNG.seed <- round(runif(1, 1, 200))
  inits
}

inits <- lapply(runif(6, 0.5, 1.5), make_init)

jags_data <- list(
  y = y$response,
  x = as.vector(scale(y$x)),
  site.index = y$site.index,
  d = as.vector(scale(y$distance)),
  mean.x = mean(y$x),
  sd.x = sd(y$x),
  mean.d = mean(y$distance),
  sd.d = sd(y$distance)
)


n.adapt <- 500
n.update <- 1000
n.iter <- 5000

jags_model_file <- "example-2/sim/complete_data_model.jags"
variables <- c("mu.beta0.unscaled", "beta1.unscaled", "beta2.unscaled") # "y.rep.mean", "y.rep.sd"
cl <- makeCluster(length(inits))
registerDoParallel(cl)
start.time <- Sys.time()
out.N.complete <- jags.parfit(cl, model = jags_model_file, params = variables, data = jags_data, inits = inits, n.chains = length(inits), n.update = n.update, n.iter = n.iter, n.thin = 1)
stopCluster(cl)
end.time <- Sys.time()
run.time <- end.time - start.time
run.time
MCMCsummary(out.N.complete)
unlist(true_vals)

# jm.complete <- jags.model("complete_data_model.jags", data = jags_data, inits = inits, n.chains = length(inits))
# update(jm.complete, n.iter = 1000)
# zc.complete <- coda.samples(jm.complete, variable.names = c("mu.B0", "beta1", "beta2", "mu.beta0.unscaled"), n.iter = 5000)
# MCMCsummary(zc.complete)


## Model ignoring the missing data mechanism ########
sink("example-2/sim/completely_random.jags")
cat("
    model{
    mu.B0 ~ dnorm(0, 0.00001)
    sigma.B0 ~ dunif(0, 5)
    tau.B0 <- 1 / sigma.B0^2
    beta1 ~ dnorm(0, 0.000001)
    for(j in 1:length(site.index)) {
      beta0[j] ~ dnorm(mu.B0, tau.B0)
      beta0_unscaled[j] = beta0[j] - beta1 * mean.x/sd.x
    }
    for(i in 1:length(y)) {
      lambda[i] <- min(1000, exp(beta0[site.index[i]] + beta1*x[i]))
      y[i] ~ dpois(lambda[i])
    }
    mu.beta0.unscaled = mean(beta0_unscaled)
    beta1.unscaled = beta1 / sd.x
    }
    ", fill = TRUE)
sink()
# subset the data to 100 sites sampled
y.sample <- simulate.data(
  N = 1000, n = 100 # ,
  # B1 = 0.08, B2 = 0.05, mu.B0 = 1.5, sigma.B0 = 0.05, n_years = 8
)
y <- y.sample[y.sample$Q == 1, ]
plot(y$x, y$response)
# make sequential site index
y$site.index <- as.numeric(as.factor(as.character(y$site)))

# base_init <- list(mu.B0 = 1.5, sigma.B0 = 0.005, beta0 = rep(1.5, length(y$site.index)), beta1 = 0.08, beta2 = 0.05)

# inits <- lapply(runif(6, 0.5, 1.5), make_init)
# inits <- list(
#   make_init(1.2),
#   make_init(1),
#   make_init(0.75),
#   make_init(0.5),
#   make_init(1.1),
#   make_init(1.3)
# )

jags_data <- list(y = y$response, x = as.vector(scale(y$x)), site.index = y$site.index, d = as.vector(scale(y$distance)), mean.x = mean(y$x), sd.x = sd(y$x))

n.adapt <- 500
n.update <- 1000
n.iter <- 5000

jags_model_file <- "example-2/sim/completely_random.jags"
# variables <- c("beta1", "beta2", "mu.beta0.unscaled", "beta1.unscaled", "beta2.unscaled")
cl <- makeCluster(length(inits))
registerDoParallel(cl)
start.time <- Sys.time()
out.n.ignoring.mdm <- jags.parfit(cl, model = jags_model_file, params = variables, data = jags_data, inits = inits, n.chains = length(inits), n.update = n.update, n.iter = n.iter, n.thin = 1)
stopCluster(cl)
end.time <- Sys.time()
run.time <- end.time - start.time
run.time
MCMCsummary(out.n.ignoring.mdm)
unlist(true_vals)

### model including distance with n = 100 out of N = 1000, sample based on continuous distance

y <- y.sample[y.sample$Q == 1, ]
# make sequential site index
y$site.index <- as.numeric(as.factor(as.character(y$site)))

jags_data <- list(y = y$response, x = as.vector(scale(y$x)), site.index = y$site.index, d = as.vector(scale(y$distance)), mean.x = mean(y$x), sd.x = sd(y$x), mean.d = mean(y$distance), sd.d = sd(y$distance))


n.adapt <- 500
n.update <- 1000
n.iter <- 5000

jags_model_file <- "example-2/sim/complete_data_model.jags"
# variables <- c("beta1", "beta2", "mu.beta0.unscaled", "beta1.unscaled", "beta2.unscaled")
cl <- makeCluster(length(inits))
registerDoParallel(cl)
start.time <- Sys.time()
out.n.cont.dist <- jags.parfit(cl, model = jags_model_file, params = variables, data = jags_data, inits = inits, n.chains = length(inits), n.update = n.update, n.iter = n.iter, n.thin = 1)
stopCluster(cl)
end.time <- Sys.time()
run.time <- end.time - start.time
run.time
MCMCsummary(out.n.cont.dist)

### model including distance with n = 100 out of N = 1000, sample based on categorical distance

y <- y.sample[y.sample$Q_cat == 1, ]
# make sequential site index
y$site.index <- as.numeric(as.factor(as.character(y$site)))
sort(unique(y$site.index))

jags_data <- list(y = y$response, x = as.vector(scale(y$x)), site.index = y$site.index, d = as.vector(scale(y$distance)), mean.x = mean(y$x), sd.x = sd(y$x), mean.d = mean(y$distance), sd.d = sd(y$distance))


n.adapt <- 500
n.update <- 1000
n.iter <- 5000

jags_model_file <- "example-2/sim/complete_data_model.jags"
# variables <- c("beta1", "beta2", "mu.beta0.unscaled", "beta1.unscaled", "beta2.unscaled")
cl <- makeCluster(length(inits))
registerDoParallel(cl)
start.time <- Sys.time()
out.n.cat.dist <- jags.parfit(cl, model = jags_model_file, params = variables, data = jags_data, inits = inits, n.chains = length(inits), n.update = n.update, n.iter = n.iter, n.thin = 1)
stopCluster(cl)
end.time <- Sys.time()
run.time <- end.time - start.time
run.time
MCMCsummary(out.N.complete)
MCMCsummary(out.n.ignoring.mdm)
MCMCsummary(out.n.cont.dist)
MCMCsummary(out.n.cat.dist)
unlist(true_vals)


### model including the actual inclusion probability (not the underlying distance metric)
### for n = 100 out of N = 1000, sample based on categorical distance

y <- y.sample[y.sample$Q_cat == 1, ]
# make sequential site index
y$site.index <- as.numeric(as.factor(as.character(y$site)))
sort(unique(y$site.index))

jags_data <- list(
  y = y$response,
  x = as.vector(scale(y$x)),
  site.index = y$site.index,
  d = as.vector(scale(y$norm_cat_p)), # as.vector(scale(y$distance)),
  mean.x = mean(y$x),
  sd.x = sd(y$x),
  mean.d = mean(y$norm_cat_p),
  sd.d = sd(y$norm_cat_p)
)


n.adapt <- 500
n.update <- 1000
n.iter <- 5000

jags_model_file <- "example-2/sim/complete_data_model.jags"
# variables <- c("beta1", "beta2", "mu.beta0.unscaled", "beta1.unscaled", "beta2.unscaled")
cl <- makeCluster(length(inits))
registerDoParallel(cl)
start.time <- Sys.time()
out.n.cat.incl.prob <- jags.parfit(cl, model = jags_model_file, params = variables, data = jags_data, inits = inits, n.chains = length(inits), n.update = n.update, n.iter = n.iter, n.thin = 1)
stopCluster(cl)
end.time <- Sys.time()
run.time <- end.time - start.time
run.time

MCMCsummary(out.n.cat.incl.prob)
unlist(true_vals)
