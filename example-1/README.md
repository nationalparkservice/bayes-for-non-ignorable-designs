## example-1: changes in observers and missing covariates

The [complete](complete/) model for native species richness includes two
covariates (see `jags_data$X`): an indicator for observer (the botanist taking
data, `fe_BotanistDS`) and the cover of non-native vegetation
(`fe_NumberNonBromeExoticHits2`). The cover of non-native species was modeled
using the same indicator for observer as well as total antecedent rainfall
(`fe_rain.pregr`) as fixed effects (`jags_data$W`). Rainfall measures
(accumulated precipitation in the 16-month period preceding the growing season)
were derived using a gridded meteorology product.

We compare the complete Bayesian model to the Horvitz–Thompson (HT) estimator
([ht](ht/)) and to a simpler Bayesian model ignoring the nuisance observer
variable ([null](null/)). Both Bayesian models include trend terms.

The goals for this analysis are to:
* demonstrate the _inability_ of HT to provide inference on trend or the
influence of covariates, some of which are non-ignorable;
* illustrate the use of covariates with missing values (by imputing values using
a model of the incomplete covariate data); and
* describe the importance of controlling for observer effects (the "null" vs.
the "complete" model).

The plotting script ("plotting.R") creates the figure presented in the
manuscript after results from each of the models have been compiled.
