## example-2: unequal probabilities of inclusion

The proper full model for soil stability ([hrs_wyppt_mm](hrs_wyppt_mm/))
includes hiking time as a covariate (`fe_hrs`).  

We compare the full Bayesian model to the Horvitz–Thompson (HT) estimator
([output_ORPI_100epi_MedianStabilityRdUp](output_ORPI_100epi_MedianStabilityRdUp/))
and to a simpler Bayesian model ignoring hiking time ([wyppt_mm](wyppt_mm/)).
Both Bayesian models include trend terms as well as total water year
precipitation (`fe_WYppt_mm`).

Additional simulation code ("sim.R") is also included. The results of the
simulation support the idea that the hiking time design is an example of "data
missing at random" and is ignorable conditional on covariates. Computing the
probability of inclusion based on a continuous or a categorical function of
hiking time does not change the result, which almost perfectly matches the
complete data result in both cases. Assuming "missing completely at random",
ignoring hiking time fails to recover the parameters of the complete data. Be
sure to focus on the medians, not the means, when you get the final results. The
take home message is that including the continuous hiking time variable as a
covariate in the model makes the design ignorable.

The goals for this analysis are to:
* show how ordinal measures are modeled using a latent normally-distributed
continuous variable;
* demonstrate the _inability_ of "categorical HT estimates" to provide inference
on trend or other covariates;
* describe the importance of appropriately modeling the missing data mechanism
(hiking time, in this case).

The plotting script ("plotting.R") creates the figure presented in the
manuscript after results from each of the models have been compiled.
