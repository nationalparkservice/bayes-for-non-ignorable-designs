## example-3: censored and truncated data

We compare a Bayesian model that explicitly handles censored and truncated
canopy gap size observations ([canopy-gaps-2021](canopy-gaps-2021/)) to one that
naively ignores the incompleteness of these data
[canopy-gaps-naive-2021](canopy-gaps-naive-2021/).

As with the other examples, the proper Bayesian estimates are compared to the Horvitz–Thompson (HT) estimates
[CARE_HorvitzThomsponFiles](CARE_HorvitzThomsponFiles/).

Both Bayesian models include trend terms. All models were applied to data from
a single stratum only for two reasons. First, park-wide estimates of canopy gap
sizes were not desired; and, second, subsetting the data allows us to run and
interact with the results of the model in a reasonable amount of time (tens of
minutes, as opposed to hours of compute).

The goals for this analysis are to:
* demonstrate the _inability_ of HT to provide inference on trend; and
* show that ignoring the censoring and truncation leads to biases, and that
these biases may have non-trivial implications for management.

The plotting script ("plotting.R") creates the figure presented in the
manuscript after results from each of the models have been compiled.
