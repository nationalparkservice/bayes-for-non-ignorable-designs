[![Analysis CI](https://github.com/nationalparkservice/bayes-for-non-ignorable-designs/actions/workflows/main.yml/badge.svg)](https://github.com/nationalparkservice/bayes-for-non-ignorable-designs/actions/workflows/main.yml)
[![Docker](https://github.com/nationalparkservice/bayes-for-non-ignorable-designs/actions/workflows/docker.yml/badge.svg?branch=main)](https://github.com/nationalparkservice/bayes-for-non-ignorable-designs/actions/workflows/docker.yml)

# bayes-for-non-ignorable-designs

## Getting started
First we need to clone the repo locally and obtain the various "protected" data assets from the [NPS Data Store](https://doi.org/10.36967/code-2287025).
```sh
git clone https://github.com/nationalparkservice/bayes-for-non-ignorable-designs.git
cd bayes-for-non-ignorable-designs
./get-inputs.sh
```

If you'd like to use Docker to avoid issues with dependencies, you can build and launch an RStudio Server instance using:
```sh
./docker/run.sh
```
The above command assumes you have already cloned the repo and have `cd`'ed into the repo directory. Once the container is running, simply open your browser to http://localhost:8787/ using "bayes" as username and password.

## Requirements (if not using the Docker image)
R and JAGS. Specifically, we make use of the following R packages: abind, rjags,
coda, HDInterval, spsurvey, tidyverse, hrbrthemes, ggridges, and cowplot.

## context

Study area, missingness in space and time.

| Study area  | Spatial  | Temporal  |
|---|---|---|
| ![study area](assets/network-overview.jpg)  | ![spat miss](assets/site-layout.jpg)  | ![time miss](assets/ex-visit-schedule.jpg)  |

## Examples
1. [Changes in observers and missing covariates](example-1/)

| Figure 3  | Figure 4  |
|---|---|
| ![ex1 fig1](assets/example-1/example-1-fig1.jpg)  | ![ex1 fig2](assets/example-1/example-1-fig2.jpg)  |

2. [Unequal probabilities of inclusion](example-2/)

| Figure 5a  | Figure 5b  | Figure 5c  |
|---|---|---|
| ![ex2 fig-a](assets/example-2/example-2-fig-a.jpg)  | ![ex2 fig-b](assets/example-2/example-2-fig-b.jpg)  | ![ex2 fig-c](assets/example-2/example-2-fig-c.jpg)  |

3. [Censored and truncated data](example-3/)

| Figure 6a | Figure 6b |
|---|---|
| ![ex3 fig-a](assets/example-3/example-3-fig-a.jpg)  | ![ex3 fig-b](assets/example-3/example-3-fig-b.jpg)  |

## Disclaimers
1. The JAGS models utilize a notation that differs subtly from the model math as it appears in the manuscript. For instance, the intercept and trend slope coefficients &ndash; the $`\bm{\alpha}_k`$ terms in the manuscript &ndash; appear as `B[j, 1, k]` and `B[j, 2, k]`, respectively. Likewise, we use `x` and `X` in place of $`t`$ and $`\bm{\mathrm{W}}`$.
2. Though we discuss variography in the manuscript, we do not include information on the spatial location of individual sites here in order to protect the confidentiality of the data.
3. For purposes of clarity, and to meet page constraints, we omitted some details about the censored and truncated canopy gap size data. Specifics are included in a README for the censored and truncated data example.
4. The RNG and other hidden state in your programming environment might cause results to differ subtly from those appearing in the manuscript -- the qualitative inference and conclusions drawn should not.

## Contributors
Authors: Luke Zachmann*, Tom Hobbs, Erin Borgman, Dana Witwicki, Megan Swan, Cheryl McIntyre, and Carolyn Livensperger  

\* maintainer (lzachmann@gmail.com)

## Citation
TBD

## Developer notes

### Short URL
```
curl -i https://git.io -F "url=https://github.com/nationalparkservice/bayes-for-non-ignorable-designs" -F "code=non-ignorable"
curl -i https://git.io/non-ignorable
```

The assets directory is tracked using Git LFS, which we set up using `git lfs track "assets/**"` (quotes required to avoid shell expansion).
