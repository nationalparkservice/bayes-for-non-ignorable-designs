library(tidyverse)
library(spsurvey)
source("utils.R")

# Set example / project paths and source required utilities.
ex_path <- get_ex_path()
output_path <- get_output_path(ex_path)

# Read in data containing original weights, adjusted weights, and mean response
# for each event (visit).
input_data <- read.csv(file.path(
  ex_path,
  "spsurvey-input-data.csv"
))

# Run function for all years.
cont_list <- lapply(unique(input_data$Year), function(this_year) {
  tmp <- input_data %>% filter(Year %in% this_year)

  the.sites <- data.frame(
    siteID = tmp$SiteName,
    tmp$EvalStatus == "Target-Surveyed"
  )
  # the.subpop <- data.frame(siteID = tmp$SiteName,
  #                         Popn1 = rep('AllSites', nrow(tmp)))
  the.subpop <- data.frame(
    siteID = tmp$SiteName,
    Popn1 = rep("AllSites", nrow(tmp)),
    Stratum = tmp$mdcaty
  )
  the.design <- data.frame(siteID = tmp$SiteName, wgt = tmp$adjwt)
  the.data.cat <- data.frame(
    siteID = tmp$SiteName,
    Stability = tmp$MedianStabilityRdUp
  )
  framesize <- c("V101" = 89930000, "V102" = 64640000, "V103" = 56390000, "V203" = 23880000) # frame area in square meters from resampled 100m cells
  popsize <- list(Popn1 = sum(framesize), Stratum = as.list(framesize))
  # popsize_whole <- list("Popn1"=c(234840000))  # sum of all subpopulations sampled (sum of strata in m2) - if using intial 20m cells, area = 241264800; if using 100m resampled cells area = 234840000
  tmp2 <- cat.analysis(
    sites = the.sites,
    subpop = the.subpop,
    design = the.design,
    data.cat = the.data.cat,
    popsize = popsize,
    vartype = "SRS",
    conf = 95
  )

  # write.table(tmp2, file.path(output_path,
  #                            paste0(this_year,'_ORPI_Stability.csv')),
  #            sep = ",", col.names = NA)

  tmp2 %>% mutate(Year = this_year)
})

# Combine the relevant attributes for plotting.
d_cont <- bind_rows(cont_list) %>%
  filter(Category != "Total") %>% # %in% c('Mean', '50Pct')) %>%  # c('5Pct', 'Mean', '95Pct')
  complete(Year, nesting(Type, Subpopulation, Indicator)) %>%
  complete(Category, nesting(Type, Subpopulation, Year, Indicator)) %>%
  filter(!is.na(Category)) %>%
  arrange(Year, Type, Subpopulation, Category) %>%
  select(Type, Subpopulation, Indicator, Year, Category, NResp, Estimate.P, StdError.P, LCB95Pct.P, UCB95Pct.P, Estimate.U, StdError.U, LCB95Pct.U, UCB95Pct.U)

save_table(
  d_cont, file.path(output_path, "output"),
  "ORPI_StatusEstimates_AllYears_MedianStabilityRdUp.csv"
)
# write.csv(d_cont, file.path(output_path,
#                             'ORPI_StatusEstimates_AllYears_MedianStabilityRdUp.csv'))
