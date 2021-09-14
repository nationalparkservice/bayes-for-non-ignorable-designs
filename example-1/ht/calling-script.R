library(tidyverse)
library(spsurvey)
source("utils.R")

# Set example / project paths and source required utilities.
ex_path <- get_ex_path()
output_path <- get_output_path(ex_path)

# Read in data containing original weights, adjusted weights, panel weights, and mean response for each event (visit).
input_data <- read.csv(file.path(
  ex_path, # was input_data.orig
  "spsurvey-input-data.csv"
))

### Run function for all years.
pct_list <- lapply(unique(input_data$Year), function(this_year) {
  tmp <- input_data %>% filter(Year %in% this_year)

  the.sites <- data.frame(
    siteID = tmp$SiteName,
    tmp$EvalStatus == "Target-Surveyed"
  )
  the.subpop <- data.frame(
    siteID = tmp$SiteName,
    Popn1 = rep("AllSites", nrow(tmp))
  )
  the.design <- data.frame(siteID = tmp$SiteName, wgt = tmp$PanelWgt * tmp$adjwt) # total wgt is design wgt * panel wgt
  the.data.cont <- data.frame(
    siteID = tmp$SiteName,
    native.rich = tmp$mean.native.rich
  )

  popsize_whole <- list("Popn1" = c(1886532.367184)) # sum of all subpopulations sampled
  tmp2 <- cont.analysis(
    sites = the.sites,
    subpop = the.subpop,
    design = the.design,
    data.cont = the.data.cont,
    popsize = popsize_whole,
    vartype = "SRS", # omitted xycoord for publication; otherwise can use neighborhood variance
    conf = 95,
    total = TRUE
  )

  # write.table(tmp2$Pct, file.path(ex_path, 'output',
  #                                 paste0(this_year,'_LIBI_Pct.csv')),
  #             sep = ",", col.names = NA)

  tmp2$Pct %>% mutate(Year = this_year)
})

# Combine the relevant attributes for plotting.
d_pct <- bind_rows(pct_list) %>%
  filter(Statistic %in% "Mean") %>% # c('5Pct', 'Mean', '95Pct')
  arrange(Year)
save_table(
  d_pct, file.path(output_path, "output"),
  "LIBI_StatusEstimates_AllYears_NativeRichnessOnly.csv"
)
# write.csv(d_pct, file.path(output_path,
#                           'LIBI_StatusEstimates_AllYears_NativeRichnessOnly.csv'))
