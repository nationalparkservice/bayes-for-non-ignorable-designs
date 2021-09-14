library(tidyverse)
library(spsurvey)
source("utils.R")

# Set example / project paths and source required utilities.
ex_path <- get_ex_path()
output_path <- get_output_path(ex_path)

# Read in data containing original weights, adjusted weights, and mean response
# for each event (visit).
input_data <- read.csv(file.path(ex_path, "spsurvey-input-data.csv"))


# Run function for all years.
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
  the.design <- data.frame(siteID = tmp$SiteName, wgt = tmp$PanelWgt, wgt1 = tmp$adjwt) # design wgt stage 1, panel wgt stage 2)
  the.data.cont <- data.frame(
    siteID = tmp$SiteName,
    cgap = tmp$mean.gap
  )

  popsize_whole <- list("Popn1" = c(1105555)) # sum of all subpopulations sampled
  tmp2 <- cont.analysis(
    sites = the.sites,
    subpop = the.subpop,
    design = the.design,
    data.cont = the.data.cont,
    popsize = popsize_whole,
    vartype = "SRS",
    conf = 95,
    total = TRUE
  )

  # write.table(tmp2$Pct, file.path(ex_path, 'output',
  #                                paste0(this_year,'_CARE_Pct.csv')),
  #            sep = ",", col.names = NA)

  tmp2$Pct %>% mutate(Year = this_year)
})

# Combine the relevant attributes for plotting.
d_pct <- bind_rows(pct_list) %>%
  filter(Statistic %in% "Mean") %>% # c('5Pct', 'Mean', '95Pct')
  arrange(Year)
save_table(
  d_pct, file.path(output_path, "output"),
  "CARE_StatusEstimates_AllYears_MeanCanopyGap.csv"
)
# write.csv(d_pct, file.path(ex_path, 'output',
#                            'CARE_StatusEstimates_AllYears_MeanCanopyGap.csv'))
