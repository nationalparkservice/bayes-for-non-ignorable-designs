CARE_cgaps_raw <- readRDS("data/NCPN/CARE_cgaps_raw.rds") %>%
  filter(Master_Stratification == "Hartnet-deep grassland")

write_csv(CARE_cgaps_raw, "data/NCPN/CARE_cgaps_hdeep.csv")


CARE_cgaps_raw_uncut <- readRDS("data/NCPN/CARE_cgaps_raw_uncut.rds") %>%
  filter(Master_Stratification == "Hartnet-deep grassland")

write_csv(CARE_cgaps_raw_uncut, "data/NCPN/CARE_cgaps_hdeep_uncut.csv")
