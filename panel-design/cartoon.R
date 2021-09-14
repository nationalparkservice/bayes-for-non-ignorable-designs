library(tidyverse)
library(hrbrthemes)
source("utils.R")
dir.create("assets", showWarnings = FALSE)


load_font()

d <- read.csv("panel-design/cartoon-data.csv") %>%
  mutate(
    SiteName = factor(SiteName,
      levels = unique(SiteName),
      labels = unique(str_extract(SiteName, "(?<=Site)\\d+"))
    ),
    Stratum = str_extract(Strata, "(?<=Stratum).*")
  )

ggplot(d) +
  geom_vline(
    xintercept = seq(2009, 2016, 1), linetype = "longdash", alpha = 0.1
  ) +
  geom_point(
    aes(
      x = Year, y = SiteName,
      group = Strata, color = Stratum, fill = Stratum
    ),
    pch = 21, stroke = 1.5, size = 6, color = "black"
  ) +
  theme_ipsum_rc(
    grid = "Y", base_family = "LM Roman 10", base_size = 16,
    axis_title_size = 18, strip_text_size = 18, axis_title_just = "c"
  ) +
  theme(
    legend.position = "bottom", legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-15, -15, -15, -15)
  ) +
  scale_fill_manual("Stratum", values = c("#d9d9d9", "#595959")) +
  scale_x_continuous(breaks = seq(2009, 2016, 2)) +
  labs(x = "", y = "Site")
ggsave("assets/ex-visit-schedule.jpg", width = 5, height = 6)
