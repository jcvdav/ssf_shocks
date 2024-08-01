######
# The bulk of this script was written by the authors of Free et al., 2023.
# I only take their processing pipeline to calculate state-level landings
# before and after the MHW regime. The original script is found here:
# https://github.com/cfree14/wc_mhw_case_studies/blob/main/code/Fig3_comm_impacts_broad_updated.R
######
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
# library(wcfish)
library(tidyverse)

# Commenting out original code by Free
# Directories
#datadir <- "data/landings"
#plotdir <- "figures"

# Read data
# data_orig <- readRDS(data, file=file.path(datadir, "WC_1980_2021_commercial_landings.Rds")) %>% # Comment out Free's
data_orig <- readRDS(url("https://github.com/cfree14/wc_mhw_case_studies/raw/main/data/landings/WC_1980_2021_commercial_landings.Rds")) %>%  # Andd read directly from repo
  filter(str_detect(comm_name, "lobster|cucumber|urchin"))

# Read AK total data
# ak_tot <- readRDS("data/landings/goa/processed/2004_2021_ak_goa_total.Rds") %>% # Original line by Free
ak_tot <- readRDS(url("https://github.com/cfree14/wc_mhw_case_studies/raw/main/data/landings/goa/processed/2004_2021_ak_goa_total.Rds")) %>% # Reading directly from the repo
  mutate(state="Alaska (below)")

# Rad and format AK by category data
# ak_catg <- readRDS("data/landings/goa/processed/2011_2019_ak_goa_by_catg.Rds") %>% # Original line by Free
ak_categ <- readRDS(url("https://github.com/cfree14/wc_mhw_case_studies/raw/main/data/landings/goa/processed/2011_2019_ak_goa_by_catg.Rds")) %>%  # Reading directly from ther repo
  rename(mgmt_group=category) %>%
  # Add state
  mutate(state="Alaska") %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize
  group_by(state, mgmt_group, period) %>%
  summarise(value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup()


# Build data
################################################################################

# Cap
pdiff_max <- 200

# Totals by state and year
data_tot <- data_orig %>%
  # Reduce
  filter(year>=2000 & year<=2020 & state!="At-Sea") %>%
  # Summarize
  group_by(state, year) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  # Remove Alaska
  filter(state!="Alaska") %>%
  # Add Alaska
  bind_rows(ak_tot) %>%
  # Format state
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington", "British Columbia", "Alaska (below)")))

# Total averages by state and period
data_tot_avg <- data_tot %>%
  # Reguce
  filter(year>=2011 & year<=2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize
  group_by(state, period) %>%
  summarize(landings_lb=mean(landings_lb, na.rm=T),
            value_usd=mean(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Mark years
  mutate(yr1=recode(period,
                    "Before"=2011, "During"=2014, "After"=2017) %>% as.numeric(),
         yr2=recode(period,
                    "Before"=2013, "During"=2016, "After"=2019) %>% as.numeric())

#########################
# My code starts here
#########################
data_tot_avg %>%
  select(state, period, landings_lb) %>%
  filter(!period == "After") %>%
  drop_na() %>%
  pivot_wider(names_from = period,
              values_from = landings_lb) %>%
  mutate(change = During - Before,
         pct = (change / Before) * 100) %>%
  knitr::kable(col.names = c("State", "Before (lb)", "After (lb)", "Difference (lb)", "% Diff"),
               label = "free_effects",
               caption = "Re-analysis of landings data from Free et al., 2023.",
               format = "latex",
               booktabs = T) %>% 
  cat(file = here::here("results", "tab", "tabS07_Free_data.tex"))

#########################