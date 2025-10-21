library(tidyverse)
library(haven)
library(srvyr)
library(scales)
library(dplyr)

#data import 
unzip("anes_timeseries_cdf_csv_20220916.zip", exdir = "anes_data")
list.files("anes_data")
library(readr)
anes <- read_csv("anes_data/anes_timeseries_cdf_csv_20220916.csv")
#check
glimpse(anes)

#####
data <- readRDS("Q1data.rds")



# graph: see the number of people who identify as liberal and republican + conservative and democrat changes over time 
####################
svydata <- data |>
  as_survey_design(
    weights = VCF0009z,
    nest = TRUE
  )
recoded <- svydata |>
  mutate(partyid.recode = case_when(
    VCF0302 %in% c(1:1) ~ "Republican" ,
    VCF0302 %in% c(5:5) ~ "Democrat" ,
    TRUE ~ "DK/Other/DNR"
  )
  ) |>
  mutate(age.group = case_when(
    VCF0101 %in% c(17:35) ~ "17-35",
    VCF0101 %in% c(36:59) ~ "36:59",
    VCF0101 %in% c(60:99) ~ "60+",
    TRUE ~ "DNR"
  )) |>
  mutate (
    Gender = case_when(
      VCF0104 %in% c(1:1) ~ "Male",
      VCF0104 %in% c(2:2) ~ "Female",
      TRUE ~ "DNR/Other"
    )
  ) |>
  mutate (
    hadtochoose = case_when(
      VCF0824 %in% c(1:1) ~ "Liberal",
      VCF0824 %in% c(5:5) ~ "Conservative",
      TRUE ~ "Moderate/DNR"
    )
  ) |>
  mutate(
    IncomePercentile = case_when(
      VCF0114 %in% c(1:1) ~ "0-16%",
      VCF0114 %in% c(2:2) ~ "17-33%",
      VCF0114 %in% c(3:3) ~ "34-67%",
      VCF0114 %in% c(4:4) ~ "68-95%",
      VCF0114 %in% c(5:5) ~ "96-100%",
      TRUE ~ "DK/NA/DNR"
    )
  ) |>
  filter(
    partyid.recode != "DK/Other/DNR",
    hadtochoose != "Moderate/DNR"
  ) |>
  group_by(VCF0004, partyid.recode, hadtochoose) |>
  summarize(p = survey_prop(var = 'ci'))
## Make a graph
ggplot(recoded, aes(x = VCF0004, y = p, color = hadtochoose)) +
  geom_ribbon(aes(ymin = p_low, ymax = p_upp,
                  fill = hadtochoose, color = NULL, group = hadtochoose),
              alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = .75) +
  facet_wrap(~ partyid.recode, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    x = NULL, y = "Share Within Party",
    title = "Ideology Over Time by Party",
    color = "Ideology", fill = "Ideology"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()
