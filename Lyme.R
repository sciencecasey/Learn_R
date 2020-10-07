#from Riffomonas website generalR
library(tidyverse)
library(lubridate)

annual_counts <- read_csv("https://raw.githubusercontent.com/sciencecasey/generalR_data/master/project_tycho/US.23502006.csv",
                          col_type=cols(PartOfCumulativeCountSeries = col_logical())) %>%
  filter(PartOfCumulativeCountSeries) %>%
  mutate(year = year(PeriodStartDate+7)) %>%
  group_by(year) %>%
  summarize(count = max(CountValue))

ggplot(annual_counts, aes(x=year, y=count)) +
  geom_line() +
  scale_y_continuous(limits=c(0,NA)) +
  scale_x_continuous(breaks=c(1990, 1995, 2000, 2005, 2010, 2015)) +
  labs(x="Year",
       y="Number of cases",
       title="The number of Lyme disease cases has been rising since 1990") +
  theme_linedraw()

ggsave("lyme_disease_annual_counts.pdf", width=6, height=4)
