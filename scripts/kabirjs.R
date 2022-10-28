library(tidyverse)
library(tidymodels)

# Question 1 Exploratory Data Analysis

biomarkerRaw <- read_csv('data/biomarker-raw.csv', show_col_types = FALSE)

# Data manipulation
colnames(biomarkerRaw) <- biomarkerRaw[1,]
biomarkerRaw <- biomarkerRaw[-1,]
biomarkerRaw <- lapply(biomarkerRaw, as.numeric)
biomarkerRaw <- as.data.frame(biomarkerRaw)

# Arbitrary Sample of 10 Proteins
biomarkerRaw <- biomarkerRaw %>% select(15:25)

# Pivot Long for ease of use in GGPlot
biomarkerRaw <- biomarkerRaw %>% 
  pivot_longer(everything(), names_to = "protein", values_to = "value") %>%
  arrange(protein)

# Assessing the distribution
biomarkerRaw %>%
  ggplot(aes(x=value, fill = protein)) +
  geom_histogram(binwidth = 30) +
  facet_wrap(~ protein, scales = "free") +
  labs(title = "Before Log Transformation of Data")

# Informal explanation- after examining the data - the reason that the data was 
  # log transformed was in order to "normalize" the data. Right now, almost every
  # observed protein from the sample has a heavy skew to the left. Log transforming
  # these values would lessen the skew, making it easier to perform meaningful
  # statistical analysis