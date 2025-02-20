
# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ggrepel)

# Read the data
top_ten <- read_csv("Resources/top_ten_jobs.csv")
bottom_ten <- read_csv("Resources/bottom_ten_jobs.csv")

# Add a category to distinguish top and bottom ten
top_ten <- top_ten %>% mutate(Category = "Ten most remote-workable")
bottom_ten <- bottom_ten %>% mutate(Category = "Ten least remote-workable")

# Combine datasets
jobs_data <- bind_rows(top_ten, bottom_ten)
print(jobs_data)
ggplot(jobs_data, 
       aes(x=`BA share`, y=`Weighted by wage`, 
            color = Category, label = `Metropolitan area`)) +
  geom_point(aes(size=`Median income`), alpha = 0.7) +
  geom_text_repel(
    # Repel away from the left edge, not from the right.
    xlim = c(NA, NA),
    # Do not repel from top or bottom edges.
    ylim = c(-Inf, Inf),
    # nudge_x = .15
    # nudge_y = 0.05
    # box.padding = 0.5,
    # nudge_y = .15
  ) +
  # xlim(c(35,120)) + 
  ylim(0.25, 0.75) + 
  # geom_text(hjust = 0, vjust = 1, size = 5) +
  ylab("Fraction of jobs workable from home (weighted by wage)") +
  theme_classic(base_size = 16)

  

# Prepare data for faceting by BA share and White share
# jobs_data_long <- jobs_data %>%
#   pivxot_longer(cols = c(`BA share`, `White share`), names_to = "variable", values_to = "x_value")
# 
# # Create the faceted plot.
# ggplot(jobs_data_long, aes(x = x_value, y = `Weighted by wage`, size = `Median income`, color = Category, label = `Metropolitan area`)) +
#   geom_point(alpha = 0.7) +
#   geom_text(hjust = 0, vjust = 1, size = 3) +
#   facet_wrap(~variable, scales = "free_x", labeller = labeller(variable = c(`BA share` = "BA Share", `White share` = "White Share"))) +
#   theme_minimal() +
#   labs(x = NULL, y = "Share of Jobs Weighted by Wage", color = "Category", size = "Median Income") +
#   theme(strip.text = element_text(size = 12, face = "bold"))
