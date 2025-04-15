# Load required libraries
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)

# Load data
load("metadata_clean.RData")

# Process the multi-country codes
expanded_data <- data_clean %>%
  select(continent, source_name, ISO_2) %>%
  separate_rows(ISO_2, sep = "\\|") %>%
  mutate(ISO_2 = str_trim(ISO_2)) %>%
  distinct(source_name, ISO_2)

# Count sources per country
country_counts <- expanded_data %>%
  count(ISO_2, name = "source_count")

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
ocean <- ne_download(scale = 110, type = 'ocean', category = 'physical', returnclass = 'sf')

# Join with country counts and categorize
world_data <- world %>%
  left_join(country_counts, by = c("iso_a2_eh" = "ISO_2")) %>%
  mutate(
    source_count = replace_na(source_count, 0),
    source_category = factor(case_when(
      source_count == 0 ~ "No sources",
      source_count <= 5 ~ "1-5",
      source_count <= 15 ~ "6-15",
      TRUE ~ "16+"
    ), levels = c("No sources", "1-5", "6-15", "16+"))
  )

# Create map
p1<-ggplot() +
  geom_sf(data = world_data, aes(fill = source_category), color = "darkgray", size = 0.2, alpha = 0.8) +
  geom_sf(data = ocean, fill = "lightblue", color = NA, alpha = 0.3) +
  scale_fill_manual(
    values = c("No sources" = "gray90", 
               "1-5" = "#ffcccc", 
               "6-15" = "#ff6666", 
               "16+" = "#cc0000"),
    name = "Number of Sources"
  ) + 
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box.margin = margin(0, 0, 0, -30)  
  )
