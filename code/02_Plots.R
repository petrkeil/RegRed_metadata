library(tidyverse)
library(gridExtra)
library(scales)

load("metadata_clean.RData")
data <- data_clean %>% 
  filter(continent %in% c("Antarctica", "Europe", "Africa",
                          "South America", "Asia", "North America") &
           !kingdom == "Protozoa")


# Sources plots -----------------------------------------------------------


## Number of unique sources by continent and age
un_sources_years <- data[,c("continent", "source_name", "year")]  %>%
  distinct(continent, source_name, year, .keep_all = TRUE)

un_hashes_years <- data[,c("continent", "file_hash","source_name" ,"year")]  %>%
  distinct(continent, file_hash, year,source_name, .keep_all = TRUE)

# Get current year
current_year <- year(Sys.Date())

# Create age categories
unique_sources <- un_sources_years %>%
  mutate(age_category = case_when(
    year >= (current_year - 5) ~ "Less than 5 years old",
    year >= (current_year - 20) ~ "5-20 years old",
    TRUE ~ "Older than 20 years"
  ))

# Convert to factor with specific order
unique_sources$age_category <- factor(unique_sources$age_category, 
                                      levels = c("Less than 5 years old", 
                                                 "5-20 years old", 
                                                 "Older than 20 years"))

# Count sources by continent and age category
continent_age_counts <- unique_sources %>%
  group_by(continent, age_category) %>%
  summarize(count = n()) %>%
  ungroup() %>% 
  mutate(count_log = log(count))

# plot
p2 <- ggplot(continent_age_counts, aes(x = reorder(continent, -count), y = count, fill = age_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Number of Unique Sources by Continent and Age",
    x = "Continent",
    y = "Number of Unique Sources",
    fill = "Source Age"
  ) +
 # scale_y_log10(labels=label_number()) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Less than 5 years old" = "#4CAF50",  
    "5-20 years old" = "#2196F3",         
    "Older than 20 years" = "#FFC107"     
  )) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

p2


# Records plots -----------------------------------------------------------

## Number of Records by Continent and Kingdom
# Count records by continent and kingdom
continent_kingdom_counts <- data %>%
  group_by(continent, kingdom) %>%
  summarize(count = n()) %>%
  ungroup() %>% 
  mutate(count_log = log(count)) %>% 
  arrange(count)

p3 <- ggplot(continent_kingdom_counts %>% 
               arrange(count), 
             aes(x = reorder(continent, -count), y = count, fill = kingdom)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(transform = "log10", labels=transform("log10")) +
  labs(
    title = "Number of Records by Continent and Kingdom",
    x = "Continent",
    y = "Number of Records",
    fill = "Kingdom"
  ) +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1")


p3


grid.arrange(p3, p2)
