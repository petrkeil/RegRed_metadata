library(tidyverse)
library(gridExtra)
library(scales)
library(cowplot)

load("metadata_clean.RData")
data <- data_clean %>% 
  filter(continent %in% c( "Antarctica","Europe", "Africa",
                          "South America", "Asia", "North America", "Oceania") &
           !kingdom == "Protozoa")



# Some figures
# count of unique countries 
data %>%
  select(country) %>%
  filter(!is.na(country)) %>%
  distinct()%>%
  count()

data %>%
  select(group) %>%
  filter(!is.na(group)) %>%
  distinct()%>%
  count()


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
    title = "A",
    x = "",
    y = "Number of Unique Sources",
    fill = "Source Age"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 1500, by = 250), expand=c(0.01,0)) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Less than 5 years old" = "#4CAF50",  
    "5-20 years old" = "#2196F3",         
    "Older than 20 years" = "#FFC107"     
  )) +
  theme(text=element_text(size=20),
        legend.position = c(0.95, 0.95), 
        legend.justification = c(1, 1),  
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),  
        legend.box.background = element_blank(),
        axis.text.y= element_text(color="black"),
        axis.text.x= element_text(color="black")
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
  scale_y_continuous(breaks = seq(0, 3500, by = 250), expand=c(0.01,0)) +
  labs(
    title = "B",
    x = "",
    y = "Number of Records",
    fill = "Kingdom"
  ) +
  theme_minimal() + 
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  theme(text=element_text(size=20),
    legend.position = c(0.95, 0.95), 
    legend.justification = c(1, 1),  
    legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),  
    legend.box.background = element_blank(),
    axis.text.y= element_text(color="black"),
    axis.text.x= element_text(color="black")
    )


p3

# Count records by year
year_counts<-un_sources_years%>%
  group_by(year) %>%
  summarize(count = n()) 

year_counts$year <- as.Date(paste0(year_counts$year, "-01-01"))

# p4 histogram of year counts
p4 <- ggplot(year_counts, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "#4CAF50") +
  labs(
    title = "C",
    x = "Year",
    y = "Number of Records"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "10 year") +
  theme(text=element_text(size=20),
        axis.text.y= element_text(color="black", size=15),
        axis.text.x= element_text(color="black")
  )

p4



pp <- list(p2, p3, p4)
plot_grid(plotlist=pp, ncol=1, align='v')

p1





