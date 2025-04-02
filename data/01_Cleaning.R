library(tidyverse)
library(stringr)
library(readxl)

# data <- read.csv("metada_new.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
data <- read_xlsx("metada_work_version.xlsx")

data_clean <- data %>%
  select(continent, country, GADM_level_1, GADM_level_2, region_custom, 
         region_detail, ISO_2, kingdom, phylum, class, order, group, 
         link_ref, name_orig, language, Year) %>%
  mutate(across(everything(), str_trim)) %>% 
  mutate(across(everything(), ~na_if(.,""))) %>%
  mutate(
    across(c(continent, country, 
             kingdom, phylum, class, order, group), str_to_title),
    source_link = link_ref,
    source_name = name_orig,
    year = Year,
    continent = recode(continent,
      "North_america" = "North America",
      "South_america" = "South America")
  ) 


save(data_clean, file = "metadata_clean.RData")
