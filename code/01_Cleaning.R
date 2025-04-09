library(tidyverse)
library(stringr)
library(readxl)
library(digest)

# data <- read.csv("metada_new.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
data <- read_xlsx("metada_work_version.xlsx")

# adding hashes to NA cells based on url_clean (non-dowloaded files don't have url's)
hash_lookup <- data %>%
  filter(is.na(hash)) %>%
  distinct(url_clean, Year, group) %>%
  rowwise()%>%
  mutate(new_hash = paste0("new_", digest(paste(url_clean, Year, group, sep = "_"), algo = "md5")))%>%
  ungroup()

data <- data %>%
  left_join(hash_lookup, by = c("url_clean", "Year", "group")) %>%
  mutate(hash = if_else(is.na(hash), new_hash, hash)) %>%
  select(-new_hash)
# hashes are now suitable for  counting sources

data_clean <- data %>%
  select(continent, country, GADM_level_1, GADM_level_2, region_custom, 
         region_detail, ISO_2, kingdom, phylum, class, order, group, 
         url_clean, name_orig, language, Year, hash) %>% # changed link_ref to url_clean, added hash
  mutate(across(everything(), str_trim)) %>% 
  mutate(across(everything(), ~na_if(.,""))) %>%
  mutate(
    across(c(continent, country, 
             kingdom, phylum, class, order, group), str_to_title),
    source_link = url_clean,
    source_name = name_orig,
    year = Year,
    file_hash = hash,
    continent = recode(continent,
      "North_america" = "North America",
      "South_america" = "South America")
  ) %>%
  select(-c(url_clean, name_orig, Year, hash))%>%#moved duplicated columns
  relocate((file_hash), .after = last_col()) # moved hash to the end 


write.csv(data_clean, file = "metadata_clean.csv", row.names = FALSE)
save(data_clean, file = "metadata_clean.RData")
