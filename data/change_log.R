# Change log
# Date: 2025-09-15
# Author: Florencia Grattarola

library(tidyverse)

###################################################
# CHANGES TO metadata_definitions.csv

metadata_definitions <- read_csv('data/metadata_column_definitions.csv') 

# 1) The following field names were adapted to match our updated database structure: level_0 = country, level_1 = stateProvince, level_2 = county, region_custom = customRegion, region_detail = regionDetail, iso_2 = countryCode, source_name = sourceTitle, source_link = sourceIdentifier, language = sourceLanguage, year = sourceDate.
# 2) The definition for 'level_2' = 'county' should refer to the "second" smaller administrative region than the country. 
# 3) The definition for 'taxa' should point to the record and not the source.
# 4) The definition for 'source_link' = 'sourceIdentifier' should point to the source and not the record.
# 5) The row for 'region_detail' was deleted, as the information about the countries spanning the region are available in the country column.

metadata_definitions %>% 
  rename(term = `Column Name`, definition = Definition) %>% 
  mutate(term = case_when(term == 'level_0' ~ 'country',
                          term == 'level_1' ~ 'stateProvince', 
                          term == 'level_2' ~ 'county', 
                          term == 'region_custom' ~ 'customRegion', 
                          term == 'iso_2' ~ 'countryCode', 
                          term == 'source_name' ~ 'sourceTitle', 
                          term == 'source_link' ~ 'sourceIdentifier', 
                          term == 'language' ~ 'sourceLanguage', 
                          term == 'year' ~ 'sourceDate',
                          TRUE ~ term)) %>% 
  mutate(definition = ifelse(term == 'county', str_replace(definition, 'first', 'second'), definition)) %>%
  mutate(definition = ifelse(term == 'taxa', str_replace(definition, 'source', 'record'), definition)) %>%
  mutate(definition = ifelse(term == 'sourceIdentifier', str_replace(definition, 'record', 'source'), definition)) %>% 
  filter(term != 'region_detail') -> metadata_definitions

write_csv(metadata_definitions, 'data/metadata_column_definitions.csv', na = '')


###################################################
# CHANGES TO metadata.csv

# 1) The following field names were adapted to match our updated database structure: gadm_level_0 = country, gadm_level_1 = stateProvince, gadm_level_2 = county, region_custom = customRegion, region_detail = regionDetail, iso_2 = countryCode, source_name = sourceTitle, source_link = sourceIdentifier, language = sourceLanguage, year = sourceDate.
# 2) The correct sourceIdentifier for the ids == 1912 1913 1914 1915 1916 1917 is https://www.raudonojiknyga.lt
# 3) The correct sourceIdentifier for the id == 2025 is https://www.yumpu.com/es/document/read/12111973/lista-oficial-de-la-republica-de-panama-iucn
# 4) The correct sourceIdentifier for the id == 10 is https://www.govern.ad/ca/tematiques/medi-ambient-i-sostenibilitat/medi-natural-i-biodiversitat/fauna/fauna-autoctona-i-especies-protegides
# 5) The correct sourceLanguage is 'German' and not 'Deutsch'
# 6) The following 8 ids of customRegion == 'Carpathians' were deleted as they were duplicated, 662 663 664 665 666 667 668 669.
# 7) The information in the column region_detail was moved to either the country or the customRegion columns. The values corresponding to regions ('Adelaide & Mt Lofty Ranges', 'Cordillera de Guaniguanico', 'Kangaroo Island', 'Murraylands', 'Northern & Yorke', 'SA Arid Lands (Outback)', 'Simiri', 'South East', and 'West') were moved to the column customRegion. The rest of the values, corresponding to country names, were moved to the country column.
# 8) The column region_detail was deleted, as the information about the countries spanning the region are available in the country column.
# 9) The column iso_3 was deleted, as countryCode (iso_2) is sufficient.
# 10) The redlists for Namibia, id = 2070, and 2071:2074 had NAs as iso_2, but this should be 'NA'


metadata <- read_csv('data/metadata.csv', guess_max = 4000, na = '') 

metadata %>% 
  rename(country = gadm_level_0, 
         stateProvince = gadm_level_1, 
         county = gadm_level_2, 
         customRegion = region_custom, 
         countryCode = iso_2, 
         sourceTitle = source_name, 
         sourceIdentifier = source_link, 
         sourceLanguage = language, 
         sourceDate = year) %>% 
  mutate(sourceIdentifier = ifelse(id %in% c(1912:1917), 'https://www.raudonojiknyga.lt', sourceIdentifier)) %>% 
  mutate(sourceIdentifier = ifelse(id == 2025, 'https://www.yumpu.com/es/document/read/12111973/lista-oficial-de-la-republica-de-panama-iucn', sourceIdentifier)) %>% 
  mutate(sourceIdentifier = ifelse(id == 10, 'https://www.govern.ad/ca/tematiques/medi-ambient-i-sostenibilitat/medi-natural-i-biodiversitat/fauna/fauna-autoctona-i-especies-protegides', sourceIdentifier)) %>% 
  mutate(sourceLanguage = ifelse(sourceLanguage == 'Deutsch', 'German', sourceLanguage)) %>% 
  filter(!(customRegion == 'Carpathians' & is.na(country))) %>% 
  mutate(customRegion = ifelse(region_detail %in% c('Adelaide & Mt Lofty Ranges', 
                                                    'Cordillera de Guaniguanico', 
                                                    'Kangaroo Island', 'Murraylands', 
                                                    'Northern & Yorke', 
                                                    'SA Arid Lands (Outback)', 
                                                    'Simiri', 
                                                    'South East', 
                                                    'West'), 
                               region_detail, customRegion)) %>% 
  mutate(country = ifelse(!is.na(region_detail) & 
                            (!region_detail %in% c('Adelaide & Mt Lofty Ranges', 
                                                  'Cordillera de Guaniguanico', 
                                                  'Kangaroo Island', 'Murraylands', 
                                                  'Northern & Yorke', 
                                                  'SA Arid Lands (Outback)', 
                                                  'Simiri', 
                                                  'South East', 
                                                  'West')),
                          region_detail, country)) %>% 
  select(-region_detail) %>%
  select(-iso_3) -> metadata

write_csv(metadata, 'data/metadata.csv', na ='')
