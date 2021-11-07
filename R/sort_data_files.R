library(readr)
library(dplyr)

# sometimes humans make mistakes
system("cp -r data bkup_data")

read_tsv("data/maps.tsv",
         col_types = "ciccc") %>% 
  arrange(mapid) %>% 
  write_tsv("data/maps.tsv")

read_tsv("data/packages.tsv",
         col_types = "cc") %>% 
  arrange(mapid) %>% 
  write_tsv("data/packages.tsv")

read_tsv("data/descriptions.tsv",
         col_types = "ccc") %>% 
  arrange(mapid) %>% 
  write_tsv("data/descriptions.tsv")

read_tsv("data/links.tsv",
         col_types = "cccc") %>% 
  arrange(mapid) %>% 
  write_tsv("data/links.tsv")

read_tsv("data/images.tsv",
         col_types = "ccc") %>% 
  arrange(mapid) %>% 
  write_tsv("data/images.tsv")

