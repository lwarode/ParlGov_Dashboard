
if (! "pg_party_color.csv" %in% list.files(here::here("App"))) {
  
  # Packages ----------------------------------------------------------------
  library(tidyverse)
  library(partycoloR) # devtools::install_github("lwarode/partycoloR")
  
  
  # Party Facts ParlGov -----------------------------------------------------
  pf_url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  
  pf_raw <- read_csv(pf_url, guess_max = 30000)
  
  pf_pg <- pf_raw %>% 
    filter(dataset_key == "parlgov")
  
  
  # Wikipedia Party URLs ----------------------------------------------------
  wiki_url <- "https://raw.githubusercontent.com/hdigital/partyfactsdata/master/import/wikipedia/wikipedia.csv"
  
  wiki_raw <- read_csv(wiki_url)
  
  wiki_pg <- wiki_raw %>% 
    select(partyfacts_id, url) %>% 
    right_join(pf_pg, by = "partyfacts_id")
  
  wiki_pg_list <-  wiki_pg %>%
    filter(! is.na(url)) %>%
    pull(url)
  

  # partycoloR --------------------------------------------------------------
  pg_party_color_raw <- wikipedia_party_color(wiki_pg_list)
  
  pg_party_color <- pg_party_color_raw %>% 
    right_join(wiki_pg, by = "url")
  
  write_csv(pg_party_color, "pg_party_color.csv")
  
}

library(tidyverse)
pg_party_color_raw <- read.csv(here::here("App/pg_party_color.csv"))

pg_party_color <- pg_party_color_raw %>% 
  rename(party_id = dataset_party_id) %>% 
  # mutate(color = case_when(
  #   pg_party_id %in% c(808, 501, 791, …) ~ color_2,
  #   TRUE ~ color_1
  # ))
  mutate(color = case_when(
    # CDU
    party_id == 808 ~ color_2,
    # N-VA
    party_id == 501 ~ color_2,
    # DIE LINKE
    party_id == 791 ~ color_2,
    TRUE ~ color_1
  ))


