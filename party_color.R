
if (! "pg_party_color.csv" %in% list.files()) {
  
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


